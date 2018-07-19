use std::fs::rename;
use std::env::home_dir;
use std::path::PathBuf;
use std::collections::HashSet;

use regex::Regex;

use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::cache::TaskCache;

use maildir::{MailEntry, Maildir};

use error::{Result, ResultExt};
use hotkeys::{term_cmd, str2cmd};
use dialog::DialogProvider;
use generate::{GeneratedTask, TaskGenerator};

fn set_seen_flags(old_flags: &str) -> String {
    let mut new_flags: HashSet<_> = old_flags.chars().collect();
    new_flags.insert('S');
    let mut new_flags: Vec<_> = new_flags.into_iter().collect();
    new_flags.sort();
    new_flags.into_iter().collect()
}

fn make_maildir_path(mailbox: &str) -> PathBuf {
    let mut path = home_dir().expect("Could not get home_dir");
    path.push("mail");
    path.push(mailbox);
    path
}

fn make_mailbox_path(mailbox: &str) -> PathBuf {
    let mut path = make_maildir_path(mailbox);
    path.push("cur");
    path
}

fn make_mail_filename(id: &str, flags: &str) -> String {
    format!("{}:2,{}", id, flags)
}

fn make_mail_path(mailbox: &str, id: &str, flags: &str) -> PathBuf {
    let mut path = make_mailbox_path(mailbox);
    let filename = make_mail_filename(id, flags);
    path.push(filename);
    path
}

fn move_mail(mail: &MailEntry, oldbox: &str, newbox: &str) -> Result<()> {
    let old_flags = mail.flags();
    let old_path = make_mail_path(oldbox, mail.id(), old_flags);
    let new_flags = set_seen_flags(&old_flags);
    let new_path = make_mail_path(newbox, mail.id(), &new_flags);
    rename(old_path, new_path)?;
    Ok(())
}

lazy_static!{
    static ref SHOWN_HEADERS: Vec<String> = {
        let raw = ["from", "subject", "cc", "bcc", "date", "to" ];
        raw.iter().map(|&x| x.into()).collect()
    };
}

fn print_headers(mail: &mut MailEntry) -> Result<String> {
    let mut headers = vec![];
    for header in mail.headers()? {
        let key = header.get_key()?;
        if SHOWN_HEADERS.contains(&key.to_lowercase()) {
            headers.push((key, header.get_value()?))
        }
    }
    headers.sort_by_key(|(k, _)| {
        SHOWN_HEADERS.binary_search(&k.to_lowercase()).unwrap_or(10)
    });
    headers.reverse();
    let headers = headers
        .iter()
        .map(|(k, v)| format!("{}: {}", k, v))
        .collect::<Vec<_>>()
        .join("\n");
    Ok(headers)
}

fn get_message_id(mail: &mut MailEntry) -> Result<String> {
    for header in mail.headers()? {
        if header.get_key()?.to_lowercase().as_str() == "message-id" {
            let message_id = header.get_value()?;
            let message_id = message_id.trim().trim_matches(|x| x == '<' || x == '>');
            return Ok(message_id.into());
        }
    }
    bail!("Parsed mail without Message-ID: {},", mail.id())
}

fn get_subject(mail: &mut MailEntry) -> Result<String> {
    for header in mail.headers()? {
        if header.get_key()?.to_lowercase().as_str() == "subject" {
            return Ok(header.get_value()?);
        }
    }
    bail!("Parsed mail without Message-ID: {},", mail.id())
}

fn get_from(mail: &mut MailEntry) -> Result<String> {
    for header in mail.headers()? {
        if header.get_key()?.to_lowercase().as_str() == "from" {
            return Ok(header.get_value()?);
        }
    }
    bail!("Parsed mail without Message-ID: {},", mail.id())
}

fn read_mail(mailbox: &str, mail: &mut MailEntry) -> Result<()> {
    let mailbox = make_maildir_path(mailbox);
    let mailbox = mailbox.to_str().chain_err(|| "Invalid path")?;
    let message_id = get_message_id(mail)?;
    let message_id = message_id.replace("$", ".");
    let read_command = format!(
        "push <limit>~(~i{})<return><search>~i{}<return><display-message>",
        message_id,
        message_id
    );
    str2cmd(&term_cmd("neomutt -f"))
        .arg(mailbox)
        .arg("-e")
        .arg(read_command)
        .output()?;
    Ok(())
}

fn sort_mail(
    dialog: &mut impl DialogProvider,
    mut mail: MailEntry,
    mailbox: &SortBox,
    blacklist: &mut HashSet<String>,
) -> Result<()> {
    enum Options {
        Ignore,
        ReadNow,
        MoveTo(String),
    };
    let message_id = get_message_id(&mut mail).unwrap_or("".to_owned());
    let mut options = vec![
        ("Read now: Open in mutt now".to_owned(), Options::ReadNow),
        ("Ignore: Ignore this mail".to_owned(), Options::Ignore),
    ];
    if mail.is_seen() == false {
        options.push((
            "Mark Read: Mark as read".to_owned(),
            Options::MoveTo(mailbox.mailbox.to_string()),
        ));
    }
    let option_dirs = mailbox.option_dirs.iter().map(|(name, mailbox)| {
        ((*name).to_owned(), Options::MoveTo((*mailbox).to_owned()))
    });
    let options = options.into_iter().chain(option_dirs);
    let msg = format!("Handling E-mail:\n{}", print_headers(&mut mail)?);
    let choice = dialog.select_option(msg, options)?;
    match choice {
        Options::Ignore => {
            blacklist.insert(message_id);
        }
        Options::ReadNow => {
            read_mail(mailbox.mailbox, &mut mail)?;
            if let Some(mail) = get_maildir(mailbox.mailbox).find(mail.id()) {
                return sort_mail(dialog, mail, mailbox, blacklist);
            }
        }
        Options::MoveTo(new_mailbox) => move_mail(&mail, mailbox.mailbox, &new_mailbox)?,
    };
    Ok(())
}

fn next_mail(
    mailbox: &SortBox,
    read: bool,
    blacklist: &mut HashSet<String>,
) -> Result<Option<MailEntry>> {
    let maildir = get_maildir(mailbox.mailbox);
    for mail in maildir.list_cur() {
        let mut mail = mail?;
        let message_id = get_message_id(&mut mail).unwrap_or("".to_owned());
        if (!mail.is_seen() || read) && !blacklist.contains(&message_id) {
            return Ok(Some(mail));
        }
    }
    Ok(None)
}

pub fn sort_mailbox(
    dialog: &mut impl DialogProvider,
    mailbox: &SortBox,
    sort_read: bool,
) -> Result<()> {
    update_mailbox(mailbox.mailbox)?;
    let mut blacklist = HashSet::default();
    while let Some(mail) = next_mail(mailbox, sort_read, &mut blacklist)? {
        sort_mail(dialog, mail, mailbox, &mut blacklist)?;
    }
    Ok(())
}

fn get_maildir(mailbox: &str) -> Maildir {
    let mailbox_path = make_maildir_path(mailbox);
    Maildir::from(mailbox_path)
}

fn update_mailbox(mailbox: &str) -> Result<()> {
    let maildir = get_maildir(mailbox);
    for mail in maildir.list_new() {
        let mail = mail?;
        let id = mail.id();
        // the first branch of this if is a work around for mbsync not being standardcompliant with https://cr.yp.to/proto/maildir.html
        if id.ends_with(":2,") {
            let mut old_path = make_maildir_path(mailbox);
            old_path.push("new");
            old_path.push(id);
            let mut new_path = make_mailbox_path(mailbox);
            new_path.push(id);
            rename(old_path, new_path)?;
        } else {
            maildir.move_new_to_cur(mail.id())?;
        }
    }
    Ok(())
}

pub fn mailbox_dirty(mailbox: &str) -> Result<bool> {
    update_mailbox(mailbox)?;
    let maildir = get_maildir(mailbox);
    let dirty = maildir.count_cur() > 0;
    Ok(dirty)
}

lazy_static! {
    static ref MAIL_REGEX: Regex = Regex::new("<.*@.*>").unwrap();
}

fn create_task(mail: &mut MailEntry) -> Result<Task> {
    let subject = get_subject(mail)?;
    let from = get_from(mail)?;
    let from = MAIL_REGEX.replace(&from, "");
    let from = from.trim_matches(|x: char| x.is_whitespace() || x == '"');
    let message_id = get_message_id(mail)?;
    let mut t = TaskBuilder::default()
        .description(format!("Mail: {}: {}", from, subject))
        .build()
        .expect("TaskBuilding failed inspite of set description");
    t.set_gen_name(Some("mail-task"));
    t.set_gen_id(Some(message_id));
    Ok(t)
}

pub fn create_tasks(cache: &mut TaskCache, mailbox: &str, archiv: &str) -> Result<()> {
    update_mailbox(mailbox)?;
    for mail in get_maildir(mailbox).list_cur() {
        let mut mail = mail?;
        cache.generate(Some(create_task(&mut mail)?))?;
        cache.write()?;
        move_mail(&mail, mailbox, archiv)?;
    }
    Ok(())
}

pub struct SortBox {
    pub mailbox: &'static str,
    pub option_dirs: Vec<(&'static str, &'static str)>,
}
