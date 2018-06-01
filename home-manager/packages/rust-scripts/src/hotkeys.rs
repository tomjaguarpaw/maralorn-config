use std::rc::Rc;
use dialog::DialogProvider;
use dialog::errors::{Error, ErrorKind as EK};
use dialog::rofi::RofiDialogProvider;
use std::process::Command as StdCommand;
use std::os::unix::process::CommandExt;
use std::env::var;
use error::{Result, ResultExt};

use hotkeys::Next::*;

pub fn run<T: Into<String>>(name: T, command: &str) -> Item {
    let command = command.to_owned();
    (
        name.into(),
        Do(Rc::new(move || {
            str2cmd(&command).exec();
            Ok(Exit)
        })),
    )
}

pub fn term_cmd(command: &str) -> String {
    format!(
        "{} -e {}",
        var("TERMINAL").unwrap_or("urxvt".into()),
        command
    )
}

pub fn term<T: Into<String>>(name: T, command: &str) -> Item {
    run(name, &term_cmd(command))
}

pub fn menu<T: Into<String>>(name: T, options: Vec<Item>) -> Item {
    let name = name.into();
    (name.clone(), Menu((name.clone(), options)))
}

#[derive(Clone)]
pub enum Next {
    Menu(Dialog),
    Back,
    Exit,
    Stay,
    Do(Rc<Fn() -> Result<Next>>),
}

type Dialog = (String, Vec<Item>);

type Item = (String, Next);

type Command = Vec<String>;

fn show_menu<T: DialogProvider>(dialog_provider: &mut T, menu: Dialog) -> Result<Next> {
    let (msg, mut options) = menu;
    options.insert(0, (".Back".into(), Back));
    match dialog_provider.select_option(msg, options) {
        Ok(next) => Ok(next),
        Err(Error(EK::InvalidUserInput, _)) => Ok(Stay),
        err => err.chain_err(|| "User Input Error"),
    }
}

pub fn str2cmd(cmd: &str) -> StdCommand {
    mk_cmd(
        cmd.split_whitespace()
            .map(str::to_owned)
            .collect::<Vec<_>>(),
    )
}

pub fn mk_cmd(cmd: Command) -> StdCommand {
    let mut cmd_iter = cmd.iter();
    let mut prg = StdCommand::new(cmd_iter.next().expect("Called program without name"));
    for arg in cmd_iter {
        prg.arg(arg);
    }
    prg
}

pub fn main_loop(startmenu: Next) -> Result<()> {
    let mut dialog_provider = RofiDialogProvider;
    let mut history = vec![];
    let mut next = startmenu;
    loop {
        next = match next {
            Exit => break,
            Back => {
                history.pop().chain_err(|| "Empty history")?;
                Stay
            }
            Do(script) => script()?,
            Menu(menu) => {
                history.push(menu.clone());
                show_menu(&mut dialog_provider, menu)?
            }
            Stay => Menu(history.pop().chain_err(|| "Empty history")?.clone()),
        };
    }
    Ok(())
}
