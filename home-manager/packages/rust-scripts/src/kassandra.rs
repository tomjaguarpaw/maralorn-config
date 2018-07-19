use std::fs::File;
use std::env::home_dir;

use serde_yaml::{from_reader, to_writer, to_string};

use chrono::offset::Local;
use chrono::{NaiveDateTime, TimeZone, Duration};

use uuid::Uuid;

use task_hookrs::cache::TaskCache;
use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::priority::TaskPriority;
use task_hookrs::status::TaskStatus;

use dialog::rofi::RofiDialogProvider;
use dialog::DialogProvider;
use dialog::errors::ErrorKind as DEK;

use update::{update_tasks, process_task};
use error::{Result, ResultExt, ErrorKind as EK, Error};
use hotkeys::{term_cmd, str2cmd};
use tasktree::{TreeCache, TaskNode};
use well_known::{INBOX, ACCOUNTING, TREESORT, PRIVATE_MAILBOX, KIVA_MAILBOX, AK_MAILBOX,
                 SORT_INBOX, SORT_INBOX_AK, SORT_INBOX_KIVA, MAINTENANCE, CHECK_MEDIUM, CHECK_LOW,
                 CHECK_NONE, CHECK_OPTIONAL};
use mail::{sort_mailbox, SortBox};
use generate::GeneratedTask;


pub type PriorityState = (PS, bool);
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PS {
    High,
    Medium,
    Low,
    None,
    Optional,
}

fn get_sorted_uuids(cache: &TaskCache, filter: impl Fn(&Task) -> bool) -> Vec<Uuid> {
    get_sorted_tasks(cache, filter)
        .into_iter()
        .map(|x| x.uuid().clone())
        .collect()
}

fn get_sorted_tasks(cache: &TaskCache, filter: impl Fn(&Task) -> bool) -> Vec<&Task> {
    let mut tasks = cache.filter(filter).collect::<Vec<_>>();
    tasks.sort_by_key(|t| t.entry().date());
    tasks
}

pub fn get_stale_tasks(cache: &TaskCache, priority: PriorityState) -> Vec<&Task> {
    let upper_bound = priority.0.upper_bound();
    let only_optional = priority.0.only_optional();
    let prio = priority.0.prio();
    let prio = prio.as_ref();
    let mut tasks = cache
        .filter(|t| {
            t.pending() && !task_blocked(cache, t) && t.priority() == prio &&
                !t.has_tag("project") && !t.has_tag("kategorie") &&
                (only_optional == t.has_tag("optional")) &&
                (priority.1 ||
                     t.modified()
                         .map(|m| {
                        Local.from_utc_datetime(&**m).naive_local() < upper_bound
                    })
                         .unwrap_or(true))
        })
        .collect::<Vec<_>>();
    tasks.sort_by_key(|t| t.modified().map(|t| t.date()));
    tasks
}

impl PS {
    pub fn prio(&self) -> Option<TaskPriority> {
        match self {
            PS::High => Some(TaskPriority::High),
            PS::Medium => Some(TaskPriority::Medium),
            PS::Low => Some(TaskPriority::Low),
            PS::None | PS::Optional => None,
        }
    }
    pub fn timeout(&self) -> i64 {
        match self {
            PS::High => 1,
            PS::Medium => 1,
            PS::Low => 7,
            PS::None | PS::Optional => 30,
        }
    }

    pub fn only_optional(&self) -> bool {
        *self == PS::Optional
    }

    pub fn upper_bound(&self) -> NaiveDateTime {
        (Local::now() - Duration::days(self.timeout())).naive_local()
    }
}

pub fn prio_name(prio: Option<&TaskPriority>) -> &'static str {
    match prio {
        None => "Sometime",
        Some(TaskPriority::High) => "Today",
        Some(TaskPriority::Medium) => "This week",
        Some(TaskPriority::Low) => "This month",
    }
}

pub fn tags_string(tags: &Vec<String>) -> String {
    format!("+{}", tags.join(",+"))
}

fn print_task_short(task: &Task) -> String {
    let mut info = vec![task.description().clone()];
    if let Some(tags) = task.tags() {
        info.push(tags_string(tags));
    }
    if let Some(project) = task.project() {
        info.push(format!("{}", project));
    }
    if let Some(due) = task.due() {
        info.push(format!("due: {}", **due));
    }
    if let Some(wait) = task.wait() {
        info.push(format!("wait: {}", **wait));
    }
    if task.priority().is_some() {
        info.push(prio_name(task.priority()).into());
    }
    info.join("  |  ")
}

fn print_task(task: &Task) -> String {
    let mut info = vec![task.description().clone()];
    info.push(format!("status: {}", task.status()));
    if let Some(project) = task.project() {
        info.push(format!("project: {}", project));
    }
    if let Some(tags) = task.tags() {
        if tags.len() > 0 {
            info.push(format!("tags: +{}", tags.join(", +")));
        }
    }
    if let Some(due) = task.due() {
        info.push(format!("due: {}", **due));
    }
    if let Some(wait) = task.wait() {
        info.push(format!("wait: {}", **wait));
    }
    info.push(format!("priority: {}", prio_name(task.priority())));
    info.join("\n")
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
enum Mode {
    #[serde(rename = "work")]
    Work,
    #[serde(rename = "research")]
    Research,
    #[serde(rename = "orga")]
    Orga,
    #[serde(rename = "idle")]
    Idle,
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
enum Connectivity {
    #[serde(rename = "online")]
    Online,
    #[serde(rename = "offline")]
    Offline,
    #[serde(rename = "limited")]
    Limited,
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
enum Location {
    #[serde(rename = "home")]
    Home,
    #[serde(rename = "w17")]
    W17,
    #[serde(rename = "city")]
    City,
    #[serde(rename = "uni")]
    Uni,
    #[serde(rename = "anywhere")]
    Anywhere,
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
enum MPD {
    #[serde(rename = "apollo")]
    Apollo,
    #[serde(rename = "whisky")]
    Whisky,
    #[serde(rename = "kitchen")]
    Kitchen,
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct State {
    mode: Mode,
    location: Location,
    connectivity: Connectivity,
    mpd: MPD,
}

pub fn task_blocked(cache: &TaskCache, task: &Task) -> bool {
    task.depends()
        .map(|dependencies| {
            for dependency in dependencies {
                if cache.get(dependency).map(|t| !t.obsolete()).unwrap_or(
                    false,
                )
                {
                    return true;
                }
            }
            false
        })
        .unwrap_or(false)
}

pub fn task_in_inbox(cache: &TaskCache, task: &Task) -> bool {
    task.pending() && !task.tagged() && !task_blocked(cache, task)
}

fn save_state(state: &State) -> Result<()> {
    let state_path = &{
        let mut s = home_dir().chain_err(|| "No Home")?;
        s.push(".kassandra_state");
        s
    };
    let state_file = File::create(&state_path)?;
    to_writer(state_file, &state)?;
    Ok(())
}

fn get_state() -> Result<State> {
    let state_path = &{
        let mut s = home_dir().chain_err(|| "No Home")?;
        s.push(".kassandra_state");
        s
    };
    if let Ok(state_file) = File::open(&state_path) {
        if let Ok(state) = from_reader(state_file) {
            return Ok(state);
        }
    }
    let state = State {
        mode: Mode::Orga,
        connectivity: Connectivity::Online,
        location: Location::Anywhere,
        mpd: MPD::Apollo,
    };
    save_state(&state)?;
    Ok(state)
}

fn enter_new_task<T: DialogProvider, S: Into<String>>(dialog: &mut T, msg: S) -> Result<Task> {
    Ok(TaskBuilder::default()
        .description(dialog.input_line(msg, Vec::<String>::default())?)
        .build()?)
}

pub fn needs_sorting(task: &Task) -> bool {
    !task.has_tag("project") && !task.has_tag("kategorie")
}

pub struct Kassandra {
    pub state: State,
    pub dialog: RofiDialogProvider,
    pub cache: TaskCache,
}


impl Kassandra {
    fn new() -> Result<Self> {
        Ok(Kassandra {
            state: get_state()?,
            dialog: RofiDialogProvider {},
            cache: TaskCache::new(vec![TaskStatus::Deleted]),
        })
    }

    fn run(&mut self) -> Result<()> {
        self.cache.load()?;
        update_tasks(&mut self.cache)?;
        self.cache.write()?;
        self.handle_active_tasks()?;

        self.sort_mailbox(&*PRIVATE_MAILBOX, false)?;
        self.sort_mailbox(&*KIVA_MAILBOX, false)?;
        self.sort_mailbox(&*AK_MAILBOX, false)?;

        // CHECK_UNREAD_CHATS
        // CHECK_INBOXES
        //  maralorn.de
        //  kiva
        //  ak
        process_task(self, &*MAINTENANCE)?;
        process_task(self, &*SORT_INBOX)?;
        process_task(self, &*SORT_INBOX_KIVA)?;
        process_task(self, &*SORT_INBOX_AK)?;
        update_tasks(&mut self.cache)?;
        process_task(self, &*INBOX)?;
        process_task(self, &*TREESORT)?;
        process_task(self, &*CHECK_MEDIUM)?;
        process_task(self, &*CHECK_LOW)?;
        process_task(self, &*CHECK_NONE)?;
        process_task(self, &*CHECK_OPTIONAL)?;
        process_task(self, &*ACCOUNTING)?;
        self.select_next_task()?;
        Ok(())
    }


    fn get_notes(&mut self) -> Result<()> {
        loop {
            match enter_new_task(
                &mut self.dialog,
                "Do you have anything to note?
For example, what you are doing right now or still have to do today?",
            ) {
                Ok(task) => {
                    self.cache.set(task);
                    self.cache.write()?;
                }
                Err(Error(EK::DialogError(DEK::InputCanceled), _)) => break Ok(()),
                err => {
                    err?;
                }
            }
        }
    }

    fn confirm_state(&mut self) -> Result<()> {
        loop {
            match self.dialog.select_option(
                format!(
                    "State: {}

Do you want to change the state? (Esc to cancel)",
                    to_string(&self.state)?
                ),
                vec![
                    ("working: I am at work", "work"),
                    ("researching: I am researching", "research"),
                    ("orga: I am in normal mode", "orga"),
                    ("home: I am at home", "home"),
                    ("uni: I am at the Mathebau", "uni"),
                    ("hackspace: I am in the hackspace", "w17"),
                    ("city: I am in the city", "city"),
                    ("anywhere: I'm nowhere special", "anywhere"),
                    (
                        "idle: I am not interested in doing anything",
                        "idle"
                    ),
                ],
            )? {
                "work" => self.state.mode = Mode::Work,
                "research" => self.state.mode = Mode::Research,
                "orga" => self.state.mode = Mode::Orga,
                "home" => self.state.location = Location::Home,
                "w17" => self.state.location = Location::W17,
                "city" => self.state.location = Location::City,
                "idle" => self.state.mode = Mode::Idle,
                "uni" => self.state.location = Location::Uni,
                "anywhere" => self.state.location = Location::Anywhere,
                other => bail!("Unkown option {}", other),
            };
            save_state(&self.state)?;
        }
    }

    fn handle_active_tasks(&mut self) -> Result<()> {
        while let Some(uuid) = get_sorted_uuids(
            &self.cache,
            |t| t.start().is_some() && t.pending(),
        ).into_iter()
            .next()
        {
            let description = print_task(self.cache.get(&uuid).chain_err(|| "uuid miss")?);
            match self.dialog.select_option(
                format!(
                    "You are currently working on\n{}\nWhat's the progress?",
                    description
                ),
                vec![
                    ("Continue: I'll get back to it", "continue"),
                    ("Done: I've done that!", "done"),
                    ("Later: I'll do that later", "later"),
                    ("Edit: I have changes to this task", "edit"),
                ],
            )? {
                "done" => {
                    {
                        let task = self.cache.get_mut(&uuid).chain_err(|| "missing uuid")?;
                        task.tw_stop();
                        task.tw_done();
                    }
                    self.cache.write()?;
                }
                "later" => {
                    self.cache
                        .get_mut(&uuid)
                        .chain_err(|| "missing uuid")?
                        .tw_stop();
                    self.cache.write()?;
                }
                "edit" => {
                    self.cache
                        .get_mut(&uuid)
                        .chain_err(|| "missing uuid")?
                        .tw_stop();
                    self.cache.write()?;
                    self.edit_task(&uuid)?;
                }
                _ => {
                    self.work_on_task(&uuid)?;
                }
            };
        }
        Ok(())
    }

    pub fn assure_all_sorted(&mut self) -> Result<()> {
        self.cache.refresh_tree();
        while let Some(uuid) = get_sorted_uuids(
            &self.cache,
            |t| !t.obsolete() && needs_sorting(t),
        ).into_iter()
            .next()
        {
            self.sort(&uuid)?;
            self.cache.refresh_tree();
        }
        Ok(())
    }

    pub fn sort(&mut self, uuid: &Uuid) -> Result<()> {
        let task_name = print_task(self.cache.get(uuid).chain_err(|| "mising uuid")?);
        let partof = self.select_entry_point(
            format!("Select Project for Task\n{}", task_name),
            uuid,
        )?;
        self.cache
            .get_mut(uuid)
            .chain_err(|| "assure_sorted: missing uuid")?
            .set_partof(partof);
        update_tasks(&mut self.cache)?;
        self.cache.write()?;
        Ok(())
    }

    pub fn select_entry_point(
        &mut self,
        msg: impl Into<String>,
        uuid: &Uuid,
    ) -> Result<Option<Uuid>> {
        let msg = msg.into();
        let format_msg = |cache: &TaskCache, uuid, text| -> Result<String> {
            Ok(format!(
                "{}\n{}: {}",
                &msg,
                text,
                if let Some(uuid) = uuid {
                    cache.get_project_path(&uuid)?
                } else {
                    String::default()
                }
            ))
        };
        let mut parent = None;
        loop {
            match {
                let options = get_sorted_tasks(&self.cache, |t| {
                    t.pending() && t.partof().map(|partof| partof == parent).unwrap_or(false) &&
                        (parent.is_some() || t.has_tag("kategorie"))
                });
                let options = options.into_iter().map(|t| (print_task_short(t), Ok(t)));
                let options = vec![
                    ("select this level".into(), Err("here")),
                    ("insert new node".into(), Err("new")),
                    ("go one level up".into(), Err("up")),
                    ("edit task".into(), Err("edit")),
                ].into_iter()
                    .chain(options);
                let msg = format_msg(&self.cache, parent, "currently at")?;
                match self.dialog.select_option(msg, options)? {
                    Ok(task) => {
                        parent = Some(task.uuid().clone());
                        "stay"
                    }
                    Err(other) => other,
                }
            } {

                "here" => return Ok(parent),
                "new" => {
                    let mut task = enter_new_task(
                        &mut self.dialog,
                        format_msg(&self.cache, parent, "inserting new node at")?,
                    )?;
                    task.set_partof(parent);
                    parent = Some(task.uuid().clone());
                    self.cache.set(task);
                    self.cache.write()?;
                }

                "up" => {
                    parent = if let Some(parent) = parent {
                        if let Some(parent) = self.cache.get(&parent) {
                            parent.partof()?
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                "stay" => (),
                "edit" => {
                    self.edit_task(uuid)?;
                    parent = None;
                }
                other => bail!("Unkown option {}", other),
            }
        }
    }

    pub fn select_priority(&mut self, uuid: &Uuid) -> Result<()> {
        {
            enum O {
                H,
                M,
                L,
                N,
                O,
            }
            let task = self.cache.get_mut(uuid).chain_err(
                || "select_priority: missing uuid",
            )?;
            match self.dialog.select_option(
                format!(
                    "Choose a priority for task\n{}",
                    print_task(task)
                ),
                vec![
                    (prio_name(Some(&TaskPriority::High)), O::H),
                    (
                        prio_name(Some(&TaskPriority::Medium)),
                        O::M
                    ),
                    (prio_name(Some(&TaskPriority::Low)), O::L),
                    (prio_name(None), O::N),
                    ("Optional", O::O),
                ],
            )? {
                O::H => task.set_priority(Some(TaskPriority::High)),
                O::M => task.set_priority(Some(TaskPriority::Medium)),
                O::L => task.set_priority(Some(TaskPriority::Low)),
                O::N => task.set_priority(None as Option<TaskPriority>),
                O::O => task.add_tag("optional"),
            }
        }
        Ok(self.cache.write()?)
    }

    pub fn edit_task(&mut self, uuid: &Uuid) -> Result<()> {
        loop {
            let task_name = print_task(
                self.cache.get(uuid).chain_err(|| "edit_task: missing uuid")?,
            );
            match self.dialog.select_option(
            format!(
                "Handling Task: {}\nDo you want to do this now? Can it be done in under 2 minutes?",
                task_name
            ),
            vec![
                ("Now: I'll get to it", "do"),
                ("Done: I already did this", "done"),
                ("Delete: This does not need to be done anymore","delete"),
                ("Priority","prio"),
                ("Edit description: I want to change the description", "edit"),
                ("Move: Change position in tasktree", "move"),
                ("Split: Give this tasks subtasks", "split"),
                ("Depend: Set dependency", "depend"),
                ("Wait: Set wait time", "postpone"),
                ("Due: Set a deadline", "due"),
                ("Tag: Add a tag", "tag"),
                ("Clear tags", "clear_tags"),
                ("Manual: I have to change something by hand", "manual"),
                ("Quit", "quit"),
            ],
        )? {
            "do" => {
                self.work_on_task(uuid)?;
            }
            "done" => {
                self.cache.get_mut(uuid).chain_err(|| "BUG!")?.tw_done();
                self.cache.write()?;
                break;
            }
            "delete" => {
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .tw_delete();
                self.cache.write()?;
                break;
            }
            "edit" => {
                *self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .description_mut() = self.dialog.input_line(
                    format!(
                        "Enter new description for {}",
                        task_name
                    ),
                    vec![
                        self.cache
                            .get_mut(uuid)
                            .chain_err(|| "missing uuid")?
                            .description()
                            .clone(),
                    ],
                )?;
            }
            "manual" => {
                str2cmd("tasklauncher").output()?;
                self.cache.refresh()?;
            }
            "move" => {
                self.sort(uuid)?;
            }
            "quit" => {break;}
            "split" => {self.make_project(uuid)?;}
            "depend" => {
                let query = format!(
                    "Selecting dependency for task\n{}",
                    print_task(self.cache.get(uuid).chain_err(|| "missing uuid")?)
                );
                if let Some(dependency) = self.select_entry_point(query, uuid)? {
                    let task = self.cache.get_mut(uuid).chain_err(|| "missing uuid")?;
                    if task.depends().is_some() {
                        task.depends_mut().unwrap().push(dependency);
                    } else {
                        task.set_depends(Some(Some(dependency)));
                    }
                }
                self.cache.write()?;
            }
            "tag" => {
                self.add_tag(uuid)?;
            }
            "clear_tags" => {
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .set_tags(None as Option<Vec<String>>);
                self.cache.write()?;
            }
            "prio" => {
                self.select_priority(uuid)?;
            }
            "due" => {
                str2cmd(&format!(
                    "task {} mod due:{}",
                    uuid,
                    self.dialog.input_line(
                        format!(
                            "Select a deadline for {}",
                            task_name
                        ),
                        vec!["tomorrow"],
                    )?
                )).output()?;
                self.cache.refresh()?;
            }
            "postpone" => {
                str2cmd(&format!(
                    "task {} mod wait:{}",
                    uuid,
                    self.dialog.input_line(
                        format!(
                            "How long do you want to postpone {}",
                            task_name
                        ),
                        vec!["tomorrow"],
                    )?
                )).output()?;
                self.cache.refresh()?;
            }
            other => bail!("unknown option: {}", other)
        }
        }
        Ok(())
    }

    pub fn clear_inbox(&mut self) -> Result<()> {
        while let Some(uuid) = get_sorted_uuids(&self.cache, |t| task_in_inbox(&self.cache, t))
            .into_iter()
            .next()
        {
            self.handle_task(&uuid)?;
        }
        Ok(())
    }

    pub fn make_project(&mut self, uuid: &Uuid) -> Result<bool> {
        let task_name = print_task(self.cache.get(uuid).chain_err(|| "missing uuid")?);
        let mut new_tasks = vec![];
        loop {
            match enter_new_task(
                &mut self.dialog,
                format!("Adding sub tasks of {}\n\n(Esc to cancel)", task_name),
            ) {
                Ok(mut task) => {
                    task.set_partof(Some(uuid.clone()));
                    new_tasks.push(task.uuid().clone());
                    self.cache.set(task);
                    self.cache.write()?;
                }
                Err(Error(EK::DialogError(DEK::InputCanceled), _)) => break,
                err => {
                    err?;
                }
            }
        }
        if new_tasks.len() > 0 {
            for uuid in new_tasks {
                self.handle_task(&uuid)?;
            }
            update_tasks(&mut self.cache)?;
            self.cache.write()?;
            return Ok(true);
        } else {
            return Ok(false);
        }

    }

    pub fn add_tag(&mut self, uuid: &Uuid) -> Result<()> {
        enum Select {
            Edit,
            More,
            Tag(&'static str),
        };
        match self.dialog.select_option(
            format!(
                "Handling Task: {}\nWhen will you do this?",
                print_task(
                    self.cache.get(uuid).chain_err(|| "missing uuid")?,
                )
            ),
            vec![
                (
                    "Await: Somebody else has to do this",
                    Select::Tag("await")
                ),
                (
                    "PC: When I'm at my computer",
                    Select::Tag("pc")
                ),
                ("Work: While working", Select::Tag("work")),
                (
                    "Research: While doing research",
                    Select::Tag("research")
                ),
                (
                    "Home: When I'm at home",
                    Select::Tag("home")
                ),
                (
                    "Uni: At the university",
                    Select::Tag("uni")
                ),
                ("City: In the city", Select::Tag("city")),
                (
                    "More: Set other or multiple tags",
                    Select::More
                ),
                ("Edit: This task instead", Select::Edit),
            ],
        )? {
            Select::Tag(tag) => {
                {
                    let task = self.cache.get_mut(uuid).chain_err(|| "missing uuid")?;
                    task.set_tags(None as Option<Vec<String>>);
                    task.add_tag(tag);
                }
                self.cache.write()?;
            }
            Select::More => {
                let tags = self.dialog.input_line(
                    format!(
                        "Enter tag(s) for:\n{}",
                        print_task(
                            self.cache.get(uuid).chain_err(|| "missing uuid")?,
                        )
                    ),
                    vec![
                        "alber",
                        "streicher",
                        "burkhard",
                        "cornelia",
                        "nathalie",
                    ],
                )?;
                {
                    let task = self.cache.get_mut(uuid).chain_err(|| "missing uuid")?;
                    task.set_tags(None as Option<Vec<String>>);
                    for tag in tags.split_whitespace() {
                        task.add_tag(tag);
                    }
                }
                self.cache.write()?;
            }

            Select::Edit => self.edit_task(uuid)?,
        }
        Ok(())
    }

    pub fn work_on_task(&mut self, uuid: &Uuid) -> Result<()> {
        self.cache
            .get_mut(uuid)
            .chain_err(|| "Uuid not found")?
            .tw_start();
        self.cache.write()?;
        {
            let task = self.cache.get(uuid).chain_err(|| "Uuid not found")?;
            if task.gen_name() == Some(&"mail-task".to_owned()) {
                let message_id = task.gen_id()
                    .chain_err(|| "mail-task has no genid")?
                    .trim()
                    .trim_matches(|x| x == '<' || x == '>')
                    .replace("$", ".");
                let read_command = format!(
                    "push <vfolder-from-query>id:{}<return><search>~i{}<return><display-message>",
                    message_id,
                    message_id
                );
                str2cmd(&term_cmd("neomutt"))
                    .arg("-e")
                    .arg(read_command)
                    .output()?;
            } else {
                bail!(EK::WorkingOnTask(print_task(task)));
            }
        }
        self.cache
            .get_mut(uuid)
            .chain_err(|| "Uuid not found")?
            .tw_stop();
        self.cache.write()?;
        Ok(())
    }

    pub fn handle_task(&mut self, uuid: &Uuid) -> Result<()> {
        let task_name = print_task(self.cache.get(uuid).chain_err(
            || "handle_task: missing uuid",
        )?);
        match self.dialog.select_option(
            format!(
                "Handling Task: {}\nCan this be done in under 2 minutes?",
                task_name
            ),
            vec![
                ("Yes: I'll get to it", "do"),
                (
                    "No: Do you know how short 2 minutes are?",
                    "process"
                ),
                ("Done: I already did this", "done"),
                ("Delete: This task is obsolete", "delete"),
                (
                    "Edit: I'll change that task on my own",
                    "edit"
                ),
            ],
        )? {
            "do" => {
                self.work_on_task(uuid)?;
                return self.handle_task(uuid);
            }
            "done" => {
                self.cache.get_mut(uuid).chain_err(|| "BUG!")?.tw_done();
                self.cache.write()?;
                return Ok(());
            }
            "delete" => {
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .tw_delete();
                self.cache.write()?;
                return Ok(());
            }
            "process" => (),
            "edit" => {
                self.edit_task(uuid)?;
                return Ok(());
            }
            other => bail!("Unknown option: {}", other),
        }
        if needs_sorting(self.cache.get(uuid).chain_err(|| "unknown task")?) {
            self.sort(uuid)?;
        }
        if !self.make_project(uuid)? {
            if self.cache
                .get(uuid)
                .chain_err(|| "missing uuid")?
                .priority()
                .is_none()
            {
                self.select_priority(uuid)?;
            }
        }
        if self.cache
            .get(uuid)
            .chain_err(|| "unknown task")?
            .tags()
            .map(|t| t.len() == 0)
            .unwrap_or(true)
        {
            self.add_tag(uuid)?;
        }
        Ok(())
    }


    pub fn check_priorities(&mut self, priority: PriorityState) -> Result<()> {
        let mut count = 0;
        while let Some(uuid) = get_stale_tasks(&self.cache, priority)
            .into_iter()
            .next()
            .map(|t| t.uuid().clone())
        {
            self.priority_check(&uuid, priority)?;
            if priority.0 == PS::Optional {
                count += 1;
                if count > 10 {
                    return Ok(());
                }
            }
        }
        Ok(())
    }

    pub fn show_priorities(&mut self, mut priority: PriorityState) -> Result<()> {
        enum Select {
            P(PS),
            T(Uuid),
        };
        loop {
            let options = {
                let mut tasks = get_stale_tasks(&self.cache, priority);
                if priority.1 {
                    tasks.retain(|t| self.is_relevant(t));
                }
                let tasks = tasks.into_iter().map(|t| {
                    (print_task_short(t), Select::T(t.uuid().clone()))
                });
                let mut options = vec![PS::High, PS::Low, PS::Medium, PS::None, PS::Optional];
                options.retain(|t| *t != priority.0);
                let options = options.into_iter().map(|t| {
                    (
                        format!("{:?}: Show tasks of priority {:?}", t, t),
                        Select::P(t),
                    )
                });
                options.chain(tasks).collect::<Vec<_>>()
            };
            let msg = format!(
                "Showing {} tasks of priority {:?}",
                if priority.1 { "only relevant" } else { "stale" },
                priority.0
            );
            match self.dialog.select_option(msg, options)? {
                Select::P(ps) => priority.0 = ps,
                Select::T(uuid) => return self.priority_check(&uuid, priority),
            };
        }
    }

    pub fn priority_check(&mut self, uuid: &Uuid, priority: PriorityState) -> Result<()> {
        enum Select {
            ShowAll,
            Edit,
            Keep,
            Demote,
            Promote,
        };
        let mut options = vec![
            ("Keep: Leave priority as it is", Select::Keep),
            ("Edit", Select::Edit),
            ("Show all: Show all tasks of this priority", Select::ShowAll),
        ];
        if priority.0 != PS::Optional {
            options.insert(0, ("Demote: Lower priority", Select::Demote));
        }
        if priority.0 != PS::High {
            options.insert(0, ("Promote: Raise priority", Select::Promote));
        }
        let e = &format!("Called priority_check with missing uuid {}", uuid);
        let msg = format!(
            "Checking up on the following task, because of elapsed timeout:\n{}",
            print_task(self.cache.get(uuid).expect(e))
        );
        match (self.dialog.select_option(msg, options)?, priority.0) {
            (Select::ShowAll, _) => {
                self.show_priorities(priority)?;
            }
            (Select::Edit, _) => {
                self.edit_task(uuid)?;
            }
            (Select::Keep, _) => {
                self.cache.get_mut(uuid).expect(e).add_tag("toggle");
                self.cache.write()?;
                self.cache.get_mut(uuid).expect(e).remove_tag("toggle");
                self.cache.refresh()?;
            }
            (Select::Promote, PS::Medium) => {
                self.cache.get_mut(uuid).expect(e).set_priority(
                    Some(TaskPriority::High),
                );
                self.cache.write()?;
            }
            (Select::Demote, PS::High) |
            (Select::Promote, PS::Low) => {
                self.cache.get_mut(uuid).expect(e).set_priority(
                    Some(TaskPriority::Medium),
                );
                self.cache.write()?;
            }
            (Select::Demote, PS::Medium) |
            (Select::Promote, PS::None) => {
                self.cache.get_mut(uuid).expect(e).set_priority(
                    Some(TaskPriority::Low),
                );
                self.cache.write()?;
            }
            (Select::Demote, PS::Low) |
            (Select::Promote, PS::Optional) => {
                {
                    let t = self.cache.get_mut(uuid).expect(e);
                    t.set_priority(None as Option<TaskPriority>);
                    t.remove_tag("optional");
                }
                self.cache.write()?;
            }
            (Select::Demote, PS::None) => {
                {
                    let t = self.cache.get_mut(uuid).expect(e);
                    t.set_tags(None as Option<Vec<String>>);
                    t.add_tag("optional");
                }
                self.cache.write()?;
            }
            (_, _) => bail!("invalid demotion/promotion"),
        }
        Ok(())
    }

    pub fn select_next_task(&mut self) -> Result<()> {
        enum Select {
            T(Uuid),
            Manual,
            PromoteTasks,
            ChangeState,
        };
        loop {
            let mut empty = true;
            let options = {
                let options = vec![
                    ("Manual: Edit my tasks".into(), Select::Manual),
                    (
                        "Promote: Pick tasks with lower priority".into(),
                        Select::PromoteTasks
                    ),
                    ("Status: Change status".into(), Select::ChangeState),
                ].into_iter();
                let tasks = get_sorted_tasks(&self.cache, |t| {
                    t.pending() && self.is_relevant(t) && !task_blocked(&self.cache, t) &&
                        t.priority() == Some(TaskPriority::High).as_ref()
                }).into_iter();
                let tasks = tasks.map(|t| {
                    empty = false;
                    (print_task_short(t), Select::T(t.uuid().clone()))
                });
                options.chain(tasks).collect::<Vec<_>>()
            };
            if empty {
                return self.show_priorities((PS::Medium, true));
            };
            let msg = format!("What do you want to do now?");

            match self.dialog.select_option(msg, options)? {
                Select::Manual => {
                    str2cmd("tasklauncher").output()?;
                    self.cache.refresh()?;
                }
                Select::PromoteTasks => {
                    self.show_priorities((PS::Medium, true))?;
                    self.cache.refresh()?;
                }
                Select::ChangeState => {
                    self.confirm_state()?;
                    self.cache.refresh()?;
                }
                Select::T(u) => self.edit_task(&u)?,
            }
            self.cache.write()?;
        }
    }

    fn is_relevant(&self, task: &Task) -> bool {
        let can_do_this_here = |location| match location {
            Location::Home => task.has_tag("home"),
            Location::City => task.has_tag("city"),
            Location::Uni => task.has_tag("uni"),
            _ => false,
        };

        match self.state.mode {
            Mode::Research => task.has_tag("research"),
            Mode::Work => task.has_tag("work") || can_do_this_here(self.state.location),
            Mode::Orga => task.has_tag("pc") || can_do_this_here(self.state.location),
            Mode::Idle => false,
        }
    }


    pub fn sort_mailbox(&mut self, mailbox: &SortBox, sort_read: bool) -> Result<()> {
        sort_mailbox(&mut self.dialog, mailbox, sort_read)
    }
}

fn notify_result(result: Result<()>) -> Result<()> {
    fn show(msg: &str) -> Result<()> {
        let msg = format!("message='{}'", msg);
        str2cmd("eventc notification kassandra")
            .arg("-d")
            .arg("title='Kassandra'")
            .arg("-d")
            .arg(&msg)
            .output()?;
        Ok(())
    }
    match result {
        Err(Error(EK::DialogError(DEK::InputCanceled), _)) => Ok(()),
        Err(err) => {
            show(&format!("{}", err))?;
            Err(err)
        }
        Ok(()) => show("Kassandra finished"),
    }
}

pub fn change_state() -> Result<()> {
    Kassandra::new()?.confirm_state()
}

pub fn new_tasks() -> Result<()> {
    Kassandra::new()?.get_notes()
}

pub fn kassandra() -> Result<()> {
    notify_result(Kassandra::new()?.run())
}
