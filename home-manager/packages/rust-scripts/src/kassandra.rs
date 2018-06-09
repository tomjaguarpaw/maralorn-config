use std::fs::File;
use std::env::home_dir;
use std::iter::once;

use serde_yaml::{from_reader, to_writer, to_string};

use uuid::Uuid;

use task_hookrs::cache::TaskCache;
use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::priority::TaskPriority;

use dialog::rofi::RofiDialogProvider;
use dialog::DialogProvider;
use dialog::errors::ErrorKind as DEK;

use update::update_tasks;
use generate::GeneratedTask;
use error::{Result, ResultExt, ErrorKind as EK, Error};
use hotkeys::{str2cmd, term_cmd};
use tasktree::{TreeCache, TaskNode};

fn prio_name(prio: Option<&TaskPriority>) -> &'static str {
    match prio {
        None => "Sometime",
        Some(TaskPriority::High) => "Today",
        Some(TaskPriority::Medium) => "This week",
        Some(TaskPriority::Low) => "This month",
    }
}

fn print_task_short(task: &Task) -> String {
    let mut info = vec![task.description().clone()];
    if let Some(tags) = task.tags() {
        info.push(format!("+{}", tags.join(",+")));
    }
    if let Some(project) = task.project() {
        info.push(format!("{}", project));
    }
    info.push(prio_name(task.priority()).into());
    info.join("  |  ")
}

fn print_task(task: &Task) -> String {
    let mut info = vec![task.description().clone()];
    info.push(format!("status: {}", task.status()));
    if let Some(project) = task.project() {
        info.push(format!("project: {}", project));
    }
    if let Some(tags) = task.tags() {
        info.push(format!("tags: +{}", tags.join(", +")));
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
struct State {
    mode: Mode,
    location: Location,
    connectivity: Connectivity,
    mpd: MPD,
}

fn task_blocked(cache: &TaskCache, task: &Task) -> bool {
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


fn task_inbox(cache: &TaskCache, task: &Task) -> bool {
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

fn needs_sorting(task: &Task) -> bool {
    !task.has_tag("project") && task.partof().map(|x| x.is_none()).unwrap_or(false)
}

struct Kassandra {
    state: State,
    dialog: RofiDialogProvider,
    cache: TaskCache,
}


impl Kassandra {
    fn new() -> Result<Self> {
        Ok(Kassandra {
            state: get_state()?,
            dialog: RofiDialogProvider {},
            cache: TaskCache::new(vec![]),
        })
    }


    fn run(&mut self) -> Result<()> {
        self.cache.load()?;
        update_tasks(&mut self.cache)?;
        self.cache.write()?;
        self.handle_active_tasks()?;
        self.clear_inbox()?;
        self.cache.refresh_tree();
        self.assure_all_sorted()?;

        // check unread E-Mail + ak?
        // update
        //   system
        //   home
        // mails
        //   sort inbox
        //   sort inboxkiva
        //   sort inboxak
        //   sort to sort
        //   go trough todo
        //   go trough toread
        //   go trough readlater
        // pick tasks
        // optional, later
        // await
        // dirty gits

        // task generator
        // task completion helper
        self.update_accounting()?;
        self.select_next_task()?;
        Ok(())
    }

    fn get_sorted_uuids(&self, filter: impl Fn(&Task) -> bool) -> Vec<Uuid> {
        self.get_sorted_tasks(filter)
            .into_iter()
            .map(|x| x.uuid().clone())
            .collect()
    }

    fn get_sorted_tasks(&self, filter: impl Fn(&Task) -> bool) -> Vec<&Task> {
        let mut tasks = self.cache.filter(filter).collect::<Vec<_>>();
        tasks.sort_unstable_by_key(|t| t.entry().date());
        tasks
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
                _ => (),
            };
            save_state(&self.state)?;
        }
    }

    fn handle_active_tasks(&mut self) -> Result<()> {
        while let Some(uuid) = self.get_sorted_uuids(|t| t.start().is_some() && t.pending())
            .into_iter()
            .next()
        {
            match self.dialog.select_option(
                format!(
                    "You are currently working on {}
What's the progress?",
                    print_task(
                        self.cache.get(&uuid).chain_err(|| "uuid miss")?,
                    )
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
                _ => bail!("Continuing with task"),
            };
        }
        Ok(())
    }

    pub fn assure_all_sorted(&mut self) -> Result<()> {
        if self.cache
            .filter(|t| {
                t.gen_name() == Some(&"Sortiere Tasktree".into()) && t.pending()
            })
            .next()
            .is_some()
        {
            while let Some(uuid) = self.get_sorted_uuids(|t| !t.obsolete() && needs_sorting(t))
                .into_iter()
                .next()
            {
                self.sort(&uuid)?;
            }
            for task in self.cache.filter_mut(|t| {
                t.gen_name() == Some(&"Sortiere Tasktree".into()) && t.pending()
            })
            {
                task.tw_done()
            }
            self.cache.refresh_tree();
            self.cache.write()?;
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

    pub fn select_entry_point<T: Into<String>>(
        &mut self,
        msg: T,
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
                let mut options = self.cache
                    .filter(|t| {
                        t.pending() && t.partof().map(|partof| partof == parent).unwrap_or(false) &&
                            (parent.is_some() || t.has_tag("project"))
                    })
                    .collect::<Vec<_>>();
                options.sort_unstable_by_key(|t| t.entry().date());
                match self.dialog.select_option(
                    format_msg(&self.cache, parent, "currently at")?,
                    vec![
                        ("select this level".into(), Err(0)),
                        ("insert new node".into(), Err(1)),
                        ("go one level up".into(), Err(2)),
                        ("edit task".into(), Err(4)),
                    ].into_iter()
                        .chain(options.into_iter().map(|t| (print_task_short(t), Ok(t)))),
                )? {
                    Ok(task) => {
                        parent = Some(task.uuid().clone());
                        3
                    }
                    Err(n) => n,
                }
            } {

                0 => return Ok(parent),
                1 => {
                    let mut task = enter_new_task(
                        &mut self.dialog,
                        format_msg(&self.cache, parent, "inserting new node at")?,
                    )?;
                    task.set_partof(parent);
                    parent = Some(task.uuid().clone());
                    self.cache.set(task);
                    self.cache.write()?;
                }

                2 => {
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
                3 => (),
                4 => {
                    self.edit_task(uuid)?;
                    parent = None;
                }
                _ => bail!("Hö?"),
            }
        }
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
                ("Do it: I'll get to it", "do"),
                ("Done: I already did this", "done"),
                ("Delete: This does not need to be done anymore","delete"),
                ("Edit description: I want to change the description", "edit"),
                ("Move: Change position in tasktree", "move"),
                ("Split: Give this tasks subtasks", "split"),
                ("Depend: Set dependency", "depend"),
                ("Postpone: Set wait time", "postpone"),
                ("Tag: Add a tag", "tag"),
                ("Clear tags", "clear_tags"),
                ("Manual: I have to change something by hand", "manual"),
                ("Quit", "quit"),
            ],
        )? {
            "do" => {
                self.cache.get_mut(uuid).chain_err(|| "BUG!")?.tw_start();
                self.cache.write()?;
                bail!("Work on Task now!");
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
                let tag = self.dialog.input_line(
                    format!("Enter tag for {}", task_name),
                    vec![
                        "alber",
                        "await",
                        "city",
                        "home",
                        "pc",
                        "research",
                        "streicher",
                        "uni",
                        "work",
                        "claire",
                        "burkhard",
                        "cornelia",
                        "nathalie",
                    ],
                )?;
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .add_tag(tag);
                self.cache.write()?;
            }
            "clear_tags" => {
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .set_tags(None as Option<Vec<String>>);
                self.cache.write()?;
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
            _ => {}
        }
        }
        Ok(())
    }

    pub fn clear_inbox(&mut self) -> Result<()> {
        if self.cache
            .filter(|t| {
                t.gen_name() == Some(&"Leere Inbox".into()) && t.pending()
            })
            .next()
            .is_some()
        {
            while let Some(uuid) = self.get_sorted_uuids(|t| task_inbox(&self.cache, t))
                .into_iter()
                .next()
            {
                self.handle_task(&uuid)?;
            }
            for task in self.cache.filter_mut(|t| {
                t.gen_name() == Some(&"Leere Inbox".into()) && t.pending()
            })
            {
                task.tw_done()
            }
            self.cache.write()?;
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
                ("Yes: I'll get to it", Some(true)),
                (
                    "No: Do you know how short 2 minutes are?",
                    Some(false)
                ),
                (
                    "Edit: I'll change that task on my own",
                    None
                ),
            ],
        )? {
            Some(true) => {
                self.cache.get_mut(uuid).chain_err(|| "BUG!")?.tw_start();
                self.cache.write()?;
                bail!("Work on Task now!");
            }
            Some(false) => (),
            None => {
                self.edit_task(uuid)?;
                return Ok(());
            }
        }
        if needs_sorting(self.cache.get(uuid).chain_err(|| "unknown task")?) {
            self.sort(uuid)?;
        }
        if !self.make_project(uuid)? {
            if let Some(tag) = self.dialog.select_option(
                format!(
                    "Handling Task: {}\nWhen will you do this?",
                    print_task(
                        self.cache.get(uuid).chain_err(|| "missing uuid")?,
                    )
                ),
                vec![
                    ("Optional: I might never", Some("optional")),
                    ("Later: This has to wait", Some("later")),
                    (
                        "Await: Somebody else has to do this",
                        Some("await")
                    ),
                    ("PC: When I'm at my computer", Some("pc")),
                    ("Work: While working", Some("work")),
                    (
                        "Research: While doing research",
                        Some("research")
                    ),
                    ("Home: When I'm at home", Some("home")),
                    ("Edit: This task instead", None),
                ],
            )?
            {
                self.cache
                    .get_mut(uuid)
                    .chain_err(|| "missing uuid")?
                    .add_tag(tag);
                self.cache.write()?;
            } else {
                self.edit_task(uuid)?;
            }
        }
        Ok(())
    }

    pub fn update_accounting(&mut self) -> Result<()> {
        if self.cache
            .filter(|t| {
                t.gen_name() == Some(&"Aktualisiere Buchhaltung".into()) && t.pending()
            })
            .next()
            .is_some()
        {
            str2cmd(&term_cmd("sh -c"))
                .arg("jali -l. && task gen_id:'Aktualisiere Buchhaltung' done")
                .output()?;
        }
        self.cache.write()?;
        Ok(())
    }

    pub fn select_next_task(&mut self) -> Result<()> {
        while self.cache
            .filter(|t| t.start().is_some() && t.pending())
            .next()
            .is_none() &&
            self.cache
                .filter(|t| {
                    t.pending() && self.is_relevant(t) && !task_blocked(&self.cache, t)
                })
                .next()
                .is_some()
        {
            if let Some(uuid) = {
                let next_tasks = self.get_sorted_tasks(|t| {
                    t.pending() && self.is_relevant(t) && !task_blocked(&self.cache, t)
                }).into_iter()
                    .map(|t| (print_task_short(t), Some(t.uuid().clone())))
                    .collect::<Vec<_>>();
                self.dialog.select_option(
                    "What are you going to do now?",
                    once(("Manual: Edit my tasks".into(), None))
                        .chain(next_tasks.into_iter()),
                )?
            }
            {
                self.edit_task(&uuid)?;
            } else {
                str2cmd("tasklauncher").output()?;
                self.cache.refresh()?;
                return self.select_next_task();
            }
        }
        Ok(())
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
            Mode::Work => {
                task.has_tag("work") || task.has_tag("pc") || can_do_this_here(self.state.location)
            }
            Mode::Orga => task.has_tag("pc") || can_do_this_here(self.state.location),
            Mode::Idle => false,
        }
    }
}

pub fn change_state() -> Result<()> {
    Kassandra::new()?.confirm_state()
}

pub fn new_tasks() -> Result<()> {
    Kassandra::new()?.get_notes()
}

pub fn kassandra() -> Result<()> {
    Kassandra::new()?.run()
}
