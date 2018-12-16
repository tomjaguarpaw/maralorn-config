use hostname::get_hostname;

use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::cache::TaskCache;

use error::{Result, ResultExt};
use generate::{gen_match, GeneratedTask};
use refresh::{Timer, CalendarRepeater, Interval};
use kassandra::{Kassandra, task_in_inbox, needs_sorting, prio_name, PriorityState, PS,
                get_stale_tasks};
use hotkeys::{str2cmd, term_cmd};
use tasktree::TaskNode;
use mail::{mailbox_dirty, SortBox};

use chrono::{NaiveDate, NaiveTime, Duration};

pub trait WellKnown {
    fn definition(&self) -> Task;
    fn is_this(&self, task: &Task) -> bool {
        gen_match(task, &self.definition())
    }
    fn action_necessary(&self, &TaskCache) -> Result<bool> {
        Ok(true)
    }
    fn process(&self, &mut Kassandra) -> Result<bool> {
        Ok(false)
    }
    fn refresh(&self) -> Timer;
}

lazy_static! {
    pub static ref DAILY: Timer = Timer::Repetition(CalendarRepeater {
        date: NaiveDate::from_ymd(2018, 5, 8),
        time: NaiveTime::from_hms(20, 0, 0),
        repeat: Interval::Day(1)
    });
    pub static ref MORNING: Timer = Timer::Repetition(CalendarRepeater {
        date: NaiveDate::from_ymd(2018, 5, 8),
            time: NaiveTime::from_hms(6, 0, 0),
        repeat: Interval::Day(1)
    });
    pub static ref SIMPLE: Vec<SimpleTask> = make_simple();
    pub static ref INBOX: Inbox = Inbox { timer: DAILY.clone() };
    pub static ref TREESORT: Treesort = Treesort { timer: DAILY.clone() };
    pub static ref ACCOUNTING: Accounting = Accounting { timer: DAILY.clone() };
    pub static ref MEDITATION: Meditation = Meditation { timer: MORNING.clone() };
    pub static ref MAINTENANCE_APOLLO: Maintenance = Maintenance { timer: DAILY.clone() , host: "apollo".into() };
    pub static ref CHECK_HIGH: TaskCheck = TaskCheck { timer: MORNING.clone(), priority: (PS::High, false) };
    pub static ref CHECK_MEDIUM: TaskCheck = TaskCheck { timer: MORNING.clone(), priority: (PS::Medium, false) };
    pub static ref CHECK_LOW: TaskCheck = TaskCheck { timer: DAILY.clone(), priority: (PS::Low, false) };
    pub static ref CHECK_NONE: TaskCheck = TaskCheck { timer: DAILY.clone(), priority: (PS::None, false) };
    pub static ref CHECK_OPTIONAL: TaskCheck = TaskCheck { timer: DAILY.clone(), priority: (PS::Optional,false) };
    pub static ref CHECK_DIRTY_GITS: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_INBOX: Mailsort = Mailsort::new(DAILY.clone(), &PRIVATE_MAILBOX); // wichtig
    pub static ref SORT_INBOX_KIVA: Mailsort = Mailsort::new(DAILY.clone(), &KIVA_MAILBOX); // wichtig
    pub static ref SORT_INBOX_AK: Mailsort = Mailsort::new(DAILY.clone(), &AK_MAILBOX); // wichtig
    pub static ref SORT_MAIL: SimpleTask = unimplemented!(); // zufällige 50? nicht so wichtig
    pub static ref SORT_MAIL_KIVA: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_MAIL_AK: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_SPAM: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref PRIVATE_MAILBOX: SortBox = SortBox {
        mailbox: "m-0/INBOX",
        option_dirs: vec![
            ("Todo","m-0/Bearbeiten/todo"),
            ("Spam","m-0/Spam"),
            ("Delete","m-0/Trash"),
            ("Archiv","m-0/Archiv/unsortiert"),
            ("Readlater","m-0/Move/readlater"),
            ("Lesen","m-0/Bearbeiten/lesen"),
            ("Kiva","fb4/INBOX"),
            ("Auslandskoordination","ak/INBOX"),
        ]
    };
    pub static ref KIVA_MAILBOX: SortBox = SortBox {
        mailbox: "fb4/INBOX",
        option_dirs: vec![
            ("Todo","fb4/Bearbeiten/todo"),
            ("Spam","fb4/SPAM"),
            ("Delete","fb4/Trash"),
            ("Archiv","fb4/Archiv/sortieren"),
            ("Lesen","fb4/Bearbeiten/lesen"),
            ("Privat","m-0/INBOX"),
            ("Auslandskoordination","ak/INBOX")
        ]
    };
    pub static ref AK_MAILBOX: SortBox = SortBox {
        mailbox: "ak/INBOX",
        option_dirs: vec![
            ("Todo","ak/Malte/bearbeiten"),
            ("Spam","ak/SPAM"),
            ("Delete","ak/Trash"),
            ("Archiv","ak/Malte/sortieren"),
            ("Lesen","ak/Malte/lesen"),
            ("Privat","m-0/INBOX"),
            ("Kiva","fb4/INBOX")
        ]
    };
}

pub struct SimpleTask {
    definition: Task,
    timer: Timer,
}

impl SimpleTask {
    fn new(
        description: impl Into<String>,
        gen_name: impl Into<String>,
        gen_id: impl Into<String>,
        timer: Timer,
    ) -> Self {
        let mut task = TaskBuilder::default()
            .description(description.into())
            .build()
            .expect("TaskBuilding failed inspite of set description");
        task.set_gen_name(Some(gen_name.into()));
        task.set_gen_id(Some(gen_id.into()));
        SimpleTask {
            definition: task,
            timer: timer,
        }
    }
}


impl WellKnown for SimpleTask {
    fn definition(&self) -> Task {
        self.definition.clone()
    }
    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

fn simple_task(name: &str, timer: Timer) -> SimpleTask {
    SimpleTask::new(name, name, name, timer)
}

fn simple_tasks(
    names: impl IntoIterator<Item = impl Into<String>>,
    timer: Timer,
) -> impl Iterator<Item = SimpleTask> {
    names.into_iter().map(move |x| {
        simple_task(&x.into(), timer.clone())
    })
}

fn make_simple() -> Vec<SimpleTask> {
    let weekly = Timer::Repetition(CalendarRepeater {
        date: NaiveDate::from_ymd(2018, 5, 8),
        time: NaiveTime::from_hms(20, 0, 0),
        repeat: Interval::Day(7),
    });
    let monthly = Timer::Repetition(CalendarRepeater {
        date: NaiveDate::from_ymd(2018, 5, 3),
        time: NaiveTime::from_hms(20, 0, 0),
        repeat: Interval::Month(1),
    });
    simple_tasks(
        vec![
            "Staubsaugen",
            "Putze Waschbecken",
            "Wäsche sortieren und entscheiden, welche Waschgänge notwendig sind",
        ],
        Timer::DeadTime(Duration::weeks(2)),
    ).chain(simple_tasks(
        vec![
            "Reinige Toilette",
            "Zehennägel schneiden",
            "Matekasse leeren",
        ],
        Timer::DeadTime(Duration::weeks(4)),
    ))
        .chain(simple_tasks(
            vec!["Friseurtermin machen"],
            Timer::DeadTime(Duration::weeks(6)),
        ))
        .chain(simple_tasks(
            vec!["Verbuche Kontoauszüge", "Cryptpads sichern"],
            monthly,
        ))
        .chain(simple_tasks(
            vec![
                "Korrigiere Portemonnaiezählstand",
                "Block leeren und wegsortieren",
                "Leere Kiva Fächer",
                "Inbox zu Hause wegsortieren",
            ],
            weekly,
        ))
        .collect::<Vec<_>>()
}

#[derive(Clone, Copy)]
pub struct Inbox {
    timer: Timer,
}

impl WellKnown for Inbox {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description("Leere Inbox")
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("inbox"));
        t.set_gen_id(Some("inbox"));
        t
    }

    fn action_necessary(&self, cache: &TaskCache) -> Result<bool> {
        Ok(cache.filter(|t| task_in_inbox(&cache, t)).next().is_some())
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<bool> {
        kassandra.clear_inbox()?;
        Ok(true)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

#[derive(Clone, Copy)]
pub struct Meditation {
    timer: Timer,
}

impl WellKnown for Meditation {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description("Meditation")
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("meditation"));
        t.set_gen_id(Some("meditation"));
        t
    }

    fn action_necessary(&self, _cache: &TaskCache) -> Result<bool> {
        Ok(true)
    }

    fn process(&self, _kassandra: &mut Kassandra) -> Result<bool> {
        str2cmd(&term_cmd("sh -c")).arg("meditate").spawn()?.wait()?;
        Ok(false)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

#[derive(Clone, Copy)]
pub struct Accounting {
    timer: Timer,
}

impl WellKnown for Accounting {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description("Aktualisiere Buchhaltung")
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("accounting"));
        t.set_gen_id(Some("accounting"));
        t
    }

    fn action_necessary(&self, _cache: &TaskCache) -> Result<bool> {
        Ok(true)
    }

    fn process(&self, _kassandra: &mut Kassandra) -> Result<bool> {
        str2cmd(&term_cmd("sh -c"))
            .arg("jali -l. && task gen_id:accounting done")
            .spawn()?
            .wait()?;
        Ok(false)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

#[derive(Clone, Copy)]
pub struct Treesort {
    timer: Timer,
}

impl WellKnown for Treesort {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description("Sortiere Tasktree")
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("treesort"));
        t.set_gen_id(Some("treesort"));
        t
    }

    fn action_necessary(&self, cache: &TaskCache) -> Result<bool> {
        Ok(
            cache
                .filter(|t| !t.obsolete() && needs_sorting(t))
                .next()
                .is_some(),
        )
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<bool> {
        kassandra.assure_all_sorted()?;
        Ok(true)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}


pub struct Mailsort {
    timer: Timer,
    mailbox: &'static SortBox,
    task: Task,
}

impl Mailsort {
    fn new(timer: Timer, mailbox: &'static SortBox) -> Mailsort {
        Mailsort {
            timer,
            mailbox,
            task: {
                let mut t = TaskBuilder::default()
                    .description(format!("Sortiere Mailbox {}", &mailbox.mailbox))
                    .build()
                    .expect("TaskBuilding failed inspite of set description");
                t.set_gen_name(Some("mailboxsort"));
                t.set_gen_id(Some(mailbox.mailbox));
                t
            },
        }
    }
}

impl WellKnown for Mailsort {
    fn definition(&self) -> Task {
        self.task.clone()
    }

    fn action_necessary(&self, _cache: &TaskCache) -> Result<bool> {
        mailbox_dirty(&self.mailbox.mailbox)
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<bool> {
        kassandra.sort_mailbox(&self.mailbox, true)?;
        Ok(true)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

pub struct Maintenance {
    timer: Timer,
    host: String,
}

impl WellKnown for Maintenance {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description(format!("Run system Maintenance on {}", &self.host))
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("maintenance"));
        t.set_gen_id(Some(self.host.clone()));
        t
    }

    fn action_necessary(&self, _cache: &TaskCache) -> Result<bool> {
        Ok(true)
    }

    fn process(&self, _kassandra: &mut Kassandra) -> Result<bool> {
        let host = get_hostname().chain_err(|| "Failed to get_hostname")?;
        if host == self.host {
            str2cmd("maintenance").spawn()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

#[derive(Clone, Copy)]
pub struct TaskCheck {
    timer: Timer,
    priority: PriorityState,
}

impl WellKnown for TaskCheck {
    fn definition(&self) -> Task {
        let mut t = TaskBuilder::default()
            .description(format!(
                "Check tasks matching \"{:}\"",
                if self.priority.0.only_optional() {
                    "+optional"
                } else {
                    prio_name(self.priority.0.prio().as_ref())
                }
            ))
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("task_check"));
        t.set_gen_id(Some(format!("{:?}", self.priority)));
        t
    }

    fn action_necessary(&self, cache: &TaskCache) -> Result<bool> {
        Ok(get_stale_tasks(cache, self.priority).len() > 0)
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<bool> {
        kassandra.check_priorities(self.priority)?;
        Ok(true)
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}
