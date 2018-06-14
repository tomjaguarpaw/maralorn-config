use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::cache::TaskCache;

use error::Result;
use generate::{gen_match, GeneratedTask};
use refresh::{Timer, CalendarRepeater, Interval};
use kassandra::{Kassandra, task_in_inbox, needs_sorting};
use hotkeys::{str2cmd, term_cmd};
use tasktree::TaskNode;

use chrono::{NaiveDate, NaiveTime, Duration};

pub trait WellKnown {
    fn definition(&self) -> &Task;
    fn is_this(&self, task: &Task) -> bool {
        gen_match(task, self.definition())
    }
    fn action_necessary(&self, &TaskCache) -> Result<bool> {
        Ok(true)
    }
    fn process(&self, &mut Kassandra) -> Result<()> {
        Ok(())
    }
    fn refresh(&self) -> Timer;
}

lazy_static! {
    pub static ref SIMPLE: Vec<SimpleTask> = make_simple();
    pub static ref INBOX: Inbox = Inbox {
        timer: Timer::Repetition(CalendarRepeater {
                date: NaiveDate::from_ymd(2018, 5, 8),
                time: NaiveTime::from_hms(20, 0, 0),
                repeat: Interval::Day(1),
    })};
    pub static ref TREESORT: Treesort = Treesort {
        timer: Timer::Repetition(CalendarRepeater {
                date: NaiveDate::from_ymd(2018, 5, 8),
                time: NaiveTime::from_hms(20, 0, 0),
                repeat: Interval::Day(1),
    })};
    pub static ref ACCOUNTING: Accounting = Accounting {
        timer: Timer::Repetition(CalendarRepeater {
                date: NaiveDate::from_ymd(2018, 5, 8),
                time: NaiveTime::from_hms(20, 0, 0),
                repeat: Interval::Day(1),
    })};
    pub static ref CHECK_MEDIUM: SimpleTask = unimplemented!(); // auch +await // einfach
    pub static ref CHECK_LOW: SimpleTask = unimplemented!(); // einfach
    pub static ref CHECK_NONE: SimpleTask = unimplemented!(); //einfach
    pub static ref CHECK_OPTIONAL: SimpleTask = unimplemented!(); // zufällig 10 //einfach
    pub static ref CHECK_DIRTY_GITS: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_INBOX: SimpleTask = unimplemented!(); // wichtig
    pub static ref SORT_INBOX_KIVA: SimpleTask = unimplemented!(); // wichtig
    pub static ref SORT_INBOX_AK: SimpleTask = unimplemented!(); // wichtig
    pub static ref SORT_MAIL: SimpleTask = unimplemented!(); // zufällige 50? nicht so wichtig
    pub static ref SORT_MAIL_KIVA: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_MAIL_AK: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref SORT_SPAM: SimpleTask = unimplemented!(); // nicht so wichtig
    pub static ref UPDATE_APOLLO: SimpleTask = unimplemented!(); // einfach aber nicht dringend
    pub static ref GC_APOLLO: SimpleTask = unimplemented!(); // einfach aber nicht dringend
    pub static ref OPT_APOLLO: SimpleTask = unimplemented!(); // einfach aber nicht dringend
    pub static ref BACKUP_APOLLO: SimpleTask = unimplemented!(); // einfach aber nicht dringend
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
    fn definition(&self) -> &Task {
        &self.definition
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
    let daily = Timer::Repetition(CalendarRepeater {
        date: NaiveDate::from_ymd(2018, 5, 8),
        time: NaiveTime::from_hms(20, 0, 0),
        repeat: Interval::Day(1),
    });
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
        vec!["Reinige Toilette", "Zehennägel schneiden"],
        Timer::DeadTime(Duration::weeks(4)),
    ))
        .chain(simple_tasks(
            vec!["Friseurtermin machen"],
            Timer::DeadTime(Duration::weeks(6)),
        ))
        .chain(simple_tasks(vec!["Klavier üben"], daily))
        .chain(simple_tasks(vec!["Verbuche Kontoauszüge"], monthly))
        .chain(simple_tasks(
            vec![
                "Korrigiere Portemonnaiezählstand",
                "Block leeren und wegsortieren",
                "Leere Kiva Fächer",
                "Inbox zu Hause wegsortieren",
                "Cryptpads sichern",
            ],
            weekly,
        ))
        .collect::<Vec<_>>()
}

pub struct Inbox {
    timer: Timer,
}

impl WellKnown for Inbox {
    fn definition(&self) -> &Task {
        lazy_static! {
            static ref TASK: Task = {
                let mut t = TaskBuilder::default()
                    .description("Leere Inbox")
                    .build()
                    .expect("TaskBuilding failed inspite of set description");
                t.set_gen_name(Some("inbox"));
                t.set_gen_id(Some("inbox"));
                t
            };
        };
        &TASK
    }

    fn action_necessary(&self, cache: &TaskCache) -> Result<bool> {
        Ok(cache.filter(|t| task_in_inbox(&cache, t)).next().is_some())
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<()> {
        kassandra.clear_inbox()
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

pub struct Accounting {
    timer: Timer,
}

impl WellKnown for Accounting {
    fn definition(&self) -> &Task {
        lazy_static! {
            static ref TASK: Task = {
                let mut t = TaskBuilder::default()
                    .description("Aktualisiere Buchhaltung")
                    .build()
                    .expect("TaskBuilding failed inspite of set description");
                t.set_gen_name(Some("accounting"));
                t.set_gen_id(Some("accounting"));
                t
            };
        };
        &TASK
    }

    fn action_necessary(&self, _cache: &TaskCache) -> Result<bool> {
        Ok(true)
    }

    fn process(&self, _kassandra: &mut Kassandra) -> Result<()> {
        str2cmd(&term_cmd("sh -c"))
            .arg("jali -l. && task gen_id:accounting done")
            .output()?;
        Ok(())
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}

pub struct Treesort {
    timer: Timer,
}

impl WellKnown for Treesort {
    fn definition(&self) -> &Task {
        lazy_static! {
            static ref TASK: Task = {
                let mut t = TaskBuilder::default()
                    .description("Sortiere Tasktree")
                    .build()
                    .expect("TaskBuilding failed inspite of set description");
                t.set_gen_name(Some("treesort"));
                t.set_gen_id(Some("treesort"));
                t
            };
        };
        &TASK
    }

    fn action_necessary(&self, cache: &TaskCache) -> Result<bool> {
        Ok(
            cache
                .filter(|t| !t.obsolete() && needs_sorting(t))
                .next()
                .is_some(),
        )
    }

    fn process(&self, kassandra: &mut Kassandra) -> Result<()> {
        kassandra.assure_all_sorted()
    }

    fn refresh(&self) -> Timer {
        self.timer.clone()
    }
}
