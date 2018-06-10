use task_hookrs::task::{Task, TaskBuilder};
use task_hookrs::cache::TaskCache;

use error::Result;
use kassandra::State;
use generate::{gen_match, GeneratedTask};
use refresh::{Timer, CalendarRepeater, Interval};

use chrono::{NaiveDate, NaiveTime, Duration};

pub trait WellKnown {
    fn definition(&self) -> &Task;
    fn is_this(&self, task: &Task) -> bool {
        gen_match(task, self.definition())
    }
    fn action_necessary(&self, &TaskCache, State) -> Result<bool> {
        Ok(true)
    }
    fn process(&self, &mut TaskCache, State) -> Result<()> {
        Ok(())
    }
    fn refresh(&self) -> Timer;
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

fn simple_tasks<'a>(
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
        .chain(simple_tasks(
            vec![
                "Aktualisiere Buchhaltung",
                "Leere Inbox",
                "Sortiere Tasktree",
                "Sortiere Inbox",
                "Sortiere Inbox Auslandskoordination",
                "Sortiere Inbox Kiva",
                "Klavier üben",
                "Tasks der Woche kontrollieren",
            ],
            daily,
        ))
        .chain(simple_tasks(
            vec!["Kontrolliere +optional", "Verbuche Kontoauszüge"],
            monthly,
        ))
        .chain(simple_tasks(
            vec![
                "Kontrolliere Spam",
                "Korrigiere Portemonnaiezählstand",
                "Sortiere Archiv",
                "Sortiere Archiv Kiva",
                "Sortiere Archiv Auslandskoordination",
                "Kontrolliere +later",
                "Kontrolliere +await",
                "Block leeren und wegsortieren",
                "Leere Kiva Fächer",
                "Inbox zu Hause wegsortieren",
                "Cryptpads sichern",
                "Update nixos apollo",
                "Update home hephaistos",
                "Tasks des Monats kontrollieren",
            ],
            weekly,
        ))
        .collect::<Vec<_>>()
}

lazy_static! {
    pub static ref SIMPLE: Vec<SimpleTask> = make_simple();
}
