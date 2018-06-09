use task_hookrs::error::Result;
use task_hookrs::cache::TaskCache;
use task_hookrs::task::{Task, TaskBuilder};
use generate::GeneratedTask;
use refresh::{TaskRefresher, Timer};
use tasktree::TreeCache;
use chrono::NaiveDate;
use chrono::Duration;
use kairos::timetype::TimeType as TT;
use kairos::iter::extensions::{Weekly, Monthly, Daily};

fn simple_task(name: &str) -> Task {
    let mut task = TaskBuilder::default().description(name).build().expect(
        "TaskBuilding failed inspite of set description",
    );
    task.set_gen_name(Some(name));
    task.set_gen_id(Some(name));
    task
}

fn simple_tasks<'a, T>(names: T) -> Vec<Task>
where
    T: IntoIterator<Item = &'a str>,
{
    names.into_iter().map(simple_task).collect()
}

pub fn update_tasks(cache: &mut TaskCache) -> Result<()> {
    let daily = || {
        Timer::Repetition(
            TT::moment(NaiveDate::from_ymd(2018, 5, 8).and_hms(20, 0, 0))
                .daily(1)
                .unwrap(),
        )
    };
    let weekly = || {
        Timer::Repetition(
            TT::moment(NaiveDate::from_ymd(2018, 5, 8).and_hms(20, 0, 0))
                .weekly(1)
                .unwrap(),
        )
    };
    let monthly = || {
        Timer::Repetition(
            TT::moment(NaiveDate::from_ymd(2018, 5, 3).and_hms(20, 0, 0))
                .monthly(1)
                .unwrap(),
        )
    };
    cache.reactivate(
        simple_tasks(vec![
            "Staubsaugen",
            "Putze Waschbecken",
            "Wäsche sortieren und entscheiden, welche Waschgänge notwendig sind",
        ]),
        Timer::DeadTime(Duration::weeks(2)),
    )?;
    cache.reactivate(
        simple_tasks(
            vec!["Reinige Toilette", "Zehennägel schneiden"],
        ),
        Timer::DeadTime(Duration::weeks(4)),
    )?;
    cache.reactivate(
        simple_tasks(vec!["Friseurtermin machen"]),
        Timer::DeadTime(Duration::weeks(6)),
    )?;
    cache.reactivate(
        simple_tasks(vec![
            "Aktualisiere Buchhaltung",
            "Leere Inbox",
            "Sortiere Tasktree",
            "Sortiere Inbox",
            "Sortiere Inbox Auslandskoordination",
            "Sortiere Inbox Kiva",
            "Update nixos apollo",
            "Update home hephaistos",
            "Klavier üben",
        ]),
        daily(),
    )?;
    cache.reactivate(
        simple_tasks(vec!["Verbuche Kontoauszüge"]),
        monthly(),
    )?;
    cache.reactivate(
        simple_tasks(vec![
            "Kontrolliere Spam",
            "Korrigiere Portemonnaiezählstand",
            "Sortiere Archiv",
            "Sortiere Archiv Kiva",
            "Sortiere Archiv Auslandskoordination",
            "Kontrolliere +later",
            "Kontrolliere +optional",
            "Kontrolliere +await",
            "Block leeren und wegsortieren",
            "Leere Kiva Fächer",
            "Inbox zu Hause wegsortieren",
        ]),
        weekly(),
    )?;
    cache.refresh_tree();
    Ok(())
}
