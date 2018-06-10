use chrono::offset::{Local, TimeZone};
use chrono::{NaiveDate, Duration, NaiveTime, Datelike, NaiveDateTime};

use task_hookrs::status::TaskStatus as TS;
use task_hookrs::task::Task;
use task_hookrs::cache::TaskCache;
use task_hookrs::error::Result;
use task_hookrs::date::Date;

use generate::TaskGenerator;
use tasktree::TaskNode;

#[derive(Copy, Debug, Clone, Eq, PartialEq)]
pub enum Timer {
    DeadTime(Duration),
    Repetition(CalendarRepeater),
}

#[derive(Copy, Debug, Clone, Eq, PartialEq)]
pub enum Interval {
    Year(i32),
    Month(u32),
    Day(i32),
}

#[derive(Copy, Debug, Clone, Eq, PartialEq)]
pub struct CalendarRepeater {
    pub time: NaiveTime,
    pub date: NaiveDate,
    pub repeat: Interval,
}

impl Iterator for CalendarRepeater {
    type Item = NaiveDateTime;
    fn next(&mut self) -> Option<NaiveDateTime> {
        let ret = Some(self.date.clone().and_time(self.time));
        self.date = match self.repeat {
            Interval::Year(year) => {
                NaiveDate::from_ymd(self.date.year() + year, self.date.month(), self.date.day())
            }
            Interval::Month(month) => {
                NaiveDate::from_ymd(
                    self.date.year() + ((self.date.month() + month - 1) / 12) as i32,
                    1 + ((self.date.month() + month - 1) % 12),
                    self.date.day(),
                )
            }
            Interval::Day(day) => NaiveDate::from_num_days_from_ce(
                self.date.num_days_from_ce() + day,
            ),
        };
        ret
    }
}

pub trait TaskRefresher {
    fn reactivate(
        &mut self,
        tasks: impl IntoIterator<Item = Task>,
        recurrence: Timer,
    ) -> Result<()>;
}

impl TaskRefresher for TaskCache {
    fn reactivate(
        &mut self,
        tasks: impl IntoIterator<Item = Task>,
        recurrence: Timer,
    ) -> Result<()> {
        let now = Local::now();
        let recent = match recurrence {
            Timer::DeadTime(time) => (now - time).naive_local(),
            Timer::Repetition(iter) => {
                iter.take_while(|t| *t <= now.naive_local())
                    .last()
                    .ok_or("Repetition starts in the future")?
                    .clone()
            }
        };
        let mut uuids = vec![];
        let changes = tasks
            .into_iter()
            .filter(|task| if let Some(old) = self.get_by_gen(&task) {
                if old.obsolete() &&
                    Local
                        .from_utc_datetime(
                            &**old.end().expect("Ended tasks have to have an end date"),
                        )
                        .naive_local() < recent
                {
                    uuids.push(old.uuid().clone());
                }
                false
            } else {
                true
            })
            .collect::<Vec<_>>();
        for uuid in uuids {
            let old = self.get_mut(&uuid).expect("BUG!");
            *old.status_mut() = TS::Pending;
            old.set_end(None as Option<Date>);
        }
        self.generate(changes)
    }
}
