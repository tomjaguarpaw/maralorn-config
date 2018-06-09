use chrono::offset::{Local, TimeZone};
use chrono::Duration;

use kairos::timetype::TimeType as TT;
use kairos::iter::Iter;
use kairos::error::Result as KairosResult;

use task_hookrs::status::TaskStatus as TS;
use task_hookrs::task::Task;
use task_hookrs::cache::TaskCache;
use task_hookrs::error::Result;
use task_hookrs::date::Date;

use generate::TaskGenerator;
use tasktree::TaskNode;

pub enum Timer {
    DeadTime(Duration),
    Repetition(Iter),
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
        let now_moment = TT::Moment(Local::now().naive_local());
        let recent = match recurrence {
            Timer::DeadTime(time) => TT::Moment((now - time).naive_local()),
            Timer::Repetition(iter) => {
                iter.filter_map(KairosResult::ok)
                    .take_while(|t| *t <= now_moment)
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
                    TT::Moment(
                        Local
                            .from_utc_datetime(
                                &**old.end().expect("Ended tasks have to have an end date"),
                            )
                            .naive_local(),
                    ) < recent
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
