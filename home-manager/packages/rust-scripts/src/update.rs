use task_hookrs::error::Result;
use task_hookrs::cache::TaskCache;

use refresh::TaskRefresher;
use tasktree::TreeCache;
use well_known::{SIMPLE, WellKnown};

pub fn update_tasks(cache: &mut TaskCache) -> Result<()> {
    for simple_task in SIMPLE.iter() {
        cache.reactivate(
            Some(simple_task.definition().clone()),
            simple_task.refresh(),
        )?;
    }
    cache.refresh_tree();
    Ok(())
}
