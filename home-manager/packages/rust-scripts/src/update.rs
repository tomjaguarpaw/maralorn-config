use task_hookrs::cache::TaskCache;

use refresh::TaskRefresher;
use tasktree::{TreeCache, TaskNode};
use well_known::{SIMPLE, WellKnown, INBOX, ACCOUNTING, TREESORT};
use error::Result;
use kassandra::Kassandra;

fn check_completion(cache: &mut TaskCache, task: &impl WellKnown) -> Result<()> {
    if !task.action_necessary(cache)? {
        let uuid = cache
            .filter(|t| task.is_this(t) && t.pending())
            .map(|t| t.uuid().clone())
            .next();
        if let Some(uuid) = uuid {
            cache.get_mut(&uuid).unwrap().tw_done();
        }
    }
    Ok(())
}

fn is_active(cache: &TaskCache, task: &impl WellKnown) -> bool {
    cache
        .filter(|t| task.is_this(t) && t.pending())
        .next()
        .is_some()
}

fn update_task(cache: &mut TaskCache, task: &impl WellKnown) -> Result<()> {
    cache.reactivate(
        Some(task.definition().clone()),
        task.refresh(),
    )?;
    check_completion(cache, task)
}

pub fn process_task(kassandra: &mut Kassandra, task: &impl WellKnown) -> Result<()> {
    update_task(&mut kassandra.cache, task)?;
    if is_active(&mut kassandra.cache, task) {
        kassandra.cache.write()?;
        task.process(kassandra)?;
        kassandra.cache.refresh()?;
        update_task(&mut kassandra.cache, task)?;
    }
    kassandra.cache.write()?;
    Ok(())
}

pub fn update_tasks(cache: &mut TaskCache) -> Result<()> {
    for simple_task in SIMPLE.iter() {
        update_task(cache, simple_task)?;
    }
    update_task(cache, &*INBOX)?;
    update_task(cache, &*TREESORT)?;
    update_task(cache, &*ACCOUNTING)?;
    cache.refresh_tree();
    // CREATE TODOS FROM MAIL
    // FROM GITLAB
    // FROM OTRS
    // FROM GITHUBgT
    Ok(())
}
