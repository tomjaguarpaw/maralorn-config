extern crate rust_scripts;
extern crate task_hookrs;

use rust_scripts::error::Result;
use rust_scripts::otrs::sync_otrs;
use task_hookrs::cache::TaskCache;
use task_hookrs::status::TaskStatus;

fn main() -> Result<()> {
    let mut cache = TaskCache::new(vec![TaskStatus::Deleted]);
    cache.load()?;
    sync_otrs(&mut cache)?;
    cache.write()?;
    Ok(())
}
