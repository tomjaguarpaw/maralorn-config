//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//

//! This module contains the generate function and the GeneratedTask trait.
//! You can routinely generate GeneratedTasks which represent some external state,
//! like e-mails you need to read, tickets from an issue tracker, or updates you have to do on a
//! system. If you pass them to the generate function, it will update your taskwarrior accordingly.
//! Create new tasks, complete old ones and update once which have new information.
//! Fields which you don't set in a GeneratedTask can be set in taskwarrior and won't be
//! overwritten on calling generate again.
use task_hookrs::task::Task;
use task_hookrs::status::TaskStatus as TS;
use task_hookrs::uda::UDAValue as U;
use generate::OrphanBehavior as O;
use task_hookrs::cache::TaskCache;
use task_hookrs::error::{Result, ResultExt};
use std::collections::HashMap;
use uuid::Uuid;


/// This trait contains additional methods for the Task struct, for representing programatically
/// generated tasks
pub trait GeneratedTask {
    /// The name of this generator type. e.g. "mail"
    fn gen_name(&self) -> Option<&String>;
    /// The name of this generator type. e.g. "mail", mutable
    fn gen_name_mut(&mut self) -> Option<&mut String>;
    /// Set the name of this generator type. e.g. "mail"
    fn set_gen_name<T>(&mut self, new: Option<T>)
    where
        T: Into<String>;
    /// The id of the generator type, which should be unique for all generator Tasks with the same
    /// name
    fn gen_id(&self) -> Option<&String>;
    /// The generator id, mutable
    fn gen_id_mut(&mut self) -> Option<&mut String>;
    /// Set the generator id
    fn set_gen_id<T>(&mut self, new: Option<T>)
    where
        T: Into<String>;
    /// What should happen with the task, when the generator is missing.
    /// This is relevant, when the generate method is called with one or more tasks with a
    /// generator name set, all existing tasks with that generator name will be dealt with
    /// according to this field.
    fn gen_orphan(&self) -> OrphanBehavior;
    /// Set the OrphanBehavior
    fn set_gen_orphan<T>(&mut self, new: OrphanBehavior);
}

lazy_static! {
    static ref DELETE: U = U::Str("delete".into());
    static ref COMPLETE: U = U::Str("complete".into());
    static ref EMPTY: U = U::Str(String::default());
}
static GEN_NAME: &str = "gen_name";
static GEN_ID: &str = "gen_id";
static GEN_ORPHAN: &str = "gen_orphan";

/// The behavior if a task with missing generator is encountered.
pub enum OrphanBehavior {
    /// Complete generated tasks, with missing generator
    CompleteOrphan,
    /// Delete generated tasks, with missing generator
    DeleteOrphan,
    /// Don't do anything with generated tasks, with missing generator
    KeepOrphan,
}

impl GeneratedTask for Task {
    fn gen_name(&self) -> Option<&String> {
        self.uda().get(GEN_NAME).and_then(
            |x| if let &U::Str(ref x) = x {
                Some(x)
            } else {
                None
            },
        )
    }

    fn gen_name_mut(&mut self) -> Option<&mut String> {
        self.uda_mut().get_mut(GEN_NAME).and_then(|x| {
            if let &mut U::Str(ref mut x) = x {
                Some(x)
            } else {
                None
            }
        })
    }

    fn set_gen_name<T>(&mut self, new: Option<T>)
    where
        T: Into<String>,
    {
        if let Some(new) = new {
            self.uda_mut().insert(GEN_NAME.into(), U::Str(new.into()));
        } else {
            self.uda_mut().remove(GEN_NAME);
        }
    }

    fn gen_id(&self) -> Option<&String> {
        self.uda().get(GEN_ID).and_then(
            |x| if let &U::Str(ref x) = x {
                Some(x)
            } else {
                None
            },
        )
    }

    fn gen_id_mut(&mut self) -> Option<&mut String> {
        self.uda_mut().get_mut(GEN_ID).and_then(|x| {
            if let &mut U::Str(ref mut x) = x {
                Some(x)
            } else {
                None
            }
        })
    }

    fn set_gen_id<T>(&mut self, new: Option<T>)
    where
        T: Into<String>,
    {
        if let Some(new) = new {
            self.uda_mut().insert(GEN_ID.into(), U::Str(new.into()));
        } else {
            self.uda_mut().remove(GEN_ID);
        }
    }

    fn gen_orphan(&self) -> OrphanBehavior {
        let u = self.uda().get(GEN_ORPHAN).unwrap_or(&*EMPTY);
        if *u == *COMPLETE {
            O::CompleteOrphan
        } else if *u == *DELETE {
            O::DeleteOrphan
        } else {
            O::KeepOrphan
        }
    }

    fn set_gen_orphan<T>(&mut self, new: OrphanBehavior) {
        match new {
            O::DeleteOrphan => self.uda_mut().insert(GEN_ORPHAN.into(), DELETE.clone()),
            O::CompleteOrphan => self.uda_mut().insert(GEN_ORPHAN.into(), COMPLETE.clone()),
            O::KeepOrphan => self.uda_mut().remove(GEN_ORPHAN),
        };
    }
}

fn check_ignores(cache: &TaskCache) -> Result<()> {
    if cache.ignore().len() > 0 {
        return Err("Don't use generate with a TaskCache with ignores".into());
    };
    Ok(())
}

fn process_new(cache: &mut TaskCache, new: Vec<Task>) {
    for task in new {
        cache.set(task);
    }
}

fn r<'a>(c: &'a TaskCache, u: &Uuid) -> Result<&'a Task> {
    c.get(u).chain_err(|| "Cache miss for changed task")
}

fn w<'a>(c: &'a mut TaskCache, u: &Uuid) -> Result<&'a mut Task> {
    c.get_mut(u).chain_err(|| "Cache miss for changed task")
}

fn process_matches(cache: &mut TaskCache, matches: Vec<(Uuid, Task)>) -> Result<()> {
    let c = cache;
    for (u, new) in matches {
        if r(c,&u)?.status() != new.status() {
            *w(c,&u)?.status_mut() = new.status().clone();
        }
        if r(c,&u)?.description() != new.description() {
            *w(c,&u)?.description_mut() = new.description().clone();
        }
        if let Some(ann) = new.annotations() {
            if r(c,&u)?.annotations().map(|x| x != ann) == Some(true) {
                w(c,&u)?.annotations_mut().map(|o| *o = ann.clone());
            }
        }
        if let Some(dep) = new.depends() {
            if r(c,&u)?.depends().map(|x| x != dep) == Some(true) {
                w(c,&u)?.depends_mut().map(|o| *o = dep.clone());
            }
        }
        if let Some(due) = new.due() {
            if r(c,&u)?.due().map(|x| x != due) == Some(true) {
                w(c,&u)?.due_mut().map(|o| *o = due.clone());
            }
        }
        if let Some(end) = new.end() {
            if r(c,&u)?.end().map(|x| x != end) == Some(true) {
                w(c,&u)?.end_mut().map(|o| *o = end.clone());
            }
        }
        if let Some(ann) = new.annotations() {
            if r(c,&u)?.annotations().map(|x| x != ann) == Some(true) {
                w(c,&u)?.annotations_mut().map(|o| *o = ann.clone());
            }
        }
        if let Some(recur) = new.recur() {
            if r(c,&u)?.recur().map(|x| x != recur) == Some(true) {
                w(c,&u)?.recur_mut().map(|o| *o = recur.clone());
            }
        }
        if let Some(scheduled) = new.scheduled() {
            if r(c,&u)?.scheduled().map(|x| x != scheduled) == Some(true) {
                w(c,&u)?.scheduled_mut().map(|o| *o = scheduled.clone());
            }
        }
        if let Some(start) = new.start() {
            if r(c,&u)?.start().map(|x| x != start) == Some(true) {
                w(c,&u)?.start_mut().map(|o| *o = start.clone());
            }
        }
        if let Some(tags) = new.tags() {
            if r(c,&u)?.tags().map(|x| x != tags) == Some(true) {
                w(c,&u)?.tags_mut().map(|o| *o = tags.clone());
            }
        }
        if let Some(until) = new.until() {
            if r(c,&u)?.until().map(|x| x != until) == Some(true) {
                w(c,&u)?.until_mut().map(|o| *o = until.clone());
            }
        }
        if let Some(wait) = new.wait() {
            if r(c,&u)?.wait().map(|x| x != wait) == Some(true) {
                w(c,&u)?.wait_mut().map(|o| *o = wait.clone());
            }
        }
        if !(r(c,&u)?.uda() >= new.uda()) {
            w(c,&u)?.uda_mut().append(&mut new.uda().clone());
        }
    }
    Ok(())
}

fn process_orphans(
    cache: &mut TaskCache,
    orphans: HashMap<String, HashMap<String, Uuid>>,
) -> Result<()> {
    for uuid in orphans.values().flat_map(HashMap::values) {
        match cache.get(uuid).chain_err(|| "Cache miss for orphan")?.gen_orphan() {
            O::CompleteOrphan => if *cache.get(uuid).chain_err(|| "Cache miss for orphan")?.status() != TS::Completed {
                *cache.get_mut(uuid).chain_err(|| "Cache miss for orphan")?.status_mut() = TS::Completed;
            }
            O::DeleteOrphan => if *cache.get(uuid).chain_err(|| "Cache miss for orphan")?.status() != TS::Deleted {
                *cache.get_mut(uuid).chain_err(|| "Cache miss for orphan")?.status_mut() = TS::Deleted;
            }
            _ => ()
            }
    }
    Ok(())
}

/// Take a TaskCache (which should not ignore any task states) and make sure, that all given
/// generator tasks are represented in taskwarrior.

pub trait TaskGenerator {
    fn get_by_gen(&self, generator: &Task) -> Option<&Task>;
    fn get_by_gen_mut(&mut self, generator: &Task) -> Option<&mut Task>;
    fn generate<T>(&mut self, generators: T) -> Result<()>
where
    T: IntoIterator,
    T::Item: Into<Task>;
}

fn gen_match(a: &Task, b: &Task) -> bool {
    a.gen_name() == b.gen_name() && a.gen_id() == b.gen_id()
}
impl TaskGenerator for TaskCache {
    fn get_by_gen(&self, generator: &Task) -> Option<&Task> {
        self.filter(|t| gen_match(t, generator)).next()
    }
    fn get_by_gen_mut(&mut self, generator: &Task) -> Option<&mut Task> {
        self.filter_mut(|t| gen_match(t, generator)).next()
    }

fn generate<T>(&mut self, generators: T) -> Result<()>
where
    T: IntoIterator,
    T::Item: Into<Task>,
{
    check_ignores(self)?;
    let mut orphans = HashMap::<String, HashMap<String, Uuid>>::default();
    let mut create = Vec::<Task>::default();
    let mut changes = Vec::<(Uuid, Task)>::default();
    for g in generators {
        let new = g.into();
        if let Some(old) = {
            let name = new.gen_name().chain_err(|| "gen_name missing")?;
            let id = new.gen_id().chain_err(|| "gen_id missing")?;
                orphans
                    .entry(name.clone())
                    .or_insert_with(|| {
                        self
                            .filter(|t| t.gen_name() == Some(name))
                            .filter_map(|t| t.gen_id().map(|id| (id.clone(), t.uuid().clone())))
                            .collect()
                    })
                    .remove(id)
        }
        {
            changes.push((old, new));
        } else {
            create.push(new);
        }
    }
    process_new(self, create);
    process_matches(self, changes)?;
    process_orphans(self, orphans)
}
}
