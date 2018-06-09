use std::collections::HashSet;

use uuid::Uuid;

use task_hookrs::task::Task;
use task_hookrs::status::TaskStatus as TS;
use task_hookrs::cache::TaskCache;
use task_hookrs::uda::UDAValue;

use error::{Result, ResultExt};

pub trait TaskNode {
    fn partof(&self) -> Result<Option<Uuid>>;
    fn set_partof(&mut self, Option<Uuid>);
    fn add_tag<T: Into<String>>(&mut self, T);
    fn remove_tag<T: Into<String>>(&mut self, T);
    fn has_tag<T: Into<String>>(&self, T) -> bool;
    fn tagged(&self) -> bool;
    fn pending(&self) -> bool;
    fn obsolete(&self) -> bool;
}

impl TaskNode for Task {
    fn partof(&self) -> Result<Option<Uuid>> {
        Ok(if let Some(uuid) = self.uda().get("partof".into()) {
            if let &UDAValue::Str(ref uuid) = uuid {
                Some(Uuid::parse_str(uuid).chain_err(|| "No uuid in partof uda")?)
            } else {
                Err("No String in partof uda")?
            }
        } else {
            None
        })
    }

    fn set_partof(&mut self, partof: Option<Uuid>) {
        if let Some(uuid) = partof {
            self.uda_mut().insert(
                "partof".into(),
                UDAValue::Str(uuid.to_string()),
            );
        } else {
            self.uda_mut().remove("partof".into());
        }
    }

    fn add_tag<T: Into<String>>(&mut self, tag: T) {
        let tag = tag.into();
        if !self.has_tag(tag.clone()) {
            if self.tags().is_some() {
                self.tags_mut().unwrap().push(tag);
            } else {
                self.set_tags(Some(Some(tag)));
            }
        }
    }

    fn remove_tag<T: Into<String>>(&mut self, tag: T) {
        if let Some(tags) = self.tags_mut() {
            let tag = tag.into();
            tags.retain(|t| *t != tag);
        }
    }

    fn has_tag<T: Into<String>>(&self, tag: T) -> bool {
        self.tags()
            .map(|tags| tags.contains(&tag.into()))
            .unwrap_or(false)
    }

    fn tagged(&self) -> bool {
        self.tags().map(|t| t.len() > 0).unwrap_or(false)
    }

    fn pending(&self) -> bool {
        *self.status() == TS::Pending
    }

    fn obsolete(&self) -> bool {
        *self.status() == TS::Completed || *self.status() == TS::Deleted
    }
}

pub trait TreeCache {
    fn get_parent(&self, uuid: &Uuid) -> Result<Option<&Task>>;
    fn get_parent_mut(&mut self, uuid: &Uuid) -> Result<Option<&mut Task>>;
    fn get_children(&self, uuid: &Uuid) -> Vec<&Task>;
    fn get_children_mut(&mut self, uuid: &Uuid) -> Vec<&mut Task>;
    fn get_project_path(&self, uuid: &Uuid) -> Result<String>;
    fn refresh_tree(&mut self);
}
fn get_project_name(cache: &TaskCache, task: &Task) -> Result<Option<String>> {
    Ok(if let Some(parent_uuid) = task.partof()? {
        Some(cache.get_project_path(&parent_uuid)?)
    } else {
        None
    })
}

impl TreeCache for TaskCache {
    fn get_parent(&self, uuid: &Uuid) -> Result<Option<&Task>> {
        let task = self.get(uuid).chain_err(|| "task uuid not found")?;
        Ok(if let Some(uuid) = task.partof()? {
            Some(self.get(&uuid).chain_err(|| "parent uuid not found")?)
        } else {
            None
        })
    }

    fn get_parent_mut(&mut self, uuid: &Uuid) -> Result<Option<&mut Task>> {
        let parent_option = self.get(uuid).chain_err(|| "task uuid not found")?.partof()?;
        Ok(if let Some(uuid) = parent_option {
            Some(self.get_mut(&uuid).chain_err(|| "parent uuid not found")?)
        } else {
            None
        })
    }

    fn get_children(&self, uuid: &Uuid) -> Vec<&Task> {
        self.filter(|t| t.partof().unwrap_or(None) == Some(*uuid))
            .collect()
    }

    fn get_children_mut(&mut self, uuid: &Uuid) -> Vec<&mut Task> {
        self.filter_mut(|t| t.partof().unwrap_or(None) == Some(*uuid))
            .collect()
    }

    fn get_project_path(&self, uuid: &Uuid) -> Result<String> {
        let task = self.get(uuid).chain_err(|| "Uuid not found in Cache")?;
        let end = task.description().to_lowercase().replace(
            char::is_whitespace,
            "",
        );
        Ok(if let Some(parent) = task.partof()? {
            format!("{}.{}", self.get_project_path(&parent)?, end)
        } else {
            end
        })
    }

    fn refresh_tree(&mut self) {
        let parents = self.filter(|t| !t.obsolete())
            .filter_map(|t| t.partof().unwrap_or(None))
            .collect::<HashSet<_>>();
        let mut add = vec![];
        let mut remove = vec![];
        let mut set = vec![];

        for (project_name, project, tagged, uuid) in
            self.filter(|t| !t.obsolete()).map(|t| {
                (
                    get_project_name(self, t),
                    t.project(),
                    t.has_tag("project"),
                    t.uuid().clone(),
                )
            })
        {
            if parents.contains(&uuid) {
                if !tagged {
                    add.push(uuid);
                }
            } else {
                if tagged {
                    remove.push(uuid);
                }
            }
            if let Ok(project_name) = project_name {
                if project != project_name.as_ref() {
                    set.push((uuid, project_name))
                }
            }
        }

        for uuid in add {
            self.get_mut(&uuid).expect("Bug").add_tag("project");
        }
        for uuid in remove {
            self.get_mut(&uuid).expect("Bug").remove_tag("project");
        }
        for (uuid, name) in set {
            self.get_mut(&uuid).expect("Bug").set_project(name);
        }
    }
}
