extern crate task_hookrs;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate reqwest;
#[macro_use]
extern crate hyper;
#[macro_use]
extern crate error_chain;
extern crate config;

use reqwest::{IntoUrl, Client, RequestBuilder};
use task_hookrs::task::Task;
use serde_json::from_str;
use std::str::from_utf8;
use std::process::Command;
use std::thread::sleep;
use std::time::Duration;
use config::{Config, Environment};

error_chain! {
    foreign_links {
        Io(::std::io::Error);
    }
}

#[derive(Debug, Deserialize)]
struct Settings {
    user: String,
    key: String,
}

struct Habitask {
    client: Client,
    settings: Settings,
}

fn blink() {
    sleep(Duration::from_secs(1));
}


fn query(filter: String) -> Result<Vec<Task>> {
    let stdout = &Command::new("task")
        .arg("export")
        .arg(filter)
        .output()?
        .stdout;
    Ok(from_str(from_utf8(stdout)?)?)
}

#[derive(Deserialize)]
struct Item {
    pub id: String,
    pub text: String,
    pub completed: bool,
}

#[derive(Deserialize)]
struct Todo {
    pub id: String,
    pub checklist: Vec<Item>,
}

#[derive(Deserialize)]
struct Response {
    pub data: Todo,
}


impl Habitask {
    fn new() -> Result<Habitask> {
        let mut s = Config::new();
        s.merge(Environment::with_prefix("habitask"))?;
        let s = s.try_into()?;
        Habitask {
            client: Client::new(),
            settings: s,
        }
    }

    fn login(&self, mut builder: RequestBuilder) -> RequestBuilder {
        header! { (XApiUser, "x-api-user") => [String] }
        header! { (XApiKey, "x-api-key") => [String] }
        builder
            .header(XApiUser(self.settings.user.clone()))
            .header(XApiKey(self.settings.key.clone()));
        builder
    }

    fn post(&self, url: impl IntoUrl) -> RequestBuilder {
        self.login(self.client.post(url))
    }

    fn make_todo(&self, name: &str, prio: &str, tasks: Vec<Task>) -> Result<()> {
        if tasks.len() == 0 {
            return;
        }
        let Todo { id, .. } = self.create_todo(name, prio)?;
        let mut checklist: Vec<Item> = Vec::new();
        for task in tasks {
            checklist = self.add_item(&id, &task.description())?.checklist;
            blink();
        }
        for item in checklist {
            self.check_item(&id, &item.id)?;
            blink();
        }
        Ok(())
    }

    fn create_todo(&self, name: &str, prio: &str) -> Result<Todo> {
        let map = vec![("text", name), ("type", "todo"), ("priority", prio)];
        self.post("https://habitica.com/api/v3/tasks/user")
            .json(&map)
            .send()?
            .json::<Response>()?
            .data
    }

    fn add_item(&self, id: &str, item: &str) -> Result<Todo> {
        let map = vec![("text", item)];
        let url = format!("https://habitica.com/api/v3/tasks/{}/checklist", id);
        Ok(self.post(&url)
            .json(&map)
            .send()?
            .json::<Response>()?
            .data)
    }

    fn check_item(&self, id: &str, item: &str) -> Result<()>{
        let url = format!(
            "https://habitica.com/api/v3/tasks/{}/checklist/{}/score",
            id,
            item
        );
        self.post(&url)
            .json(&Vec::<(&str, &str)>::new())
            .send()?;
        Ok(())
    }

    fn score_task(&self, id: &str) -> Result<()>{
        let url = format!("https://habitica.com/api/v3/tasks/{}/score/up", id);
        self.post(&url)
            .json(&Vec::<(&str, &str)>::new())
            .send();
        Ok(())
    }
}

fn main() -> Result<()> {
    let new = "-DELETED entry.after:now-24h";
    let done = "+COMPLETED end.after:now-24h";
    let after = "entry.after:now-";
    let before = "entry.before:now-";
    let mask = "-auto";
    let instant_done = query(format!("{} -TAGGED {}48h {}", done, after, mask))?;
    let created = query(format!("{} {}", new, mask))?;
    let routines = query(format!("{} +auto", done))?;
    let tasks = query(format!("{} {}1week {}", done, after, mask))?;
    let a_little_old = query(format!("{} {}1month {}1week {}", done, after, before, mask))?;
    let old = query(format!(
        "{} {}3month {}1month {}",
        done,
        after,
        before,
        mask
    ))?;
    let very_old = query(format!("{} {}1year {}3month {}", done, after, before, mask))?;
    let crazy_old = query(format!("{} {}1year {}", done, before, mask))?;
    let habitask = Habitask::new()?;
    for _ in created {
        habitask.score_task("note")?;
        blink();
    }
    for _ in instant_done {
        habitask.score_task("do-it")?;
        blink();
    }
    habitask.make_todo("Routinen erledigt!", "0.1", routines)?;
    habitask.make_todo("Herausforderungen bezwungen!", "1", tasks)?;
    habitask.make_todo(
        "Ein paar Tage alte Herausforderungen bezwungen!",
        "1.5",
        a_little_old,
    )?;
    habitask.make_todo("Einige Wochen alte Herausforderungen bezwungen!", "2", old)?;
    habitask.make_todo("Monate alte Herausforderungen bezwungen!", "2", very_old)?;
    habitask.make_todo("Uralte Herausforderungen bezwungen!", "2", crazy_old)?;
    Ok(())
}
