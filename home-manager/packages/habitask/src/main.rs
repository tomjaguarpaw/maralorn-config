extern crate task_hookrs;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate reqwest;
#[macro_use]
extern crate hyper;

extern crate config;

use reqwest::{IntoUrl, Client, RequestBuilder};
use task_hookrs::task::Task;
use serde_json::from_str;
use std::str::from_utf8;
use std::process::Command;
use std::thread::sleep;
use std::time::Duration;
use config::{Config, Environment};

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


fn query(filter: String) -> Vec<Task> {
    let stdout = &Command::new("task")
        .arg("export")
        .arg(filter)
        .output()
        .unwrap()
        .stdout;
    from_str(from_utf8(stdout).unwrap()).unwrap()
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
    fn new() -> Habitask {
        let mut s = Config::new();
        s.merge(Environment::with_prefix("habitask")).unwrap();
        let s = s.try_into().unwrap();
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

    fn make_todo(&self, name: &str, prio: &str, tasks: Vec<Task>) {
        if tasks.len() == 0 {
            return;
        }
        let Todo { id, .. } = self.create_todo(name, prio);
        let mut checklist: Vec<Item> = Vec::new();
        for task in tasks {
            checklist = self.add_item(&id, &task.description()).checklist;
            blink();
        }
        for item in checklist {
            self.check_item(&id, &item.id);
            blink();
        }
    }

    fn create_todo(&self, name: &str, prio: &str) -> Todo {
        let map = vec![("text", name), ("type", "todo"), ("priority", prio)];
        self.post("https://habitica.com/api/v3/tasks/user")
            .json(&map)
            .send()
            .unwrap()
            .json::<Response>()
            .unwrap()
            .data
    }

    fn add_item(&self, id: &str, item: &str) -> Todo {
        let map = vec![("text", item)];
        let url = format!("https://habitica.com/api/v3/tasks/{}/checklist", id);
        self.post(&url)
            .json(&map)
            .send()
            .unwrap()
            .json::<Response>()
            .unwrap()
            .data
    }

    fn check_item(&self, id: &str, item: &str) {
        let url = format!(
            "https://habitica.com/api/v3/tasks/{}/checklist/{}/score",
            id,
            item
        );
        self.post(&url)
            .json(&Vec::<(&str, &str)>::new())
            .send()
            .unwrap();
    }

    fn score_task(&self, id: &str) {
        let url = format!("https://habitica.com/api/v3/tasks/{}/score/up", id);
        self.post(&url)
            .json(&Vec::<(&str, &str)>::new())
            .send()
            .unwrap();
    }
}

fn main() {
    let new = "-DELETED entry.after:now-24h";
    let done = "+COMPLETED end.after:now-24h";
    let after = "entry.after:now-";
    let before = "entry.before:now-";
    let mask = "-auto";
    let instant_done = query(format!("{} -TAGGED {}48h {}", done, after, mask));
    let created = query(format!("{} {}", new, mask));
    let routines = query(format!("{} +auto", done));
    let tasks = query(format!("{} {}1week {}", done, after, mask));
    let a_little_old = query(format!("{} {}1month {}1week {}", done, after, before, mask));
    let old = query(format!(
        "{} {}3month {}1month {}",
        done,
        after,
        before,
        mask
    ));
    let very_old = query(format!("{} {}1year {}3month {}", done, after, before, mask));
    let crazy_old = query(format!("{} {}1year {}", done, before, mask));
    let habitask = Habitask::new();
    for _ in created {
        habitask.score_task("note");
        blink();
    }
    for _ in instant_done {
        habitask.score_task("do-it");
        blink();
    }
    habitask.make_todo("Routinen erledigt!", "1", routines);
    habitask.make_todo("Herausforderungen bezwungen!", "1", tasks);
    habitask.make_todo(
        "Ein paar Tage alte Herausforderungen bezwungen!",
        "1.5",
        a_little_old,
    );
    habitask.make_todo("Einige Wochen alte Herausforderungen bezwungen!", "2", old);
    habitask.make_todo("Monate alte Herausforderungen bezwungen!", "2", very_old);
    habitask.make_todo("Uralte Herausforderungen bezwungen!", "2", crazy_old);
}
