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


fn query(filter: String) -> Vec<task_hookrs::task::Task> {
    let stdout = &Command::new("task")
        .arg("export")
        .arg(filter)
        .output()
        .unwrap()
        .stdout;
    let string = std::str::from_utf8(stdout).unwrap();
    from_str::<Vec<Task>>(string).unwrap()
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
        s.merge(Environment::with_prefix("habatisk"));
        let s: Settings = s.try_into().unwrap();
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
        let mut map = std::collections::HashMap::new();
        map.insert("text", name);
        map.insert("type", "todo");
        map.insert("priority", prio);
        let mut res = self.post("https://habitica.com/api/v3/tasks/user")
            .json(&map)
            .send()
            .unwrap();
        println!("{:?}", res);
        let Response { data } = res.json().unwrap();
        data
    }

    fn add_item(&self, id: &str, item: &str) -> Todo {
        let mut map = std::collections::HashMap::new();
        map.insert("text", item);
        let mut res = self.post(
            &("https://habitica.com/api/v3/tasks/".to_owned() + id + "/checklist"),
        ).json(&map)
            .send()
            .unwrap();
        println!("{:?}", res);
        let Response { data } = res.json().unwrap();
        data
    }

    fn check_item(&self, id: &str, item: &str) {
        let map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        let res = self.post(
            &("https://habitica.com/api/v3/tasks/".to_owned() + id + "/checklist/" +
                  item + "/score"),
        ).json(&map)
            .send()
            .unwrap();
        println!("{:?}", res);
    }
    fn score_task(&self, id: &str) {
        let client = Client::new();
        let map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        let res = self.post(
            &("https://habitica.com/api/v3/tasks/".to_owned() + id + "/score/up"),
        ).json(&map)
            .send()
            .unwrap();
        println!("{:?}", res);
    }
}

fn main() {
    let habitask = Habitask::new();
    let new = "-DELETED entry.after:now-24h";
    let done = "+COMPLETED end.after:now-24h";
    let after = "entry.after:new-";
    let before = "entry.before:new-";
    let mask = "-auto";
    let habitask = Habitask::new();
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
    println!(
        "{},{},{},{},{},{},{},{}",
        tasks.len(),
        routines.len(),
        instant_done.len(),
        created.len(),
        a_little_old.len(),
        old.len(),
        very_old.len(),
        crazy_old.len()
    );
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
