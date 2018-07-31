use task_hookrs::task::TaskBuilder;
use task_hookrs::cache::TaskCache;

use error::{Result, ResultExt};
use generate::{GeneratedTask, TaskGenerator, OrphanBehavior};
use reqwest::Client;
use config::{Config, Environment};

#[derive(Debug, Deserialize)]
struct Ticket {
    #[serde(rename = "TicketID")]
    number: u32,
    #[serde(rename = "Title")]
    title: String,
    #[serde(rename = "CustomerUserID")]
    customer: String,
}

#[derive(Debug, Clone, Deserialize)]
struct Settings {
    user: String,
    password: String,
    queues: String,
    owners: String,
    host: String,
}

#[derive(Debug, Serialize)]
struct QueueRequest {
    #[serde(rename = "UserLogin")]
    user: String,
    #[serde(rename = "Password")]
    password: String,
    #[serde(rename = "Queues")]
    queues: Vec<String>,
    #[serde(rename = "States")]
    states: Vec<String>,
    #[serde(rename = "OwnerIDs")]
    owners: Vec<u32>,
}

#[derive(Debug, Deserialize)]
struct QueueResponse {
    #[serde(rename = "TicketID")]
    tickets: Vec<String>,
}

#[derive(Debug, Serialize)]
struct Credentials {
    #[serde(rename = "UserLogin")]
    user: String,
    #[serde(rename = "Password")]
    password: String,
}

#[derive(Debug, Deserialize)]
struct TicketResponse {
    #[serde(rename = "Ticket")]
    ticket: Vec<Ticket>,
}

impl From<Settings> for Credentials {
    fn from(settings: Settings) -> Self {
        Credentials {
            user: settings.user,
            password: settings.password,
        }
    }
}

impl From<Settings> for QueueRequest {
    fn from(settings: Settings) -> Self {
        QueueRequest {
            user: settings.user,
            password: settings.password,
            queues: settings
                .queues
                .split(", ")
                .into_iter()
                .map(|s| s.to_string())
                .collect(),
            states: vec!["open".to_owned(), "new".to_owned()],
            owners: settings
                .owners
                .split(", ")
                .into_iter()
                .map(|id| id.parse().unwrap())
                .collect(),
        }
    }
}

impl Settings {
    fn get() -> Result<Self> {
        let mut settings = Config::new();
        settings.merge(Environment::with_prefix("otrs"))?;
        let settings = settings.try_into()?;
        Ok(settings)
    }
}

struct OTRSSync {
    client: Client,
    settings: Settings,
}

impl OTRSSync {
    fn new() -> Result<Self> {
        Ok(OTRSSync {
            client: Client::new(),
            settings: Settings::get()?,
        })
    }

    fn get_ids(&self) -> Result<Vec<String>> {
        let mut builder = self.client.post(&self.queue_url());
        let request_body = QueueRequest::from(self.settings.clone());
        builder.json(&request_body);
        let mut response = builder.send()?;
        let response = response.json::<QueueResponse>()?;
        Ok(response.tickets)
    }

    fn get_ticket(&self, id: String) -> Result<Ticket> {
        let mut builder = self.client.post(&self.ticket_url(id));
        let request_body = Credentials::from(self.settings.clone());
        builder.json(&request_body);
        let mut response = builder.send()?;
        let response = response.json::<TicketResponse>()?;
        Ok(response.ticket.into_iter().next().chain_err(
            || "No Tickets",
        )?)
    }

    fn queue_url(&self) -> String {
        self.base_url() + "TicketSearch"
    }

    fn base_url(&self) -> String {
        format!(
            "https://{}/otrs/nph-genericinterface.pl/Webservice/GenericConnector/",
            &self.settings.host
        )
    }

    fn ticket_url(&self, id: String) -> String {
        self.base_url() + "TicketGet/" + &id
    }

    fn get_tickets(&self) -> Result<Vec<Ticket>> {
        self.get_ids()?
            .into_iter()
            .map(|id| self.get_ticket(id))
            .collect()
    }
}

pub fn sync_otrs(cache: &mut TaskCache) -> Result<()> {
    let sync = OTRSSync::new()?;
    let tickets = sync.get_tickets()?;
    let tasks = tickets.into_iter().map(|ticket| {
        let mut t = TaskBuilder::default()
            .description(format!("CDA: {}: {}", ticket.customer, ticket.title))
            .build()
            .expect("TaskBuilding failed inspite of set description");
        t.set_gen_name(Some("cda-otrs"));
        t.set_gen_id(Some(ticket.number.to_string()));
        t.set_gen_orphan(OrphanBehavior::CompleteOrphan);
        t
    });
    cache.generate(tasks)?;
    cache.write()?;
    Ok(())
}
