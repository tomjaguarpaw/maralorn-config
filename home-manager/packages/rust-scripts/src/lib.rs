extern crate dialog;
extern crate uuid;
extern crate task_hookrs;
#[macro_use]
extern crate lazy_static;
extern crate chrono;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
extern crate maildir;
extern crate mailparse;
extern crate regex;

pub mod hotkeys;
pub mod generate;
pub mod refresh;
pub mod update;
pub mod kassandra;
pub mod tasktree;
pub mod well_known;
pub mod mail;

pub mod error {
    use task_hookrs::error as terror;
    use dialog::errors as derror;
    use serde_yaml;
    use maildir;
    use mailparse;
    error_chain! {
        links {
            TaskError(terror::Error, terror::ErrorKind);
            DialogError(derror::Error, derror::ErrorKind);
        }
        foreign_links {
            Io(::std::io::Error);
            Yaml(serde_yaml::Error);
            PathError(::std::env::JoinPathsError);
            MailDir(maildir::MailEntryError);
            MailParse(mailparse::MailParseError);
        }
    }
}
