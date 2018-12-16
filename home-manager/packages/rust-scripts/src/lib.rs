#![warn(missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]
extern crate dirs;
extern crate dialog;
extern crate uuid;
extern crate task_hookrs;
extern crate hostname;
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
extern crate reqwest;
extern crate config;

pub mod hotkeys;
pub mod generate;
pub mod refresh;
pub mod update;
pub mod kassandra;
pub mod tasktree;
pub mod well_known;
pub mod mail;
pub mod otrs;
#[allow(renamed_and_removed_lints)]

pub mod error {
    use task_hookrs::error as terror;
    use dialog::errors as derror;
    error_chain! {
        links {
            TaskError(terror::Error, terror::ErrorKind);
            DialogError(derror::Error, derror::ErrorKind);
        }
        foreign_links {
            Io(::std::io::Error);
            Yaml(::serde_yaml::Error);
            PathError(::std::env::JoinPathsError);
            MailDir(::maildir::MailEntryError);
            MailParse(::mailparse::MailParseError);
            Config(::config::ConfigError);
            HttpError(::reqwest::Error);
        }
        errors {
            WorkingOnTask(t: String) {
                description("Working on task")
                display("Working on task:\n{}", t)
            }
        }
    }
}
