extern crate dialog;
extern crate uuid;
extern crate task_hookrs;
#[macro_use]
extern crate lazy_static;
extern crate kairos;
extern crate chrono;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;

pub mod hotkeys;
pub mod generate;
pub mod refresh;
pub mod update;
pub mod kassandra;
pub mod tasktree;

pub mod error {
    use task_hookrs::error as terror;
    use dialog::errors as derror;
    use serde_yaml;
    error_chain! {
        links {
            TaskError(terror::Error, terror::ErrorKind);
            DialogError(derror::Error, derror::ErrorKind);
        }
        foreign_links {
            Io(::std::io::Error);
            Yaml(serde_yaml::Error);
            PathError(::std::env::JoinPathsError);
        }
    }
}
