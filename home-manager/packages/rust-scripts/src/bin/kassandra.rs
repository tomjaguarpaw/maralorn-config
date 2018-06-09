extern crate rust_scripts;

use rust_scripts::{
    error::Result,
    kassandra::kassandra
};

fn main() -> Result<()> {
    kassandra()
}
