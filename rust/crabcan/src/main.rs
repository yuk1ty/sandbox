use std::process::exit;

use crate::{container::start, error::exit_with_retcode};

#[macro_use]
extern crate scan_fmt;
mod cli;
mod config;
mod container;
mod error;

fn main() {
    match cli::parse_args() {
        Ok(args) => {
            log::info!("{:?}", args);
            exit_with_retcode(start(args))
        }
        Err(e) => {
            log::error!("Error while parsing arguments:\n\t{}", e);
            exit(e.get_retcode());
        }
    };
}
