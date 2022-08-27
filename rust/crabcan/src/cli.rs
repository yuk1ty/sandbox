use std::path::PathBuf;
use structopt::StructOpt;

use crate::error::Errcode;

#[derive(Debug, StructOpt)]
#[structopt(name = "crabcan", about = "A simple container in Rust.")]
pub struct Args {
    #[structopt(short, long)]
    debug: bool,
    #[structopt(short, long)]
    pub command: String,
    #[structopt(short, long)]
    pub uid: u32,
    #[structopt(parse(from_os_str), short = "m", long = "mount")]
    pub mount_dir: PathBuf,
}

pub fn parse_args() -> Result<Args, Errcode> {
    let args = Args::from_args();

    if args.debug {
        setup_log(log::LevelFilter::Debug);
    } else {
        setup_log(log::LevelFilter::Info);
    }

    Ok(args)
}

pub fn setup_log(level: log::LevelFilter) {
    env_logger::Builder::from_default_env()
        .format_timestamp_secs()
        .filter(None, level)
        .init();
}
