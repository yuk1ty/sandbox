use std::{fmt, process::exit};

pub fn exit_with_retcode(res: Result<(), Errcode>) {
    match res {
        Ok(_) => {
            log::debug!("Exit without any error, returning 0");
            exit(0);
        }
        Err(e) => {
            let retcode = e.get_retcode();
            log::error!("Error on exit:\n\t{}\n\tReturning {}", e, retcode);
            exit(retcode);
        }
    }
}

#[derive(Debug)]
pub enum Errcode {
    NotSupported(u8),
    ContainerError(u8),
}

impl Errcode {
    pub fn get_retcode(&self) -> i32 {
        1
    }
}

impl fmt::Display for Errcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            _ => write!(f, "{:?}", self),
        }
    }
}
