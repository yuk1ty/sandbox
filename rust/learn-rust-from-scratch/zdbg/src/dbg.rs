use crate::helper::DynError;
use nix::{
    libc::user_regs_struct,
    sys::{
        personality::{self, Persona},
        ptrace,
        wait::{waitpid, WaitStatus},
    },
    unistd::{execvp, fork, ForkResult, Pid},
};
use std::ffi::{c_void, CString};

pub struct DbgInfo {
    pid: Pid,
    brk_addr: Option<*mut c_void>,
    brk_val: i64,
    filename: String,
}

pub struct ZDbg<T> {
    info: Box<DbgInfo>,
    _state: T,
}

pub struct Running;
pub struct NotRunning;

pub enum State {
    Running(ZDbg<Running>),
    NotRunning(ZDbg<NotRunning>),
    Exit,
}

impl<T> ZDbg<T> {
    fn set_break_addr(&mut self, cmd: &[&str]) -> bool {
        if self.info.brk_addr.is_some() {
            eprintln!(
                "<< ブレークポイントは設定済みです : Addr = {:p} >>",
                self.info.brk_addr.unwrap()
            );
            false
        } else if let Some(addr) = get_break_addr(cmd) {
            self.info.brk_addr = Some(addr);
            true
        } else {
            false
        }
    }

    fn do_cmd_common(&self, cmd: &[&str]) {
        match cmd[0] {
            "help" | "h" => do_help(),
            _ => (),
        }
    }
}

impl ZDbg<NotRunning> {
    pub fn new(filename: String) -> Self {
        ZDbg {
            info: Box::new(DbgInfo {
                pid: Pid::from_raw(0),
                brk_addr: None,
                brk_val: 0,
                filename,
            }),
            _state: NotRunning,
        }
    }

    pub fn do_cmd(mut self, cmd: &[&str]) -> Result<State, DynError> {
        if cmd.is_empty() {
            return Ok(State::NotRunning(self));
        }

        match cmd[0] {
            "run" | "r" => return self.do_run(cmd),
            "break" | "b" => {
                self.do_break(cmd);
            }
            "exit" => return Ok(State::Exit),
            _ => self.do_cmd_common(cmd),
        }

        Ok(State::NotRunning(self))
    }

    fn do_break(&mut self, cmd: &[&str]) -> bool {
        self.set_break_addr(cmd)
    }

    fn do_run(mut self, cmd: &[&str]) -> Result<State, DynError> {
        let args: Vec<CString> = cmd.iter().map(|s| CString::new(*s).unwrap()).collect();

        // match unsafe { fork()? } {
        //     ForkResult::Child => {
        //         let p = personality::get().unwrap();
        //     }
        // }
    }
}

impl ZDbg<Running> {}

fn do_help() {
    println!(
        r#"コマンド一覧 (括弧内は省略記法)
break 0x8000 : ブレークポイントを0x8000番地に設定 (b 0x8000)
run          : プログラムを実行 (r)
continue     : プログラムを再開 (c)
stepi        : 機械語レベルで1ステップ実行 (s)
registers    : レジスタを表示 (regs)
exit         : 終了
help         : このヘルプを表示 (h)"#
    );
}

fn get_break_addr(cmd: &[&str]) -> Option<*mut c_void> {
    if cmd.len() < 2 {
        eprintln!("<<アドレスを指定してください\n例 : break 0x8000>>");
        return None;
    }

    let addr_str = cmd[1];
    if &addr_str[0..2] != "0x" {
        eprintln!("<<アドレスは16進数でのみ指定可能です\n例 : break 0x8000>>");
        return None;
    }

    let addr = match usize::from_str_radix(&addr_str[2..], 16) {
        Ok(addr) => addr,
        Err(e) => {
            eprintln!("<<アドレス変換エラー : {}>>", e);
            return None;
        }
    } as *mut c_void;

    Some(addr)
}
