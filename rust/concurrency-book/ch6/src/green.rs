use std::{
    alloc::{alloc, dealloc, Layout},
    collections::{HashSet, LinkedList},
    ffi::c_void,
};

use nix::sys::mman::{mprotect, ProtFlags};

#[repr(C)]
struct Registers {
    d8: u64,
    d9: u64,
    d10: u64,
    d11: u64,
    d12: u64,
    d13: u64,
    d14: u64,
    d15: u64,
    d16: u64,
    d17: u64,
    d18: u64,
    d19: u64,
    d20: u64,
    d21: u64,
    d22: u64,
    d23: u64,
    d24: u64,
    d25: u64,
    d26: u64,
    d27: u64,
    d28: u64,
    // リンクレジスタ
    x30: u64,
    // スタックポインタ
    sp: u64,
}

impl Registers {
    fn new(sp: u64) -> Self {
        Registers {
            d8: 0,
            d9: 0,
            d10: 0,
            d11: 0,
            d12: 0,
            d13: 0,
            d14: 0,
            d15: 0,
            d16: 0,
            d17: 0,
            d18: 0,
            d19: 0,
            d20: 0,
            d21: 0,
            d22: 0,
            d23: 0,
            d24: 0,
            d25: 0,
            d26: 0,
            d27: 0,
            d28: 0,
            x30: entry_point as u64,
            sp,
        }
    }
}

extern "C" {
    // 現在のレジスタを保存する。協調的にコンテキストスイッチを行う前に復帰ポイントを保存する際に呼び出す。
    fn set_context(ctx: *mut Registers) -> u64;
    // コンテキストスイッチをする。
    fn switch_context(ctx: *const Registers) -> !;
}

type Entry = fn();
const PAGE_SIZE: usize = 4 * 1024;

struct Context {
    regs: Registers,
    stack: *mut u8,
    stack_layut: Layout,
    entry: Entry,
    id: u64, // thread id
}

impl Context {
    fn get_regs_mut(&mut self) -> *mut Registers {
        &mut self.regs as *mut Registers
    }

    fn get_regs(&self) -> *const Registers {
        &self.regs as *const Registers
    }

    fn new(func: Entry, stack_size: usize, id: u64) -> Self {
        let layout = Layout::from_size_align(stack_size, PAGE_SIZE).unwrap();
        let stack = unsafe { alloc(layout) };

        unsafe { mprotect(stack as *mut c_void, PAGE_SIZE, ProtFlags::PROT_NONE).unwrap() };

        let regs = Registers::new(stack as u64 + stack_size as u64);

        Context {
            regs,
            stack,
            stack_layut: layout,
            entry: func,
            id,
        }
    }
}

static mut CTX_MAIN: Option<Box<Registers>> = None;
static mut UNUSED_STACK: (*mut u8, Layout) = (std::ptr::null_mut(), Layout::new::<u8>());
static mut CONTEXTS: LinkedList<Box<Context>> = LinkedList::new();
static mut ID: *mut HashSet<u64> = std::ptr::null_mut();

fn get_id() -> u64 {
    loop {
        let rnd = rand::random::<u64>();
        unsafe {
            if !(*ID).contains(&rnd) {
                (*ID).insert(rnd);
                return rnd;
            };
        }
    }
}

pub fn spawn(func: Entry, stack_size: usize) -> u64 {
    unsafe {
        let id = get_id();
        CONTEXTS.push_back(Box::new(Context::new(func, stack_size, id)));
        schedule();
        id
    }
}

pub fn schedule() {
    unsafe {
        if CONTEXTS.len() == 1 {
            return;
        }

        let mut ctx = CONTEXTS.pop_front().unwrap();
        let regs = ctx.get_regs_mut();
        CONTEXTS.push_back(ctx);

        if set_context(regs) == 0 {
            let next = CONTEXTS.front().unwrap();
            switch_context((**next).get_regs());
        }

        rm_unused_stack();
    }
}

extern "C" fn entry_point() {
    unsafe {
        let ctx = CONTEXTS.front().unwrap();
        ((**ctx).entry)();
        let ctx = CONTEXTS.pop_front().unwrap();
        (*ID).remove(&ctx.id);
        UNUSED_STACK = ((*ctx).stack, (*ctx).stack_layut);

        match CONTEXTS.front() {
            Some(c) => {
                switch_context((**c).get_regs());
            }
            None => {
                if let Some(c) = &CTX_MAIN {
                    switch_context(&**c as *const Registers);
                }
            }
        }
    }
    unreachable!("entry_point");
}

unsafe fn rm_unused_stack() {
    if UNUSED_STACK.0 != std::ptr::null_mut() {
        mprotect(
            UNUSED_STACK.0 as *mut c_void,
            PAGE_SIZE,
            ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
        )
        .unwrap();
        dealloc(UNUSED_STACK.0, UNUSED_STACK.1);
        UNUSED_STACK = (std::ptr::null_mut(), Layout::new::<u8>());
    }
}
