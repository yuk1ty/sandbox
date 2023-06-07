use std::{
    alloc::{alloc, dealloc, Layout},
    collections::{HashMap, HashSet, LinkedList},
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
static mut MESSAGES: *mut MappedList<u64> = std::ptr::null_mut();
static mut WAITING: *mut HashMap<u64, Box<Context>> = std::ptr::null_mut();

struct MappedList<T> {
    map: HashMap<u64, LinkedList<T>>,
}

impl<T> MappedList<T> {
    fn new() -> Self {
        MappedList {
            map: HashMap::new(),
        }
    }

    fn push_back(&mut self, key: u64, val: T) {
        if let Some(list) = self.map.get_mut(&key) {
            list.push_back(val);
        } else {
            let mut list = LinkedList::new();
            list.push_back(val);
            self.map.insert(key, list);
        }
    }

    fn pop_front(&mut self, key: u64) -> Option<T> {
        if let Some(list) = self.map.get_mut(&key) {
            let val = list.pop_front();
            if list.len() == 0 {
                self.map.remove(&key);
            }
            val
        } else {
            None
        }
    }

    fn clear(&mut self) {
        self.map.clear();
    }
}

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

pub fn spawn_from_main(func: Entry, stack_size: usize) {
    unsafe {
        if let Some(_) = &CTX_MAIN {
            panic!("spawn_from_main is called twice");
        }

        CTX_MAIN = Some(Box::new(Registers::new(0)));
        if let Some(ctx) = &mut CTX_MAIN {
            let mut msgs = MappedList::new();
            MESSAGES = &mut msgs as *mut MappedList<u64>;

            let mut waiting = HashMap::new();
            WAITING = &mut waiting as *mut HashMap<u64, Box<Context>>;

            let mut ids = HashSet::new();
            ID = &mut ids as *mut HashSet<u64>;

            if set_context(&mut **ctx as *mut Registers) == 0 {
                CONTEXTS.push_back(Box::new(Context::new(func, stack_size, get_id())));
                let first = CONTEXTS.front().unwrap();
                switch_context(first.get_regs());
            }

            rm_unused_stack();

            CTX_MAIN = None;
            CONTEXTS.clear();
            MESSAGES = std::ptr::null_mut();
            WAITING = std::ptr::null_mut();
            ID = std::ptr::null_mut();

            msgs.clear();
            waiting.clear();
            ids.clear();
        }
    }
}

pub fn send(key: u64, msg: u64) {
    unsafe {
        (*MESSAGES).push_back(key, msg);

        if let Some(ctx) = (*WAITING).remove(&key) {
            CONTEXTS.push_back(ctx);
        }
    }
    schedule();
}

pub fn recv() -> Option<u64> {
    unsafe {
        let key = CONTEXTS.front().unwrap().id;

        if let Some(msg) = (*MESSAGES).pop_front(key) {
            return Some(msg);
        }

        if CONTEXTS.len() == 1 {
            panic!("deadlock");
        }

        let mut ctx = CONTEXTS.front().unwrap();
        let regs = ctx.get_regs_mut();
        (*WAITING).insert(key, ctx);

        if set_context(regs) == 0 {
            let next = CONTEXTS.front().unwrap();
            switch_context((**next).get_regs());
        }

        rm_unused_stack();

        (*MESSAGES).pop_front(key)
    }
}
