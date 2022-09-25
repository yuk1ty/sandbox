use std::collections::HashMap;

use lunatic::{
    process::{AbstractProcess, ProcessRef},
    supervisor::{Supervisor, SupervisorConfig},
};

pub struct PersistentSupervisor;

impl Supervisor for PersistentSupervisor {
    type Arg = String;
    type Children = PersistentProcess;

    fn init(config: &mut SupervisorConfig<Self>, name: Self::Arg) {
        config.children_args(((), Some(name)))
    }
}

pub struct PersistentProcess {
    todos: HashMap<String, Todo>,
}

impl AbstractProcess for PersistentProcess {
    type Arg = ();
    type State = Self;

    fn init(_: ProcessRef<Self>, _: Self::Arg) -> Self::State {
        PersistentProcess {
            todos: HashMap::new(),
        }
    }
}

pub struct Todo {
    title: String,
    description: String,
}

fn main() {}
