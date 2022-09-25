use lunatic::{
    process::{
        AbstractProcess, Message, MessageHandler, ProcessRef, Request, RequestHandler, StartProcess,
    },
    supervisor::{Supervisor, SupervisorConfig},
};
use serde::{Deserialize, Serialize};
use submillisecond::{http::StatusCode, router, Application, Json};
use uuid::Uuid;

pub struct PersistentSupervisor;

impl Supervisor for PersistentSupervisor {
    type Arg = String;
    type Children = PersistentProcess;

    fn init(config: &mut SupervisorConfig<Self>, name: Self::Arg) {
        config.children_args(((), Some(name)))
    }
}

pub struct PersistentProcess {
    todos: Vec<Todo>,
}

impl AbstractProcess for PersistentProcess {
    type Arg = ();
    type State = Self;

    fn init(_: ProcessRef<Self>, _: Self::Arg) -> Self::State {
        PersistentProcess { todos: Vec::new() }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Todo {
    id: Uuid,
    title: String,
    description: String,
}

#[derive(Serialize, Deserialize)]
pub struct AddTodo(Todo);

impl MessageHandler<AddTodo> for PersistentProcess {
    fn handle(state: &mut Self::State, AddTodo(todo): AddTodo) {
        state.todos.push(todo);
    }
}

#[derive(Serialize, Deserialize)]
pub struct ListTodo;

impl RequestHandler<ListTodo> for PersistentProcess {
    type Response = Vec<Todo>;

    fn handle(state: &mut Self::State, _: ListTodo) -> Self::Response {
        state.todos.clone()
    }
}

#[derive(Deserialize, Debug)]
pub struct CreateTodo {
    title: String,
    description: String,
}

fn create_todo(Json(req): Json<CreateTodo>) -> StatusCode {
    let persistence = ProcessRef::<PersistentProcess>::lookup("persistence").unwrap();
    let todo = Todo {
        id: Uuid::new_v4(),
        title: req.title,
        description: req.description,
    };
    persistence.send(AddTodo(todo));

    StatusCode::CREATED
}

fn list_todo() -> Json<Vec<Todo>> {
    let persistence = ProcessRef::<PersistentProcess>::lookup("persistence").unwrap();
    Json(persistence.request(ListTodo))
}

fn main() -> std::io::Result<()> {
    PersistentSupervisor::start_link("persistence".to_string(), None);
    Application::new(router! {
        POST "/api/todos" => create_todo
        GET  "/api/todos" => list_todo
    })
    .serve("0.0.0.0:3000")
}
