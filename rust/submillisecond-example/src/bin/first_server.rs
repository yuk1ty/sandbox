use serde::{Deserialize, Serialize};
use submillisecond::{router, Application, Json};

#[derive(Serialize)]
struct User {
    id: String,
}

impl User {
    fn new(id: impl Into<String>) -> Self {
        User { id: id.into() }
    }
}

// handler が正しく型解決するためには、Debug が必要らしい。修正した方がいいとは思う。
#[derive(Deserialize, Debug)]
struct PersonalInfo {
    name: String,
    age: u8,
}

fn show_registered_users() -> Json<Vec<User>> {
    Json(vec![
        User::new("user-a"),
        User::new("user-b"),
        User::new("user-c"),
    ])
}

fn echo_personal_info(Json(req): Json<PersonalInfo>) -> String {
    format!(
        "Hi! Your name is {} and you're {} years old, right?",
        req.name, req.age
    )
}

fn main() -> std::io::Result<()> {
    Application::new(router! {
        GET "/users" => show_registered_users
        POST "/echo" => echo_personal_info
    })
    .serve("0.0.0.0:3000")
}
