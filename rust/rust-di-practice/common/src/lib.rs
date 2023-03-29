use serde_derive::Serialize;

#[derive(Serialize)]
pub struct User {
    pub id: String,
    pub effective: bool,
}
