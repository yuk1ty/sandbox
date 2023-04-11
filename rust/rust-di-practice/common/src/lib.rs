use serde_derive::Serialize;

#[derive(Serialize, Clone)]
pub struct User {
    pub id: String,
    pub effective: bool,
}
