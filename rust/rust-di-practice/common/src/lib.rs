use serde_derive::Serialize;

#[derive(Serialize, Clone, Debug)]
pub struct User {
    pub id: String,
    pub effective: bool,
}
