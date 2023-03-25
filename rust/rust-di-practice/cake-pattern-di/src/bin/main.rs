use std::sync::Arc;

use actix_web::{web::Data, App, HttpServer};
use cake_pattern_di::{UserRepositoryImpl, UserServiceImpl};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    Ok(())
}
