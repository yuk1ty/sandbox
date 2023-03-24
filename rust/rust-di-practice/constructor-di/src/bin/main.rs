use std::sync::Arc;

use actix_web::{web::Data, App, HttpServer};
use constructor_di::{
    dynamic_dispatch::UserService as DynUserService,
    static_dispatch::UserService as StaticUserService, UserRepositoryImpl,
};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let user_repository = UserRepositoryImpl::new();
    let dynamic_dispatched_user_service = Data::new(DynUserService::new(Arc::new(user_repository)));

    let user_repository = UserRepositoryImpl::new();
    let static_dispatched_user_service = Data::new(StaticUserService::new(user_repository));

    HttpServer::new(move || {
        App::new()
            .app_data(Data::clone(&dynamic_dispatched_user_service))
            .app_data(Data::clone(&static_dispatched_user_service))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
