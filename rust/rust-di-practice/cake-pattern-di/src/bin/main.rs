use actix_web::{web::Data, App, HttpServer};
use cake_pattern_di::{router::find_user, AppModule};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(move || {
        App::new()
            .service(find_user)
            .app_data(Data::new(AppModule::new()))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
