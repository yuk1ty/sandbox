use actix_web::{web::Data, App, HttpServer};
use shaku_di::AppModule;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(move || App::new().app_data(Data::new(AppModule::builder().build())))
        .bind(("127.0.0.1", 8080))?
        .run()
        .await
}
