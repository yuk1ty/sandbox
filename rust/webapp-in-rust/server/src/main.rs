use std::{env, net::SocketAddr, sync::Arc};

use axum::{http::StatusCode, response::IntoResponse, routing::get, Extension, Router};
use sqlx::{MySql, MySqlPool, Pool};

struct Book {
    id: i64,
    title: String,
    author: String,
    publisher: String,
    isbn: String,
    rating: i8,
}

type MySqlConPool = Arc<Pool<MySql>>;

async fn health_check() -> impl IntoResponse {
    StatusCode::NO_CONTENT
}

async fn book_list(Extension(db): Extension<MySqlConPool>) -> anyhow::Result<impl IntoResponse> {
    let mut conn = db.acquire().await?;
    sqlx::query!("select * from books")
        .fetch_all(&mut conn)
        .await?;
    Ok(StatusCode::OK)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let pool = MySqlPool::connect(&env::var("DATABASE_URL")?).await?;
    let app = Router::new()
        .nest("/health", get(health_check))
        .layer(Extension(Arc::new(pool)));
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
    Ok(())
}
