use std::{env, net::SocketAddr, sync::Arc};

use axum::{http::StatusCode, response::IntoResponse, routing::get, Extension, Json, Router};
use serde::Serialize;
use sqlx::{MySql, MySqlPool, Pool};

#[derive(Serialize)]
struct Book {
    id: i64,
    title: String,
    author: String,
    publisher: String,
    isbn: String,
}

#[derive(Serialize)]
struct BookList(Vec<Book>);

type MySqlConPool = Arc<Pool<MySql>>;

async fn health_check() -> impl IntoResponse {
    StatusCode::NO_CONTENT
}

async fn book_list(
    Extension(db): Extension<MySqlConPool>,
) -> anyhow::Result<impl IntoResponse, StatusCode> {
    let conn = db.acquire().await;
    if conn.is_err() {
        return Err(StatusCode::INTERNAL_SERVER_ERROR);
    }

    sqlx::query_as!(Book, "select * from books")
        .fetch_all(&mut conn.unwrap())
        .await
        .map(|books| (StatusCode::OK, Json(BookList(books))))
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let pool = MySqlPool::connect(&env::var("DATABASE_URL")?).await?;
    let app = Router::new()
        .route("/health", get(health_check))
        .route("/books", get(book_list))
        .layer(Extension(Arc::new(pool)));
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
    Ok(())
}
