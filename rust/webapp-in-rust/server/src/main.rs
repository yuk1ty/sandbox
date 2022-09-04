use std::{env, net::SocketAddr, sync::Arc};

use axum::{
    extract::Path,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, patch, post},
    Extension, Json, Router,
};
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::{MySql, MySqlPool, Pool};

#[derive(Serialize)]
struct Book {
    id: i64,
    title: String,
    author: String,
    publisher: String,
    isbn: String,
    comment: String,
    created_at: NaiveDateTime,
    updated_at: NaiveDateTime,
}

#[derive(Serialize)]
struct BookList(Vec<Book>);

#[derive(Deserialize)]
struct CreateNewBook {
    title: String,
    author: String,
    publisher: String,
    isbn: String,
    comment: String,
}

#[derive(Deserialize)]
struct UpdateComment {
    comment: String,
}

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
        .map(|books| Json(BookList(books)))
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn create_item(
    Json(req): Json<CreateNewBook>,
    Extension(db): Extension<MySqlConPool>,
) -> anyhow::Result<impl IntoResponse, StatusCode> {
    let conn = db.acquire().await;
    if conn.is_err() {
        return Err(StatusCode::INTERNAL_SERVER_ERROR);
    }

    let rows_affected = sqlx::query!(
        r#"insert into books (title, author, publisher, isbn, comment, created_at, updated_at) values (?, ?, ?, ?, ?, now(), now())"#,
        req.title,
        req.author,
        req.publisher,
        req.isbn,
        req.comment,
    )
    .execute(&mut conn.unwrap())
    .await
    .map(|result| result.rows_affected());

    match rows_affected {
        Ok(count) => {
            if count > 0 {
                Ok(StatusCode::CREATED)
            } else {
                Err(StatusCode::INTERNAL_SERVER_ERROR)
            }
        }
        Err(err) => {
            eprintln!("{:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn update_comment(
    Path(id): Path<i64>,
    Json(req): Json<UpdateComment>,
    Extension(db): Extension<MySqlConPool>,
) -> anyhow::Result<impl IntoResponse, StatusCode> {
    let conn = db.acquire().await;
    if conn.is_err() {
        return Err(StatusCode::INTERNAL_SERVER_ERROR);
    }

    let rows_affected = sqlx::query!(
        r#"update books set comment = ?, updated_at = now() where id = ?"#,
        req.comment,
        id
    )
    .execute(&mut conn.unwrap())
    .await
    .map(|result| result.rows_affected());

    match rows_affected {
        Ok(count) => {
            if count > 0 {
                Ok(StatusCode::OK)
            } else {
                Err(StatusCode::INTERNAL_SERVER_ERROR)
            }
        }
        Err(err) => {
            eprintln!("{:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let pool = MySqlPool::connect(&env::var("DATABASE_URL")?).await?;
    let books_router = Router::new()
        .route("/", get(book_list))
        .route("/", post(create_item))
        .route("/:id/comment", patch(update_comment));
    let app = Router::new()
        .route("/health", get(health_check))
        .nest("/books", books_router)
        .layer(Extension(Arc::new(pool)));
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
    Ok(())
}
