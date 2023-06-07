use anyhow::Result;
use common::User;

pub mod service {
    use super::*;

    pub fn find_user<R: UserRepository>(id: String, repository: &R) -> Result<Option<User>> {
        repository.find_user(id)
    }

    pub fn deactivate_user<R: UserRepository>(id: String, repository: &R) -> Result<()> {
        let user = repository.find_user(id)?;
        if let Some(mut user) = user {
            user.effective = false;
            repository.update(user)?;
        };
        Ok(())
    }
}

pub trait UserRepository: Send + Sync + 'static {
    fn find_user(&self, id: String) -> Result<Option<User>>;

    fn update(&self, user: User) -> Result<()>;
}

pub struct UserRepositoryImpl {
    database: Database,
}

impl UserRepositoryImpl {
    pub fn new(database: Database) -> UserRepositoryImpl {
        UserRepositoryImpl { database }
    }
}

impl UserRepository for UserRepositoryImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        self.database.find_user(id)
    }

    fn update(&self, user: User) -> Result<()> {
        let user = self.database.find_user(user.id)?;
        if let Some(mut user) = user {
            user.effective = false;
            self.database.update(user)?;
        };
        Ok(())
    }
}

pub struct Database;

impl Database {
    pub fn find_user(&self, id: String) -> Result<Option<User>> {
        Ok(Some(User {
            id: "id-a".to_string(),
            effective: true,
        }))
    }

    pub fn update(&self, user: User) -> Result<()> {
        Ok(println!("updated user: {:?}", user))
    }
}

pub struct AppModule {
    user_repository: UserRepositoryImpl,
}

impl AppModule {
    pub fn new() -> AppModule {
        let database = Database;
        let user_repository = UserRepositoryImpl::new(database);

        AppModule { user_repository }
    }

    pub fn user_repository(&self) -> &UserRepositoryImpl {
        &self.user_repository
    }
}

pub mod router {
    use actix_web::{get, web::Data, web::Path, HttpResponse};

    use crate::service;

    #[get("/users/{id}")]
    pub async fn find_user(id: Path<String>, app_module: Data<crate::AppModule>) -> HttpResponse {
        let user = service::find_user(id.into_inner(), app_module.user_repository());
        match user {
            Ok(Some(user)) => HttpResponse::Ok().json(user),
            Ok(None) => HttpResponse::NotFound().finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }
}
