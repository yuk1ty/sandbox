use std::sync::Arc;

use anyhow::Result;
use common::User;
use shaku::{module, Component, Interface};

pub trait Database: Interface {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

#[derive(Component)]
#[shaku(interface = Database)]
pub struct DatabaseImpl;

impl Database for DatabaseImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        Ok(Some(User {
            id: "id-a".to_string(),
            effective: true,
        }))
    }

    fn update(&self, user: User) -> Result<()> {
        Ok(println!("updated_user: {:?}", user))
    }
}

pub trait UserRepository: Interface {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

#[derive(Component)]
#[shaku(interface = UserRepository)]
pub struct UserRepositoryImpl {
    #[shaku(inject)]
    database: Arc<dyn Database>,
}

impl UserRepository for UserRepositoryImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        self.database.find_user(id)
    }

    fn update(&self, user: User) -> Result<()> {
        self.database.update(user)
    }
}

pub trait UserService: Interface {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn deactivate_user(&self, id: String) -> Result<()>;
}

#[derive(Component)]
#[shaku(interface = UserService)]
pub struct UserServiceImpl {
    #[shaku(inject)]
    user_repository: Arc<dyn UserRepository>,
}

impl UserService for UserServiceImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        self.user_repository.find_user(id)
    }

    fn deactivate_user(&self, id: String) -> Result<()> {
        let user = self.user_repository.find_user(id)?;
        if let Some(mut user) = user {
            user.effective = false;
            self.user_repository.update(user)?;
        };
        Ok(())
    }
}

module! {
    pub AppModule {
        components = [UserServiceImpl, UserRepositoryImpl, DatabaseImpl],
        providers = []
    }
}

pub mod router {
    use crate::{AppModule, UserService};
    use actix_web::{get, web::Path, HttpResponse};
    use shaku_actix::Inject;

    #[get("/users/{id}")]
    pub async fn find_user(
        id: Path<String>,
        service: Inject<AppModule, dyn UserService>,
    ) -> HttpResponse {
        let user = service.find_user(id.into_inner());
        match user {
            Ok(Some(user)) => HttpResponse::Ok().json(user),
            Ok(None) => HttpResponse::NotFound().finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }
}
