use anyhow::Result;
use common::User;

pub struct UserService<UR: UserRepository> {
    repository: UR,
}

impl<UR: UserRepository> UserService<UR> {
    pub fn new(repository: UR) -> UserService<UR> {
        UserService { repository }
    }

    pub fn find_user(&self, id: String) -> Result<Option<User>> {
        self.repository.find_user(id)
    }

    pub fn deactivate_user(&self, id: String) -> Result<()> {
        let user = self.repository.find_user(id)?;
        if let Some(mut user) = user {
            user.effective = false;
            self.repository.update(user)?;
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
    static_user_service: UserService<UserRepositoryImpl>,
}

impl AppModule {
    pub fn new() -> AppModule {
        let database = Database;
        let static_user_service = UserService::new(UserRepositoryImpl::new(database));

        AppModule {
            static_user_service,
        }
    }

    pub fn static_user_service(&self) -> &UserService<UserRepositoryImpl> {
        &self.static_user_service
    }
}

pub mod router {
    use actix_web::{get, web::Data, web::Path, HttpResponse};

    #[get("/users/{id}")]
    pub async fn find_user(id: Path<String>, app_module: Data<crate::AppModule>) -> HttpResponse {
        let user = app_module.static_user_service.find_user(id.into_inner());
        match user {
            Ok(Some(user)) => HttpResponse::Ok().json(user),
            Ok(None) => HttpResponse::NotFound().finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }
}
