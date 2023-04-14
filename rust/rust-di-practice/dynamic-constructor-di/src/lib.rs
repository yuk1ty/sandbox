use std::sync::Arc;

use anyhow::Result;
use common::User;

pub struct UserService {
    repository: Arc<dyn UserRepository>,
}

impl UserService {
    pub fn new(repository: Arc<dyn UserRepository>) -> UserService {
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
        let user = self.find_user(user.id)?;
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
    user_service: UserService,
}

impl AppModule {
    pub fn new() -> AppModule {
        let database = Database;
        let user_service = UserService::new(Arc::new(UserRepositoryImpl::new(database)));

        AppModule { user_service }
    }

    pub fn user_service(&self) -> &UserService {
        &self.user_service
    }
}
