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

pub struct UserRepositoryImpl {}

impl UserRepositoryImpl {
    pub fn new() -> UserRepositoryImpl {
        UserRepositoryImpl {}
    }
}

impl UserRepository for UserRepositoryImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        todo!()
    }

    fn update(&self, user: User) -> Result<()> {
        todo!()
    }
}
