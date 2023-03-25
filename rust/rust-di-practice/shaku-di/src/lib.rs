use std::sync::Arc;

use anyhow::Result;
use common::User;
use shaku::{module, Component, Interface};

pub trait UserRepository: Interface {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

#[derive(Component)]
#[shaku(interface = UserRepository)]
pub struct UserRepositoryImpl;

impl UserRepository for UserRepositoryImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        todo!()
    }

    fn update(&self, user: User) -> Result<()> {
        todo!()
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
        components = [UserRepositoryImpl],
        providers = []
    }
}
