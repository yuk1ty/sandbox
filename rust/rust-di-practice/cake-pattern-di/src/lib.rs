use anyhow::Result;
use common::User;

pub trait UserRepository: Send + Sync + 'static {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

pub trait UsesUserRepository {
    type UserRepository: UserRepository;
    fn user_repository(&self) -> &Self::UserRepository;
}

pub struct MixInUserRepository {
    user_repository: UserRepositoryImpl,
}

impl UsesUserRepository for MixInUserRepository {
    type UserRepository = UserRepositoryImpl;

    fn user_repository(&self) -> &Self::UserRepository {
        &self.user_repository
    }
}

pub struct UserRepositoryImpl {}

impl UserRepository for UserRepositoryImpl {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        todo!()
    }

    fn update(&self, user: User) -> Result<()> {
        todo!()
    }
}

pub trait UserService: UsesUserRepository {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        self.user_repository().find_user(id)
    }

    fn deactivate_user(&self, id: String) -> Result<()> {
        let user = self.user_repository().find_user(id)?;
        if let Some(mut user) = user {
            user.effective = false;
            self.user_repository().update(user)?;
        };
        Ok(())
    }
}

impl<T: UsesUserRepository> UserService for T {}

pub struct UserServiceImpl {}

impl UsesUserRepository for UserServiceImpl {
    type UserRepository = UserRepositoryImpl;

    fn user_repository(&self) -> &Self::UserRepository {
        &self.user_repository()
    }
}
