use anyhow::Result;
use common::User;

pub trait UsesUserRepository {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

pub trait UserRepository {}

impl<T: UserRepository> UsesUserRepository for T {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        todo!()
    }

    fn update(&self, user: User) -> Result<()> {
        todo!()
    }
}

pub trait ProvidesUserRepository {
    type T: UsesUserRepository;
    fn user_repository(&self) -> &Self::T;
}

pub trait UsesUserService {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn deactivate_user(&self, id: String) -> Result<()>;
}

pub trait UserService: ProvidesUserRepository {}

impl<T: UserService> UsesUserService for T {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        todo!()
    }

    fn deactivate_user(&self, id: String) -> Result<()> {
        todo!()
    }
}

pub trait ProvidesUserService {
    type T: UsesUserService;
    fn user_service(&self) -> &Self::T;
}

pub struct MixInUserService {}

impl UserRepository for MixInUserService {}
impl UserService for MixInUserService {}

impl ProvidesUserRepository for MixInUserService {
    type T = MixInUserService;
    fn user_repository(&self) -> &Self::T {
        self
    }
}

impl ProvidesUserService for MixInUserService {
    type T = MixInUserService;
    fn user_service(&self) -> &Self::T {
        self
    }
}
