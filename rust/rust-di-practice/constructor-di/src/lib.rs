use anyhow::Result;
use dynamic_dispatch::UserService as DynUserService;
use static_dispatch::UserService;
use std::sync::Arc;

use common::User;

pub mod dynamic_dispatch {
    use super::*;

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
}

pub mod static_dispatch {
    use super::*;

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

// 直接 AppModule に持たせてしまうと参照のライフタイムの問題が発生するので、
// あえて別の構造体に切り出して、そこから参照を得るように調整している。
pub struct RepositoriesModule {
    user_repository: Arc<dyn UserRepository>,
}

impl RepositoriesModule {
    // データベースへの接続情報などを入れる必要が出てきた場合には、この new 関数内で生成するか
    // もしくは上のモジュールからパスするように実装を調整するとよい。
    pub fn new() -> RepositoriesModule {
        let user_repository = Arc::new(UserRepositoryImpl::new());

        RepositoriesModule { user_repository }
    }

    pub fn user_repository(&self) -> Arc<dyn UserRepository> {
        Arc::clone(&self.user_repository)
    }
}

pub struct AppModule {
    repositories_module: RepositoriesModule,
    dynamic_user_service: Arc<DynUserService>,
    static_user_service: UserService<UserRepositoryImpl>,
}

impl AppModule {
    pub fn new() -> AppModule {
        let repositories_module = RepositoriesModule::new();
        let dynamic_user_service = Arc::new(DynUserService::new(Arc::clone(
            &repositories_module.user_repository(),
        )));
        let static_user_service = UserService::new(UserRepositoryImpl::new());

        AppModule {
            repositories_module,
            dynamic_user_service,
            static_user_service,
        }
    }

    pub fn dynamic_user_service(&self) -> Arc<DynUserService> {
        Arc::clone(&self.dynamic_user_service)
    }

    pub fn static_user_service(&self) -> &UserService<UserRepositoryImpl> {
        &self.static_user_service
    }
}
