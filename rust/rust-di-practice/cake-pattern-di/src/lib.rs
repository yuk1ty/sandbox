use anyhow::Result;
use common::User;

pub trait UsesDatabase: Send + Sync + 'static {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

pub trait Database: Send + Sync + 'static {}

impl<T: Database> UsesDatabase for T {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        Ok(Some(User {
            id: "id-a".to_string(),
            effective: true,
        }))
    }

    fn update(&self, user: User) -> Result<()> {
        Ok(println!("updated user: {:?}", user))
    }
}

pub trait ProvidesDatabase: Send + Sync + 'static {
    type T: UsesDatabase;
    fn database(&self) -> &Self::T;
}

pub trait UsesUserRepository: Send + Sync + 'static {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn update(&self, user: User) -> Result<()>;
}

pub trait UserRepository: ProvidesDatabase {}

impl<T: UserRepository> UsesUserRepository for T {
    fn find_user(&self, id: String) -> Result<Option<User>> {
        self.database().find_user(id)
    }

    fn update(&self, user: User) -> Result<()> {
        self.database().update(user)
    }
}

pub trait ProvidesUserRepository: Send + Sync + 'static {
    type T: UsesUserRepository;
    fn user_repository(&self) -> &Self::T;
}

pub trait UsesUserService: Send + Sync + 'static {
    fn find_user(&self, id: String) -> Result<Option<User>>;
    fn deactivate_user(&self, id: String) -> Result<()>;
}

pub trait UserService: ProvidesUserRepository {}

impl<T: UserService> UsesUserService for T {
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

pub trait ProvidesUserService: Send + Sync + 'static {
    type T: UsesUserService;
    fn user_service(&self) -> &Self::T;
}

pub struct AppModule;

impl AppModule {
    pub fn new() -> Self {
        Self
    }
}

// 本来は下記実装を用意する必要があるが、マクロによって置き換え済み。

// impl Database for AppModule {}
// impl UserRepository for AppModule {}
// impl UserService for AppModule {}

// impl ProvidesDatabase for AppModule {
//     type T = Self;
//     fn database(&self) -> &Self::T {
//         self
//     }
// }

// impl ProvidesUserRepository for AppModule {
//     type T = Self;
//     fn user_repository(&self) -> &Self::T {
//         self
//     }
// }

// impl ProvidesUserService for AppModule {
//     type T = Self;
//     fn user_service(&self) -> &Self::T {
//         self
//     }
// }

build_container!(Database, AppModule);
build_container!(UserRepository, AppModule);
build_container!(UserService, AppModule);

provide!(ProvidesDatabase, database, AppModule);
provide!(ProvidesUserRepository, user_repository, AppModule);
provide!(ProvidesUserService, user_service, AppModule);

#[macro_export]
macro_rules! build_container {
    // trait = どのトレイトを依存注入の対象とするか
    // module = どのモジュールに紐付けするか
    ($trait:ident, $module:ident) => {
        impl $trait for $module {}
    };
}

#[macro_export]
macro_rules! provide {
    // provider = 指定対象とする Provides トレイト
    // accessor = なんというメソッドとして呼び出しできるようにするか
    // module = どのモジュールに紐付けするか
    ($provider:ident, $accessor:tt, $module:ident) => {
        impl $provider for $module {
            type T = Self;
            fn $accessor(&self) -> &Self::T {
                self
            }
        }
    };
}

pub mod router {
    use actix_web::{get, web::Data, web::Path, HttpResponse};

    use crate::{ProvidesUserService, UsesUserService};

    #[get("/users/{id}")]
    pub async fn find_user(id: Path<String>, app_module: Data<crate::AppModule>) -> HttpResponse {
        let user = app_module.user_service().find_user(id.into_inner());
        match user {
            Ok(Some(user)) => HttpResponse::Ok().json(user),
            Ok(None) => HttpResponse::NotFound().finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }
}
