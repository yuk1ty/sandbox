use anyhow::Result;
use common::User;
use std::sync::Arc;

#[derive(Clone)]
pub struct Reader<'a, E, A> {
    // もし並行処理安全性を求めないなら、Rc にする手もある。
    // 他の実装との整合性の兼ね合いで Arc を利用している。
    run: Arc<dyn Fn(E) -> A + 'a>,
}

impl<'a, E: 'a + Clone, A: 'a> Reader<'a, E, A> {
    pub fn pure<F>(f: F) -> Reader<'a, E, A>
    where
        F: Fn(E) -> A + 'a,
    {
        Reader { run: Arc::new(f) }
    }

    pub fn flat_map<B, F>(self, f: F) -> Reader<'a, E, B>
    where
        F: 'a + Fn(A) -> Reader<'a, E, B>,
        B: 'a,
    {
        Reader {
            run: Arc::new(move |env| (f((*self.run)(env.clone()))).run(env)),
        }
    }

    pub fn map<B, F>(self, f: F) -> Reader<'a, E, B>
    where
        F: 'a + Fn(A) -> B,
        B: 'a,
    {
        Reader {
            run: Arc::new(move |env| f((*self.run)(env))),
        }
    }

    pub fn local<F>(self, f: F) -> Reader<'a, E, A>
    where
        F: 'a + Fn(E) -> E,
    {
        Reader {
            run: Arc::new(move |env| (*self.run)(f(env))),
        }
    }

    pub fn run(&self, env: E) -> A {
        (self.run)(env)
    }
}

pub fn ask<'a, E: 'a + Clone>() -> Reader<'a, E, E> {
    Reader::pure(|env| env)
}

pub fn local<'a, E, A, F>(f: F, reader: Reader<'a, E, A>) -> Reader<'a, E, A>
where
    F: 'a + Fn(E) -> E,
    E: 'a + Clone,
    A: 'a,
{
    Reader {
        run: Arc::new(move |e: E| (*reader.run)(f(e))),
    }
}

#[test]
fn test_reader_flat_map() {
    // Basic idea is from https://www.scalawithcats.com/dist/scala-with-cats.html

    #[derive(Clone)]
    struct Cat {
        name: String,
        favourite_food: String,
    }

    impl Cat {
        pub fn new(name: impl Into<String>, favourite_food: impl Into<String>) -> Cat {
            Cat {
                name: name.into(),
                favourite_food: favourite_food.into(),
            }
        }
    }

    let cat_name = Reader::pure(|cat: Cat| cat.name);
    let ans = cat_name.run(Cat::new("Garfield", "lasagne"));
    assert_eq!(ans, "Garfield");

    let greet_kitty = cat_name.map(|name| format!("Hello {}!", name));
    let ans = greet_kitty.run(Cat::new("Heathcliff", "junk food"));
    assert_eq!(ans, "Hello Heathcliff!");

    let feed_kitty = Reader::pure(|cat: Cat| format!("Have a nice bowl of {}", cat.favourite_food));
    let greet_and_feed = greet_kitty.flat_map(move |greeting| {
        feed_kitty
            .clone()
            .map(move |meal| format!("{}, {}", greeting, meal))
    });

    let ans = greet_and_feed.run(Cat::new("Garfield", "lasagne"));
    assert_eq!(ans, "Hello Garfield!, Have a nice bowl of lasagne");
}

pub struct UserService;

impl UserService {
    pub fn find_user<'a>(&self, id: String) -> Reader<'a, Arc<AppModule>, Result<Option<User>>> {
        // ask().flat_map(move |module: Arc<AppModule>| module.user_repository.find_user(id.clone()))
        mdo! {
            module <- ask::<Arc<AppModule>>();
            ret module.user_repository.find_user(id.clone())
        }
    }

    pub fn deactivate_user<'a>(&self, id: String) -> Reader<'a, Arc<AppModule>, Result<()>> {
        // ask()
        //     .flat_map(move |module: Arc<AppModule>| module.user_repository.find_user(id.clone()))
        //     .flat_map(|user| {
        //         if let Ok(Some(user)) = user {
        //             ask().flat_map(move |module: Arc<AppModule>| {
        //                 let mut user = user.clone();
        //                 user.effective = false;
        //                 module.user_repository.update(user)
        //             })
        //         } else {
        //             Reader::pure(|_| Ok(()))
        //         }
        //     })
        mdo! {
            module <- ask::<Arc<AppModule>>();
            user <- module.user_repository.find_user(id.clone());
            ret if let Ok(Some(user)) = user {
                module.user_repository.update(user)
            } else {
                Reader::pure(|_| Ok(()))
            }
        }
    }
}

pub trait UserRepository: Send + Sync + 'static {
    fn find_user<'a>(&self, id: String) -> Reader<'a, Arc<AppModule>, Result<Option<User>>>;

    fn update<'a>(&self, user: User) -> Reader<'a, Arc<AppModule>, Result<()>>;
}

#[derive(Clone)]
pub struct UserRepositoryImpl;

impl UserRepository for UserRepositoryImpl {
    fn find_user<'a>(&self, id: String) -> Reader<'a, Arc<AppModule>, Result<Option<User>>> {
        ask().map(move |module: Arc<AppModule>| module.database.find_user(id.clone()))
    }

    fn update<'a>(&self, user: User) -> Reader<'a, Arc<AppModule>, Result<()>> {
        self.find_user(user.id).flat_map(|user| {
            if let Ok(Some(user)) = user {
                Reader::pure(move |module: Arc<AppModule>| module.database.update(user.clone()))
            } else {
                Reader::pure(|_| Ok(()))
            }
        })
    }
}

#[derive(Clone)]
pub struct Database;

impl Database {
    pub fn find_user<'a>(&self, id: String) -> Result<Option<User>> {
        Ok(Some(User {
            id: "id-a".to_string(),
            effective: true,
        }))
    }

    pub fn update<'a>(&self, user: User) -> Result<()> {
        Ok(println!("updated user: {:?}", user))
    }
}

pub struct AppModule {
    pub user_service: UserService,
    pub user_repository: Arc<dyn UserRepository>,
    pub database: Database,
}

impl AppModule {
    pub fn new() -> AppModule {
        let database = Database;
        let user_repository = Arc::new(UserRepositoryImpl);
        let user_service = UserService;

        AppModule {
            user_service,
            user_repository,
            database,
        }
    }
}

#[macro_export]
macro_rules! mdo {
    ($i:ident <- $e:expr; $($rest:tt)*) => {
        $e.flat_map(move |$i| mdo!($($rest)*))
    };
    (_ <- $e:expr; $($rest:tt)*) => {
        $e.flat_map(move |_| mdo!($($rest)*))
    };
    (ret $e:expr) => {
        $e
    };
}

pub mod router {
    use actix_web::{get, web::Data, web::Path, HttpResponse};

    #[get("/users/{id}")]
    pub async fn find_user(id: Path<String>, app_module: Data<crate::AppModule>) -> HttpResponse {
        // UserService の依存は AppModule ではなく実はもう一階層上のモジュールから渡されるべきかもしれない。
        let user = app_module
            .user_service
            .find_user(id.into_inner())
            .run(app_module.into_inner());
        match user {
            Ok(Some(user)) => HttpResponse::Ok().json(user),
            Ok(None) => HttpResponse::NotFound().finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }
}
