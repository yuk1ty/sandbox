use anyhow::Result;
use common::User;
use std::sync::Arc;

pub struct Reader<'a, E, A> {
    run: Box<dyn Fn(E) -> A + 'a>,
}

impl<'a, E: 'a + Clone, A: 'a> Reader<'a, E, A> {
    pub fn pure<F>(f: F) -> Reader<'a, E, A>
    where
        F: Fn(E) -> A + 'a,
    {
        Reader { run: Box::new(f) }
    }

    pub fn flat_map<B, F>(self, f: F) -> Reader<'a, E, B>
    where
        F: 'a + Fn(A) -> Reader<'a, E, B>,
        B: 'a,
    {
        Reader {
            run: Box::new(move |env| (f((*self.run)(env.clone()))).run(env)),
        }
    }

    // TODO not needed?
    pub fn run(&self, env: E) -> A {
        (self.run)(env)
    }
}

#[test]
fn test_reader_flat_map() {
    let reader1 = Reader::pure(|x: i32| x + 1);
    let reader2 = Reader::pure(|x: i32| x * 2);
    // TODO
}

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
