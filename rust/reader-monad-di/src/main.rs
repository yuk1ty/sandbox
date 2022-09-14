struct Repo;

impl Repo {
    fn make_user(&self, user: User) -> i32 {
        1
    }

    fn find_user(&self, id: i32) -> User {
        User {}
    }
}

#[derive(Debug)]
struct User;

fn create_user(user: User) -> impl FnOnce(&Repo) -> i32 {
    |repo: &Repo| repo.make_user(user)
}

fn find_user(id: i32) -> impl FnOnce(&Repo) -> User {
    move |repo: &Repo| {
        let user = repo.find_user(id);
        let n2 = create_user(user)(repo);
        let user = repo.find_user(n2);
        user
    }
}

fn main() {
    let repo = Repo {};
    let user = User {};
    let id = create_user(user)(&repo);
    let user = find_user(id)(&repo);
    println!("{:?}", user);
}
