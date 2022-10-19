#[derive(Clone)]
struct Context {
    param: String,
    id: u32,
}

trait FromContext {
    /// Extracts data from the `Context` struct.
    /// This should be implemented each field in `Context`.
    fn from_context(context: &Context) -> Self;
}

pub struct Param(pub String);

impl FromContext for Param {
    fn from_context(context: &Context) -> Self {
        Param(context.param.clone())
    }
}

pub struct Id(pub u32);

impl FromContext for Id {
    fn from_context(context: &Context) -> Self {
        Id(context.id)
    }
}

trait Handler<T> {
    fn call(self, context: Context);
}

impl<F, T> Handler<T> for F
where
    F: Fn(T),
    T: FromContext,
{
    fn call(self, context: Context) {
        (self)(T::from_context(&context))
    }
}

impl<F, T1, T2> Handler<(T1, T2)> for F
where
    F: Fn(T1, T2),
    T1: FromContext,
    T2: FromContext,
{
    fn call(self, context: Context) {
        // 1. Extract each field value from Context.
        // 2. Pass them to the function (self).
        // 3. Apply the function.
        (self)(T1::from_context(&context), T2::from_context(&context))
    }
}

fn trigger<T, H>(context: Context, handler: H)
where
    H: Handler<T>,
{
    handler.call(context)
}

fn find_user(Id(id): Id) {
    println!("find user by {}", id);
}

fn params_and_id(Param(param): Param, Id(id): Id) {
    println!("param: {}, id: {}", param, id);
}

fn main() {
    let context = Context {
        param: "parameter!".to_string(),
        id: 42,
    };
    trigger(context.clone(), find_user);
    trigger(context.clone(), params_and_id);
}
