use criterion::{criterion_group, criterion_main, Criterion};

pub trait Action {
    fn do_something(&self, a: i32, b: i32) -> i32;
}

pub struct A;

impl Action for A {
    #[inline(never)]
    fn do_something(&self, a: i32, b: i32) -> i32 {
        a + b
    }
}

pub struct B;

impl Action for B {
    #[inline(never)]
    fn do_something(&self, a: i32, b: i32) -> i32 {
        a - b
    }
}

pub struct C;

impl Action for C {
    #[inline(never)]
    fn do_something(&self, a: i32, b: i32) -> i32 {
        a * b
    }
}

fn act<A: Action>(action: &A, a: i32, b: i32) -> i32 {
    action.do_something(a, b)
}

fn act_dyn(action: &dyn Action, a: i32, b: i32) -> i32 {
    action.do_something(a, b)
}

fn call_static_act(c: &mut Criterion) {
    let a = A;
    c.bench_function("static", |b| b.iter(|| act(&a, 1, 2)));
}

fn call_static_dyn(c: &mut Criterion) {
    let a = A;
    c.bench_function("dynamic", |b| b.iter(|| act_dyn(&a, 1, 2)));
}

fn call_multiple_dyn_act(c: &mut Criterion) {
    let acts: Vec<&dyn Action> = vec![&A, &B, &C];
    c.bench_function("multiple_dyn", |b| {
        b.iter(|| {
            for act in &acts {
                act_dyn(*act, 1, 2);
            }
        })
    });
}

criterion_group!(
    benches,
    call_static_act,
    call_static_dyn,
    call_multiple_dyn_act
);
criterion_main!(benches);
