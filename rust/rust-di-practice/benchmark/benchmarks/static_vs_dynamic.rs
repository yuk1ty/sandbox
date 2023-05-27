use criterion::{criterion_group, criterion_main, Criterion};

fn call_static_method(c: &mut Criterion) {
    let app_module = static_constructor_di::AppModule::new();
    let service = app_module.user_service();
    c.bench_function("static_dispatch", |b| {
        b.iter(|| service.find_user("1".to_string()))
    });
}

fn call_dynamic_method(c: &mut Criterion) {
    let app_module = dynamic_constructor_di::AppModule::new();
    let service = app_module.user_service();
    c.bench_function("dynamic_dispatch", |b| {
        b.iter(|| service.find_user("1".to_string()))
    });
}

criterion_group!(benches, call_static_method, call_dynamic_method);
criterion_main!(benches);
