use benchmark_murmur_xxhash::{hash_with_murmur3, hash_with_xxhash};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn criterion_murmur3(c: &mut Criterion) {
    c.bench_function("murmur3", |b| {
        b.iter(|| hash_with_murmur3(black_box("user_a")))
    });
}

fn criterion_xx(c: &mut Criterion) {
    c.bench_function("xxhash", |b| b.iter(|| hash_with_xxhash("user_a")));
}

criterion_group!(benches, criterion_murmur3, criterion_xx);
criterion_main!(benches);
