use criterion::{criterion_group, criterion_main, Criterion};

pub fn parsing_benchmark(c: &mut Criterion) {
    // let bytes = include_bytes!("../data/example.pcd");
    // let reader = PcdReader::from_bytes(bytes.to_vec());
    c.bench_function("pcd ascii example parser", |b| b.iter(|| 1 + 1));
}

criterion_group!(parsing, parsing_benchmark);
criterion_main!(parsing);
