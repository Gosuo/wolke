use criterion::{Criterion, criterion_group, criterion_main};
use wolke::io::PcdReader;

pub fn parsing_benchmark(c: &mut Criterion) {
    let bytes = include_bytes!("../data/example.pcd");
    let reader = PcdReader::from_bytes(bytes.to_vec());
    c.bench_function("pcd ascii example parser", |b| b.iter(|| reader.parse().is_ok()));
}

criterion_group!(benches, parsing_benchmark);
criterion_main!(benches);