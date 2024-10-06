use backtracking::{LexError, Lexer};
use common::{code, Token, BENCH_LEN};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::hint::black_box;

fn lex(text: &str) -> Vec<Result<Token, LexError>> {
    Lexer::new(text.as_bytes()).collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("backtracking");
    for len in 0..=BENCH_LEN {
        let input = black_box(code(len));
        group.throughput(Throughput::Bytes(input.as_bytes().len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(len), &input, |b, input| {
            b.iter(|| black_box(lex(black_box(input))))
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
