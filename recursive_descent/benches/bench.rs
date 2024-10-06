use common::{code, Token, BENCH_LEN};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use recursive_descent::{LexError, Lexer};
use std::{hint::black_box, io::Cursor};

fn lex(text: &str) -> Vec<Result<Token, LexError>> {
    let bytes: Vec<_> = text.as_bytes().into();
    let bytes = Cursor::new(bytes);
    let lexer = Lexer::new(bytes);
    lexer.collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("recursive descent");
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
