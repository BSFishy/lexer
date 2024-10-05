use backtracking::{LexError, Lexer};
use common::{Token, CODE};
use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

fn lex(text: &str) -> Vec<Result<Token, LexError>> {
    Lexer::new(text.as_bytes()).collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("backtracking lex", |b| b.iter(|| lex(black_box(CODE))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
