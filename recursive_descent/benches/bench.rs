use common::{Token, CODE};
use criterion::{criterion_group, criterion_main, Criterion};
use recursive_descent::{LexError, Lexer};
use std::hint::black_box;

fn lex(text: &'static str) -> Vec<Result<Token, LexError>> {
    Lexer::new(text.as_bytes()).collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("recursive descent lex", |b| b.iter(|| lex(black_box(CODE))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
