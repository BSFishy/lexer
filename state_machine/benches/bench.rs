use common::CODE;
use criterion::{criterion_group, criterion_main, Criterion};
use state_machine::{LexError, Lexer, Token};
use std::hint::black_box;

fn lex(text: &str) -> Vec<Result<Token, LexError>> {
    Lexer::new(text.as_bytes()).collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("state machine lex", |b| b.iter(|| lex(black_box(CODE))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
