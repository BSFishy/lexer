use pattern::Lexer;

fn main() {
    let c = "//r/funcfunsdf";
    let lexer = Lexer::new(c.as_bytes());
    for token in lexer {
        println!("{:?}", token);
    }
}
