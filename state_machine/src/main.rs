use state_machine::Lexer;

fn main() {
    let c = "funfunc\"test\"func";
    let lexer = Lexer::new(c.as_bytes());
    for token in lexer {
        println!("{:?}", token);
    }
}
