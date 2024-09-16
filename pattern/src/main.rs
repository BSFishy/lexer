use pattern::{Lexer, Token};

fn main() {
    // let mut c = "//r/sdf".chars().peekable();
    // let token = Token::lex(&mut c);
    // println!("{:?}", token);
    //
    // let token = Token::lex(&mut c);
    // println!("{:?}", token);
    //
    // for c in c {
    //     print!("{}", c);
    // }
    // println!();
    let c = "//r/sdf";
    let lexer = Lexer::new(c.as_bytes());
    for token in lexer {
        println!("{:?}", token);
    }
}
