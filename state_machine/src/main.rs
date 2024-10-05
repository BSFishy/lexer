use std::{env::args, fs::File, path::PathBuf};

use anyhow::{bail, Context, Result};
use state_machine::Lexer;

fn main() -> Result<()> {
    let file_name: Result<String> = match args().nth(1) {
        Some(file_name) => Ok(file_name),
        None => bail!("Usage: ./lexer <file>"),
    };
    let file_name = file_name?;

    let path = PathBuf::from(&file_name);
    if !path.exists() || !path.is_file() {
        bail!("{file_name} is not a file");
    }

    let file = File::open(path).context(format!("failed to open {file_name}"))?;
    let lexer = Lexer::new(file);
    for token in lexer {
        println!("{token:?}");
        // let token = token.context("failed to lex")?;
        // if let Token::Unknown(c) = token {
        //     bail!("unexpected character: {c}");
        // }
        //
        // print!("{token}");
    }

    Ok(())
}
