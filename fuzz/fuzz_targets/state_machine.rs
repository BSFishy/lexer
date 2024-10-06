#![no_main]

use libfuzzer_sys::fuzz_target;
use state_machine::Lexer;

fuzz_target!(|data: &[u8]| {
    let _ = Lexer::new(data).collect::<Vec<_>>();
});
