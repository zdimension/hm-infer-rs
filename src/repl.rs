use crate::build_env;
use crate::SExpr;
use std::io;
use std::io::Write;
use crate::scheme::ReadError;

pub fn launch_repl()
{
    build_env!({
        let mut input = String::new();
        loop
        {
            print!("{}", if input.is_empty() { ">>> "} else { "... " });
            io::stdout().flush().unwrap();
            let clear = match io::stdin().read_line(&mut input) {
                Ok(n) => {
                    match input.parse().map(|e: SExpr| env.analyze(&e, &mut syms)) {
                        Ok(Ok(res)) => {println!("{}", res); true},
                        Ok(Err(msg)) => {println!("analysis error: {}", msg); true},
                        Err(ReadError::EOFFound) => {
                            false
                        },
                        Err(e) => {println!("parse error: {}", e); true},
                    }
                },
                Err(error) => break
            };
            if clear {
                input.clear();
            }
        }
    });
}