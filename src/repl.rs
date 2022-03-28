use crate::build_env;
use crate::SExpr;
use std::io;
use std::io::Write;

pub fn launch_repl()
{
    build_env!({
        loop
        {
            let mut input = String::new();
            print!(">>> ");
            io::stdout().flush().unwrap();
            match io::stdin().read_line(&mut input) {
                Ok(n) => {
                    match input.parse().map(|e: SExpr| env.analyze(&e, &mut syms)) {
                        Ok(Ok(res)) => println!("{}", res),
                        Ok(Err(msg)) => println!("analysis error: {}", msg),
                        Err(e) => println!("parse error: {}", e),
                    }
                },
                Err(error) => break
            }
        }
    });
}