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
                    let expr: SExpr = input.parse().unwrap();
                    println!("{}", env.analyze(&expr, &syms));
                }
                Err(error) => println!("error: {}", error),
            }
        }
    });
}