use typed_arena::Arena;

use scheme::SExpr;

pub mod infer;
pub mod scheme;
pub mod typing;
mod tests;
mod repl;

use typing::BaseType::*;
use typing::*;

pub use repl::launch_repl;

pub struct Environment<'a> {
    pub int_type: &'a BaseType<'a>,
    pub bool_type: &'a BaseType<'a>,
    pub str_type: &'a BaseType<'a>,
    pub unit_type: &'a BaseType<'a>,
    pub arena: &'a Arena<BaseType<'a>>,
}

#[macro_export]
macro_rules! make_env {
    ($($s:literal: $e:expr),*) => {
        HashMap::from([
            $((String::from($s), $e)),*
        ])
    }
}

#[macro_export]
macro_rules! build_env {
    ($a: stmt) => {
        use unhygienic::*;
        unhygienic!(
        {
            use crate::make_env;
            use typed_arena::Arena;
            use crate::typing::BaseType::*;
            use crate::typing::*;
            use std::cell::Cell;
            use std::collections::HashMap;
            use crate::Environment;
            let arena = Arena::new();

            let op = |name: &str, params| arena.alloc(TypeOperator(name.to_string(), params));
            let var = || arena.alloc(TypeVariable(Cell::new(None)));
            fn fn_type<'a, 'b>(arena: &'a Arena<BaseType<'a>>, args: &'b [&'a BaseType<'a>]) -> &'a BaseType<'a> {
                if args.len() == 1 {
                    args[0]
                } else {
                    let asplit = args.split_at(1);
                    arena.alloc(TypeOperator("->".to_string(), vec![asplit.0[0], fn_type(arena, asplit.1)]))
                }
            }

            let ft = |args| fn_type(&arena, args);

            let int = op("int", vec![]);
            let bool = op("bool", vec![]);
            let str = op("str", vec![]);
            let unit = op("unit", vec![]);
            let bottom = var();

            let t1 = var();
            let t2 = var();
            let t3 = var();
            let t4 = var();

            let syms = make_env![
            "+": ft(&[int, int, int]),
            "-": ft(&[int, int, int]),
            "*": ft(&[int, int, int]),
            "/": ft(&[int, int, int]),
            "modulo": ft(&[int, int, int]),
            "=": ft(&[int, int, bool]),
            "zero": ft(&[int, bool]),
            "succ": ft(&[int, int]),
            "pred": ft(&[int, int]),
            "and": ft(&[bool, bool, bool]),
            "or": ft(&[bool, bool, bool]),
            "error": ft(&[str, bottom]),
            "if": ft(&[bool, t1, t1, t1]),
            "pair": ft(&[t1, t2, op("*", vec![t1, t2])]),
            "car": ft(&[op("*", vec![t1, t2]), t1]),
            "cdr": ft(&[op("*", vec![t1, t2]), t2]),
            "nil": op("list", vec![t1]),
            "cons": ft(&[t1, op("list", vec![t1]), op("list", vec![t1])]),
            "hd": ft(&[op("list", vec![t1]), t1]),
            "tl": ft(&[op("list", vec![t1]), op("list", vec![t1])]),
            "null?": ft(&[op("list", vec![t1]), bool]),
            "map": ft(&[ft(&[t1, t2]), op("list", vec![t1]), op("list", vec![t2])]),
            "for-each": ft(&[ft(&[t1, unit]), op("list", vec![t1]), unit]),
            "left": ft(&[t1, op("either", vec![t1, t2])]),
            "right": ft(&[t2, op("either", vec![t1, t2])]),
            "either": ft(&[op("either", vec![t1, t2]), ft(&[t1, t3]), ft(&[t2, t4]), op("either", vec![t3, t4])]),
            "just": ft(&[t1, op("option", vec![t1])]),
            "nothing": op("option", vec![t1]),
            "maybe": ft(&[op("option", vec![t1]), ft(&[t1, t2]), op("option", vec![t2])])
            ];

            let env = Environment
            {
                arena: &arena,
                int_type: int,
                bool_type: bool,
                str_type: str,
                unit_type: unit,
            };

            $a;
        });
    }
}

