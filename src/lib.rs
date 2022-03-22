use typed_arena::Arena;

use scheme::SExpr;

mod infer;
mod scheme;
mod typing;

use typing::BaseType::*;
use typing::*;

pub struct Environment<'a> {
    pub int_type: &'a BaseType<'a>,
    pub bool_type: &'a BaseType<'a>,
    pub str_type: &'a BaseType<'a>,
    pub unit_type: &'a BaseType<'a>,
    pub arena: &'a Arena<BaseType<'a>>,
}

macro_rules! tests {
    ($($n: ident: ($a: literal, $b: literal)),*) => {
        $(
            #[test]
            fn $n() {
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

                let syms = HashMap::from([
                    ("+", ft(&[int, int, int])),
                    ("-", ft(&[int, int, int])),
                    ("*", ft(&[int, int, int])),
                    ("/", ft(&[int, int, int])),
                    ("modulo", ft(&[int, int, int])),
                    ("=", ft(&[int, int, bool])),
                    ("zero", ft(&[int, bool])),
                    ("succ", ft(&[int, int])),
                    ("pred", ft(&[int, int])),
                    ("and", ft(&[bool, bool, bool])),
                    ("or", ft(&[bool, bool, bool])),
                    ("error", ft(&[str, bottom])),
                    ("if", ft(&[bool, t1, t1, t1])),
                    ("pair", ft(&[t1, t2, op("*", vec![t1, t2])])),
                    ("car", ft(&[op("*", vec![t1, t2]), t1])),
                    ("cdr", ft(&[op("*", vec![t1, t2]), t2])),
                    ("nil", op("list", vec![t1])),
                    ("cons", ft(&[t1, op("list", vec![t1]), op("list", vec![t1])])),
                    ("hd", ft(&[op("list", vec![t1]), t1])),
                    ("tl", ft(&[op("list", vec![t1]), op("list", vec![t1])])),
                    ("null?", ft(&[op("list", vec![t1]), bool])),
                    ("map", ft(&[ft(&[t1, t2]), op("list", vec![t1]), op("list", vec![t2])])),
                    ("for-each", ft(&[ft(&[t1, unit]), op("list", vec![t1]), unit])),
                    ("left", ft(&[t1, op("either", vec![t1, t2])])),
                    ("right", ft(&[t2, op("either", vec![t1, t2])])),
                    ("either", ft(&[op("either", vec![t1, t2]), ft(&[t1, t3]), ft(&[t2, t4]), op("either", vec![t3, t4])])),
                    ("just", ft(&[t1, op("option", vec![t1])])),
                    ("nothing", op("option", vec![t1])),
                    ("maybe", ft(&[op("option", vec![t1]), ft(&[t1, t2]), op("option", vec![t2])]))
                ]);

                let env = Environment
                {
                    arena: &arena,
                    int_type: int,
                    bool_type: bool,
                    str_type: str,
                    unit_type: unit,
                };

                let left: SExpr = $a.parse().unwrap();
                assert_eq!(analyze(&left, &env, &syms).to_string(), $b);
            }
        )*
    }
}

mod tests {
    use super::*;
    use infer::analyze;
    use scheme::SExpr;
    use std::cell::Cell;
    use std::collections::HashMap;

    tests![
        pair_def: ("pair", "(a -> (b -> (a * b)))"),
        bottom_1: ("(error \"this returns the bottom type (forall a. a)\")", "a"),
        bottom_2: ("(lambda (x) (if (zero x) (error \"divide by zero\") (/ 1 x)))", "(int -> int)"),
        pair_partial: ("(pair 6)", "(a -> (int * a))"),
        impl_func: ("(lambda (f) (f 5))", "((int -> a) -> a)"),
        identity: ("(lambda (f) f)", "(a -> a)"),
        first_arg: ("(lambda (x y) x)", "(a -> (b -> a))"),
        first_arg_call: ("((lambda (x y) x) #t 6)", "bool"),
        nested_let: ("(let ((five 5)) (let ((g (lambda (f) f))) (g five)))", "int"),
        pair_comp: ("(let ((f (lambda (x) x))) (pair (f 4) (f #t)))", "(int * bool)"),
        self_call: ("(let ((g (lambda (f) 5))) (g g))", "int"),
        pair_same: ("(lambda (x) (pair x x))", "(a -> (a * a))"),
        pair_lambda: ("((lambda (g) (pair 9 g)) 5)", "(int * int)"),
        pair_lambda_cdr: ("(cdr ((lambda (g) (pair 9 g)) 5))", "int"),
        pair_rev: ("(lambda (x y) (pair y x))", "(a -> (b -> (b * a)))"),
        pair_same_func: ("(lambda (g) (let ((f (lambda (x) g))) (pair (f 3) (f #t))))", "(a -> (a * a))"),
        compose: ("(lambda (f) (lambda (g) (lambda (arg) (g (f arg)))))", "((a -> b) -> ((b -> c) -> (a -> c)))"),
        letrec_fact: ("(letrec ((factorial (lambda (n) (if (zero n) 1 (* n (factorial (- n 1))))))) (factorial 5))", "int"),
        letrec_length: ("(lambda (l) (letrec ((length (lambda (l) (if (null? l) 0 (+ 1 (length (tl l))))))) (length l)))", "((list a) -> int)"),
        list_tail: ("(tl (cons 5 nil))", "(list int)"),
        maybe_just: ("(let ((x (just 123))) (maybe x (= 123)))", "(option bool)"),
        maybe_nothing: ("(let ((x nothing)) (maybe x (= 123)))", "(option bool)"),
        either_left: ("(let ((x (left 5))) (either x (= 123) (lambda (bool) 456)))", "(either bool int)"),
        kons: ("(let* ((kons (lambda (a b) (lambda (f) (f a b)))) (kar (lambda (p) (p (lambda (a d) a)))) (kdr (lambda (p) (p (lambda (a d) d)))) (test (kons 8 #t))) (pair (kar test) (kdr test)))", "(int * bool)"),
        sets: ("(let ((multiple (lambda (k) (lambda (x) (= (modulo x k) 0)))) (singleton =) (union (lambda (a b) (lambda (x) (or (a x) (b x))))) (in? (lambda (n ens) (ens n)))) (in? 1 (union (multiple 5) (singleton 2))))", "bool")
    ];
}
