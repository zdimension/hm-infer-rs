mod tests {
    use super::*;
    use crate::scheme::SExpr;
    use std::cell::Cell;
    use crate::build_env;
    use std::collections::HashMap;

    macro_rules! tests {
        ($($n: ident: ($a: literal, $b: literal)),*) => {
            $(
                #[test]
                fn $n() {
                    build_env!({
                        let left: SExpr = $a.parse().unwrap();
                        assert_eq!(env.analyze(&left, &mut syms).unwrap().to_string(), $b);
                    });
                }
            )*
        }
    }

    tests![
        comp: ("(lambda (f g) (lambda (x) (f (g x))))", "((a -> b) -> ((c -> a) -> (c -> b)))"),
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
        sets: (r#"(let ((multiple (lambda (k) (lambda (x) (= (modulo x k) 0))))
                        (singleton =) (union (lambda (a b) (lambda (x) (or (a x) (b x)))))
                        (in? (lambda (n ens) (ens n))))
                       (in? 1 (union (multiple 5) (singleton 2))))"#, "bool"),
        begin: (r"(begin (define x 5) (define y (+ x 1)) y)", "int")
    ];
}
