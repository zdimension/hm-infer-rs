mod scheme;

use scheme::SExpr;
use scheme::SExpr::*;

use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use itertools::Itertools;
use typed_arena::Arena;
use BaseType::*;

#[derive(Debug)]
enum BaseType<'a>
{
    TypeVariable(Cell<Option<&'a BaseType<'a>>>),
    TypeOperator(String, Vec<&'a BaseType<'a>>),
}

impl <'a> Display for BaseType<'a>
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result
    {
        let letter = Cell::new(b'a');
        let mut vars: HashMap<&BaseType, char> = HashMap::new();
        fn aux<'a, 'b>(t: &'a BaseType<'b>, letter: &Cell<u8>, vars: &mut HashMap<&'a BaseType<'b>, char>, f: &mut Formatter) -> std::fmt::Result
        {
            let t = resolve(t);
            match t
            {
                TypeVariable(_) =>
                    {
                        let x = vars.entry(t).or_insert_with(||
                            {
                                let c = letter.get();
                                letter.set(c + 1);
                                c as char
                            });
                        write!(f, "{}{}", x, t as *const BaseType as usize)
                    }
                TypeOperator(name, args) =>
                    {
                        match name.as_str()
                        {
                            "->" => write!(f, "({} -> {})", args[0], args[1]),
                            "*" => write!(f, "({})", args.iter().format(" * ")),
                            _ => if args.is_empty() { write!(f, "{}", name) } else { write!(f, "({} {})", name, args.iter().format(" ")) }
                        }
                    }
            }
        }

        aux(self, &letter, &mut vars, f)
    }
}

impl PartialEq for &BaseType<'_>
{
    fn eq(&self, other: &Self) -> bool
    {
        std::ptr::eq(self, other)
    }
}

impl Eq for &BaseType<'_> {}

impl Hash for &BaseType<'_>
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H)
    {
        std::ptr::hash(self, state)
    }
}

fn resolve<'a, 'b>(ty: &'a BaseType<'b>) -> &'a BaseType<'b>
{
    match ty
    {
        TypeVariable(cell) =>
            {
                if let Some(t) = cell.get()
                {
                    resolve(t)
                } else {
                    ty
                }
            }
        _ => ty
    }
}

fn contains<'a>(needle: &'a BaseType<'a>, haystack: &'a BaseType<'a>) -> bool
{
    match haystack
    {
        TypeVariable(_) =>
            {
                std::ptr::eq(resolve(needle), resolve(haystack))
            }
        TypeOperator(_, args) =>
            {
                if std::ptr::eq(resolve(needle), haystack)
                {
                    true
                } else {
                    args.iter().any(|arg| contains(needle, arg))
                }
            }
    }
}

fn unify<'a>(t1: &'a BaseType<'a>, t2: &'a BaseType<'a>)
{
    let (t1, t2) = (resolve(t1), resolve(t2));
    match (t1, t2)
    {
        (TypeVariable(cell1), _) =>
            {
                if !std::ptr::eq(t1, t2)
                {
                    if contains(t1, t2)
                    {
                        panic!("recursive unification between '{}' and '{}'", t1, t2);
                    }
                    println!("unifying '{}' and '{}'", t1 as *const BaseType as usize, t2 as *const BaseType as usize);
                    cell1.set(Some(t2));
                }
            }
        (_, TypeVariable(_)) =>
            {
                unify(t2, t1);
            }
        (TypeOperator(name1, args1), TypeOperator(name2, args2)) =>
            {
                if name1 != name2
                {
                    panic!("can't unify different types '{}' and '{}'", t1, t2);
                }
                if args1.len() != args2.len()
                {
                    panic!("type operator arity mismath between '{}' and '{}'", t1, t2);
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter())
                {
                    unify(arg1, arg2);
                }
            }
    }
}

fn duplicate<'a>(
    arena: &'a Arena<BaseType<'a>>,
    ty: &'a BaseType<'a>,
    map: &mut HashMap<&'a BaseType<'a>, &'a BaseType<'a>>,
    ngen: &[&'a BaseType<'a>]) -> &'a BaseType<'a>
{
    let ty: &BaseType = resolve(ty);
    match ty
    {
        TypeVariable(_) =>
            {
                if ngen.iter().any(|t| contains(ty, t))
                {
                    ty
                } else {
                    println!("{:?}", map);
                    *map.entry(ty).or_insert_with(|| arena.alloc(TypeVariable(Cell::new(None))))
                }
            }
        TypeOperator(name, args) =>
            {
                arena.alloc(TypeOperator(
                    name.clone(),
                    args.iter().map(|arg| duplicate(arena, arg, map, ngen)).collect()))
            }
    }
}

struct Environment<'a>
{
    int_type: &'a BaseType<'a>,
    bool_type: &'a BaseType<'a>,
    str_type: &'a BaseType<'a>,
    unit_type: &'a BaseType<'a>,
    arena: &'a Arena<BaseType<'a>>
}

fn analyze<'a>(expr: &SExpr, env: &Environment<'a>, syms: &HashMap<&str, &'a BaseType<'a>>) -> &'a BaseType<'a>
{
    fn aux<'a>(expr: &SExpr, env: &Environment<'a>, syms: &HashMap<&str, &'a BaseType<'a>>, ngen: &Vec<&'a BaseType<'a>>) -> &'a BaseType<'a>
    {
        //println!("aux {}", expr);
        let find_sym = |name|
            {
                match syms.get(name)
                {
                    Some(t) => t,
                    None => panic!("undefined symbol: {}", name)
                }
            };

        let this = |expr| aux(expr, env, syms, ngen);

        match expr
        {
            Symbol(name) => duplicate(env.arena, find_sym(name.as_str()), &mut HashMap::new(), ngen),
            Int(_) => env.int_type,
            Bool(_) => env.bool_type,
            Str(_) => env.str_type,
            List(list) => {
                let head = list.first().ok_or("expected head").unwrap();

                match head
                {
                    //todo

                    Symbol(n) if n == "let" =>
                        {
                            let mut newenv = syms.clone();
                            for binding in match list.get(1) { Some(List(items)) => items, _ => panic!("let: expected list of bindings") }
                            {
                                if let List(items) = &binding
                                {
                                    if let [Symbol(name), val] = &items[..]
                                    {
                                        newenv.insert(name.as_str(), this(val));
                                    }
                                    else
                                    {
                                        panic!("let: expected binding of the form (name value)");
                                    }
                                }
                            }
                            let body = list.get(2).ok_or("let: expected body").unwrap();
                            aux(body, env, &newenv, ngen)
                        },
                    Symbol(n) if n == "let*" =>
                        {
                            let bindings = match list.get(1) { Some(List(items)) => items, _ => panic!("let: expected list of bindings") };
                            let body = list.get(2).ok_or("let*: expected body").unwrap();
                            if bindings.is_empty()
                            {
                                this(body)
                            }
                            else
                            {
                                let (head, rest) = bindings.split_at(1);
                                this(&List(vec![
                                    Symbol("let".into()), List(head.to_vec()),
                                    List(vec![Symbol("let*".into()), List(rest.to_vec()), body.clone()])]))
                            }
                        },
                    Symbol(n) if n == "letrec" =>
                        {
                            let mut newenv = syms.clone();
                            let mut ftypes = Vec::new();
                            for binding in match list.get(1) { Some(List(items)) => items, _ => panic!("letrec: expected list of bindings") }
                            {
                                if let List(items) = &binding
                                {
                                    if let [Symbol(name), val] = &items[..]
                                    {
                                        let typevar: &BaseType = env.arena.alloc(TypeVariable(Cell::new(None)));
                                        ftypes.push((val, typevar));
                                        newenv.insert(name.as_str(), typevar);
                                    }
                                    else
                                    {
                                        panic!("letrec: expected binding of the form (name value)");
                                    }
                                }
                            }

                            for (val, typevar) in &ftypes
                            {
                                unify(typevar, aux(val, env, &newenv, &ftypes.iter().map(|(_, t)| *t).collect_vec()));
                            }

                            let body = list.get(2).ok_or("letrec: expected body").unwrap();
                            aux(body, env, &newenv, ngen)
                        },
                    Symbol(n) if n == "lambda" =>
                        {
                            let (head, rest) = (match list.get(1) { Some(List(items)) => items, _ => panic!("lambda: expected list of parameters") }).split_at(1);
                            let body = list.get(2).ok_or("lambda: expected body").unwrap();
                            if rest.is_empty()
                            {
                                let ptype = env.arena.alloc(TypeVariable(Cell::new(None)));
                                let mut newenv = syms.clone();
                                newenv.insert(match &head[0] { Symbol(name) => name.as_str(), _ => panic!("lambda: expected identifier") }, ptype);
                                env.arena.alloc(TypeOperator("->".to_string(), vec![ptype, aux(body, env, &newenv, ngen)]))
                            }
                            else
                            {
                                this(&List(vec![Symbol("lambda".into()), List(head.to_vec()),
                                               List(vec![Symbol("lambda".into()), List(rest.to_vec()), body.clone()])]))
                            }
                        },
                    f => {
                        let arg = list.get(1).ok_or("expected arg").unwrap();
                        let rest = &list[2..];
                        if rest.is_empty()
                        {
                            let val = this(f);
                            let rettype = env.arena.alloc(TypeVariable(Cell::new(None)));
                            let argtype = this(arg);
                            let functype = env.arena.alloc(TypeOperator(
                                "->".to_string(),
                                vec![argtype, rettype]));
                            unify(functype, val);
                            rettype
                        } else {
                            let inner = List(vec![f.clone(), arg.clone()]);
                            this(&List(vec![inner].iter().chain(rest).cloned().collect()))
                        }
                    }
                }
            }
        }
    }

    aux(expr, env, syms, &Vec::new())
}


fn main()
{
    let arena = Arena::new();

    let op = |name: &str, params| arena.alloc(TypeOperator(name.to_string(), params));
    let var = || arena.alloc(TypeVariable(Cell::new(None)));
    fn fn_type<'a, 'b>(arena: &'a Arena<BaseType<'a>>, args: &'b [&'a BaseType<'a>]) -> &'a BaseType<'a>
    {
        if args.len() == 1
        {
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
        unit_type: unit
    };


    let tests: SExpr = r#"(
    pair
    (error "this returns the bottom type (forall a. a)")
    (lambda (x) (if (zero x) (error "divide by zero") (/ 1 x)))
    (pair 6)
    (lambda (f) (f 5))
    (lambda (f) f)
    (lambda (x y) x)
    ((lambda (x y) x) #t 6)
    (let ([five 5]) (let ([g (lambda (f) f)]) (g five)))
    (let ([f (lambda (x) x)]) (pair (f 4) (f #t)))
    (lambda (f) (f f))
    (let ([g (lambda (f) 5)]) (g g))
    (lambda (x) (pair x x))
    ((lambda (g) (pair 9 g)) 5)
    (cdr ((lambda (g) (pair 9 g)) 5))
    (lambda (x y) (pair y x))
    (lambda (g) (let ((f (lambda (x) g))) (pair (f 3) (f #t))))
    (lambda (f) (lambda (g) (lambda (arg) (g (f arg)))))
    (letrec ((factorial (lambda (n) (if (zero n) 1 (* n (factorial (- n 1))))))) (factorial 5))
    (lambda (l) (letrec ((length (lambda (l) (if (null? l) 0 (+ 1 (length (tl l))))))) (length l)))
    (tl (cons 5 nil))
    (let ([x (just 123)])
        (maybe x (= 123)))
    (let ([x nothing])
        (maybe x (= 123)))
    (let ([x (left 5)])
        (either x (= 123) (lambda (bool) 456)))
    (let* (
             [kons (lambda (a b) (lambda (f) (f a b)))]
             [kar (lambda (p) (p (lambda (a d) a)))]
             [kdr (lambda (p) (p (lambda (a d) d)))]
             [test (kons 8 #t)]
             )
        (pair (kar test) (kdr test)))
    (let (
            [multiple (lambda (k) (lambda(x) (= (modulo x k) 0)))]
            [singleton =]
            [union (lambda (a b) (lambda (x) (or (a x) (b x))))]
            [in? (lambda (n ens) (ens n))]
            )
        (in? 1 (union (multiple 5) (singleton 2))))
    )"#.parse().unwrap();
    let tests = "(pair)".parse().unwrap();

    if let SExpr::List(ref list) = tests
    {
        for expr in list
        {
            println!("{} {}", expr, analyze(expr, &env, &syms));
        }
    }
    else
    {
        println!("how");
    }
}
