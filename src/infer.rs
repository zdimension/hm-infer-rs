use crate::{BaseType, BaseType::*, Environment, SExpr, SExpr::*};
use std::cell::Cell;
use std::collections::HashMap;

pub fn analyze<'a>(
    expr: &SExpr,
    env: &Environment<'a>,
    syms: &HashMap<&str, &'a BaseType<'a>>,
) -> &'a BaseType<'a> {
    fn aux<'a>(
        expr: &SExpr,
        env: &Environment<'a>,
        syms: &HashMap<&str, &'a BaseType<'a>>,
        ngen: &Vec<&'a BaseType<'a>>,
    ) -> &'a BaseType<'a> {
        let find_sym = |name| match syms.get(name) {
            Some(t) => t,
            None => panic!("undefined symbol: {}", name),
        };

        let this = |expr| aux(expr, env, syms, ngen);

        match expr {
            Symbol(name) => find_sym(name.as_str()).duplicate(env.arena, &mut HashMap::new(), ngen),
            Int(_) => env.int_type,
            Bool(_) => env.bool_type,
            Str(_) => env.str_type,
            List(list) => {
                let head = list.first().ok_or("expected head").unwrap();

                match head {
                    Symbol(n) if n == "let" => {
                        let mut newenv = syms.clone();
                        for binding in match list.get(1) {
                            Some(List(items)) => items,
                            _ => panic!("let*: expected list of bindings"),
                        } {
                            if let List(items) = &binding {
                                if let [Symbol(name), val] = &items[..] {
                                    newenv.insert(name.as_str(), this(val));
                                } else {
                                    panic!("let: expected binding of the form (name value)");
                                }
                            }
                        }
                        let body = list.get(2).ok_or("let: expected body").unwrap();
                        aux(body, env, &newenv, ngen)
                    }
                    Symbol(n) if n == "let*" => {
                        let bindings = match list.get(1) {
                            Some(List(items)) => items,
                            _ => panic!("let: expected list of bindings"),
                        };
                        let body = list.get(2).ok_or("let*: expected body").unwrap();
                        if bindings.is_empty() {
                            this(body)
                        } else {
                            let (head, rest) = bindings.split_at(1);
                            this(&List(vec![
                                Symbol("let".into()),
                                List(head.to_vec()),
                                List(vec![
                                    Symbol("let*".into()),
                                    List(rest.to_vec()),
                                    body.clone(),
                                ]),
                            ]))
                        }
                    }
                    Symbol(n) if n == "letrec" => {
                        let mut newenv = syms.clone();
                        let mut ftypes = Vec::new();
                        for binding in match list.get(1) {
                            Some(List(items)) => items,
                            _ => panic!("letrec: expected list of bindings"),
                        } {
                            if let List(items) = &binding {
                                if let [Symbol(name), val] = &items[..] {
                                    let typevar: &BaseType =
                                        env.arena.alloc(TypeVariable(Cell::new(None)));
                                    ftypes.push((val, typevar));
                                    newenv.insert(name.as_str(), typevar);
                                } else {
                                    panic!("letrec: expected binding of the form (name value)");
                                }
                            }
                        }

                        for (val, typevar) in &ftypes {
                            typevar.unify(aux(
                                val,
                                env,
                                &newenv,
                                &ftypes.iter().map(|(_, t)| *t).collect(),
                            ));
                        }

                        let body = list.get(2).ok_or("letrec: expected body").unwrap();
                        aux(body, env, &newenv, ngen)
                    }
                    Symbol(n) if n == "lambda" => {
                        let (head, rest) = (match list.get(1) {
                            Some(List(items)) => items,
                            _ => panic!("lambda: expected list of parameters"),
                        })
                        .split_at(1);
                        let body = list.get(2).ok_or("lambda: expected body").unwrap();
                        if rest.is_empty() {
                            let ptype = env.arena.alloc(TypeVariable(Cell::new(None)));
                            let mut newenv = syms.clone();
                            newenv.insert(
                                match &head[0] {
                                    Symbol(name) => name.as_str(),
                                    _ => panic!("lambda: expected identifier"),
                                },
                                ptype,
                            );
                            let mut newngen = ngen.clone();
                            newngen.push(ptype);
                            env.arena.alloc(TypeOperator(
                                "->".to_string(),
                                vec![ptype, aux(body, env, &newenv, &newngen)],
                            ))
                        } else {
                            this(&List(vec![
                                Symbol("lambda".into()),
                                List(head.to_vec()),
                                List(vec![
                                    Symbol("lambda".into()),
                                    List(rest.to_vec()),
                                    body.clone(),
                                ]),
                            ]))
                        }
                    }
                    f => {
                        let arg = list.get(1).ok_or("expected arg").unwrap();
                        let rest = &list[2..];
                        if rest.is_empty() {
                            let val = this(f);
                            let rettype = env.arena.alloc(TypeVariable(Cell::new(None)));
                            let argtype = this(arg);
                            let functype = env
                                .arena
                                .alloc(TypeOperator("->".to_string(), vec![argtype, rettype]));
                            functype.unify(val);
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
