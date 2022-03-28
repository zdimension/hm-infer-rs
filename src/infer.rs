use crate::{BaseType, BaseType::*, Environment, SExpr, SExpr::*};
use std::cell::Cell;
use std::collections::HashMap;

impl <'a> Environment<'a>
{
    pub fn analyze(
        &self,
        expr: &SExpr,
        syms: &HashMap<String, &'a BaseType<'a>>,
    ) -> &'a BaseType<'a> {
        fn aux<'a>(
            expr: &SExpr,
            env: &Environment<'a>,
            syms: &mut HashMap<String, &'a BaseType<'a>>,
            ngen: &Vec<&'a BaseType<'a>>,
        ) -> &'a BaseType<'a> {
            let find_sym = |name| match syms.get(name) {
                Some(t) => t,
                None => panic!("undefined symbol: {}", name),
            };

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
                                        newenv.insert(name.clone(), aux(val, env, syms, ngen));
                                    } else {
                                        panic!("let: expected binding of the form (name value)");
                                    }
                                }
                            }
                            let body = list.get(2).ok_or("let: expected body").unwrap();
                            aux(body, env, &mut newenv, ngen)
                        }
                        Symbol(n) if n == "let*" => {
                            let bindings = match list.get(1) {
                                Some(List(items)) => items,
                                _ => panic!("let: expected list of bindings"),
                            };
                            let body = list.get(2).ok_or("let*: expected body").unwrap();
                            if bindings.is_empty() {
                                aux(body, env, syms, ngen)
                            } else {
                                let (head, rest) = bindings.split_at(1);
                                aux(&List(vec![
                                    Symbol("let".into()),
                                    List(head.to_vec()),
                                    List(vec![
                                        Symbol("let*".into()),
                                        List(rest.to_vec()),
                                        body.clone(),
                                    ]),
                                ]), env, syms, ngen)
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
                                        newenv.insert(name.clone(), typevar);
                                    } else {
                                        panic!("letrec: expected binding of the form (name value)");
                                    }
                                }
                            }

                            for (val, typevar) in &ftypes {
                                typevar.unify(aux(
                                    val,
                                    env,
                                    &mut newenv,
                                    &ftypes.iter().map(|(_, t)| *t).collect(),
                                ));
                            }

                            let body = list.get(2).ok_or("letrec: expected body").unwrap();
                            aux(body, env, &mut newenv, ngen)
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
                                        Symbol(name) => name.clone(),
                                        _ => panic!("lambda: expected identifier"),
                                    },
                                    ptype,
                                );
                                let mut newngen = ngen.clone();
                                newngen.push(ptype);
                                env.arena.alloc(TypeOperator(
                                    "->".to_string(),
                                    vec![ptype, aux(body, env, &mut newenv, &newngen)],
                                ))
                            } else {
                                aux(&List(vec![
                                    Symbol("lambda".into()),
                                    List(head.to_vec()),
                                    List(vec![
                                        Symbol("lambda".into()),
                                        List(rest.to_vec()),
                                        body.clone(),
                                    ]),
                                ]), env, syms, ngen)
                            }
                        },
                        Symbol(n) if n == "begin" => {
                            let (res, stmts) = list[1..].split_last().unwrap();
                            let mut newenv = syms.clone();
                            for stmt in stmts {
                                aux(stmt, env, &mut newenv, ngen);
                            }
                            aux(res, env, &mut newenv, ngen)
                        },
                        Symbol(n) if n == "define" => {
                            let name = match list.get(1).ok_or("define: expected name") {
                                Ok(Symbol(name)) => name,
                                _ => panic!("define: expected name"),
                            };
                            let val = list.get(2).ok_or("define: expected value").unwrap();
                            let valtype = aux(val, env, syms, ngen);
                            syms.insert(name.clone(), valtype);
                            env.unit_type
                        },
                        f => {
                            let arg = list.get(1).ok_or("expected arg").unwrap();
                            let rest = &list[2..];
                            if rest.is_empty() {
                                let val = aux(f, env, syms, ngen);
                                let rettype = env.arena.alloc(TypeVariable(Cell::new(None)));
                                let argtype = aux(arg, env, syms, ngen);
                                let functype = env
                                    .arena
                                    .alloc(TypeOperator("->".to_string(), vec![argtype, rettype]));
                                functype.unify(val);
                                rettype
                            } else {
                                let inner = List(vec![f.clone(), arg.clone()]);
                                aux(&List(vec![inner].iter().chain(rest).cloned().collect()), env, syms, ngen)
                            }
                        }
                    }
                }
            }
        }

        let mut rsyms = syms.clone();
        aux(expr, self, &mut rsyms, &Vec::new())
    }

}
