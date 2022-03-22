use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use by_address::ByAddress;
use typed_arena::Arena;
use crate::{TypeOperator, TypeVariable};

pub enum BaseType<'a>
{
    TypeVariable(Cell<Option<&'a BaseType<'a>>>),
    TypeOperator(String, Vec<&'a BaseType<'a>>),
}

impl<'a> BaseType<'a>
{
    pub fn resolve(&self) -> &BaseType<'a>
    {
        match self
        {
            TypeVariable(cell) =>
                {
                    if let Some(t) = cell.get()
                    {
                        t.resolve()
                    } else {
                        self
                    }
                }
            _ => self
        }
    }

    pub fn contains(&self, needle: &'a BaseType<'a>) -> bool
    {
        let (needle, haystack) = (needle.resolve(), self.resolve());
        match haystack
        {
            TypeVariable(_) =>
                {
                    std::ptr::eq(needle, haystack)
                }
            TypeOperator(_, args) =>
                {
                    if std::ptr::eq(needle, haystack)
                    {
                        true
                    } else {
                        args.iter().any(|arg| arg.contains(needle))
                    }
                }
        }
    }

    pub fn unify(&'a self, t2: &'a BaseType<'a>)
    {
        let (t1, t2) = (self.resolve(), t2.resolve());
        match (t1, t2)
        {
            (TypeVariable(cell1), _) =>
                {
                    if !std::ptr::eq(t1, t2)
                    {
                        if t2.contains(t1)
                        {
                            panic!("recursive unification between '{}' and '{}'", t1, t2);
                        }
                        cell1.set(Some(t2));
                    }
                }
            (_, TypeVariable(_)) =>
                {
                   t2.unify(t1);
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
                        arg1.unify(arg2);
                    }
                }
        }
    }

    pub fn duplicate(
        &'a self,
        arena: &'a Arena<BaseType<'a>>,
        map: &mut HashMap<ByAddress<&'a BaseType<'a>>, &'a BaseType<'a>>,
        ngen: &[&'a BaseType<'a>]) -> &'a BaseType<'a>
    {
        let ty: &BaseType = self.resolve();
        match ty
        {
            TypeVariable(_) =>
                {
                    if ngen.iter().any(|t| t.contains(ty))
                    {
                        ty
                    } else {
                        *map.entry(ByAddress(ty)).or_insert_with(|| arena.alloc(TypeVariable(Cell::new(None))))
                    }
                }
            TypeOperator(name, args) =>
                {
                    arena.alloc(TypeOperator(
                        name.clone(),
                        args.iter().map(|arg| arg.duplicate(arena, map, ngen)).collect()))
                }
        }
    }
}

impl<'a> Debug for &BaseType<'a>
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result
    {
        write!(f, "{}", self as *const _ as usize)
    }
}

impl<'a> Display for BaseType<'a>
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result
    {
        let letter = Cell::new(b'a');
        let mut vars: HashMap<ByAddress<&BaseType>, char> = HashMap::new();
        fn aux<'a, 'b>(t: &'a BaseType<'b>, letter: &Cell<u8>, vars: &mut HashMap<ByAddress<&'a BaseType<'b>>, char>, f: &mut Formatter) -> std::fmt::Result
        {
            let t = t.resolve();
            match t
            {
                TypeVariable(_) =>
                    {
                        let x = vars.entry(ByAddress(t)).or_insert_with(||
                            {
                                let c = letter.get();
                                letter.set(c + 1);
                                c as char
                            });
                        write!(f, "{}", x)
                    }
                TypeOperator(name, args) =>
                    {
                        match name.as_str()
                        {
                            "->" => {
                                write!(f, "(")?;
                                aux(args[0], letter, vars, f)?;
                                write!(f, " -> ")?;
                                aux(args[1], letter, vars, f)?;
                                write!(f, ")")
                            }
                            "*" => {
                                write!(f, "(")?;
                                aux(args[0], letter, vars, f)?;
                                for arg in args.iter().skip(1)
                                {
                                    write!(f, " * ")?;
                                    aux(arg, letter, vars, f)?;
                                }
                                write!(f, ")")
                            }
                            _ => if args.is_empty() { write!(f, "{}", name) } else {
                                write!(f, "({}", name)?;
                                for arg in args.iter()
                                {
                                    write!(f, " ")?;
                                    aux(arg, letter, vars, f)?;
                                }
                                write!(f, ")")
                            }
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


