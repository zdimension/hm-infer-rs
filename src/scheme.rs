use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::{CharIndices, FromStr};

use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum SExpr {
    Symbol(String),
    Int(i32),
    Bool(bool),
    Str(String),
    List(Vec<SExpr>),
}

struct SchemeParser<'a>(&'a str, Peekable<CharIndices<'a>>);

#[derive(Debug)]
pub enum ReadError {
    EOFFound,
    CharacterExpected(char, Option<char>),
    EOFExpected(char),
    IntParseError,
    BoolParseError(Option<char>),
}

impl Display for ReadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ReadError::EOFFound => write!(f, "expected input but found EOF"),
            ReadError::CharacterExpected(c, Some(next)) => {
                write!(f, "expected '{}' but found '{}'", c, next)
            }
            ReadError::CharacterExpected(c, None) => {
                write!(f, "expected '{}' but found EOF", c)
            }
            ReadError::EOFExpected(c) => write!(f, "expected EOF but found '{}'", c),
            ReadError::IntParseError => write!(f, "invalid integer"),
            ReadError::BoolParseError(Some(next)) => {
                write!(f, "expected '#t' or '#f' but found '{}'", next)
            },
            ReadError::BoolParseError(None) => write!(f, "got EOF while parsing boolean"),
        }
    }
}

impl<'a> SchemeParser<'a> {
    fn new(s: &'a str) -> Self {
        SchemeParser(s, s.char_indices().peekable())
    }

    fn accept(&mut self, c: char) -> bool {
        match self.1.peek() {
            Some(&(_, ch)) if ch == c => {
                self.1.next();
                true
            }
            _ => false,
        }
    }

    fn expect(&mut self, c: char) -> Result<(), ReadError> {
        if self.accept(c) {
            Ok(())
        } else {
            let x = self.1.peek().unwrap().0;
            println!("{:?}", &self.0[x..]);
            Err(ReadError::CharacterExpected(
                c,
                self.1.peek().map(|&(_, ch)| ch),
            ))
        }
    }

    fn skip_while(&mut self, p: impl Fn(char) -> bool) {
        while let Some(&(_, ch)) = self.1.peek() {
            if !p(ch) {
                break;
            }
            self.1.next();
        }
    }

    fn current_pos(&mut self) -> usize {
        self.1.peek().map(|&(pos, _)| pos).unwrap_or(self.0.len())
    }

    fn read_number(&mut self) -> Result<SExpr, ReadError> {
        let start = self.current_pos();
        self.skip_while(|c| c.is_digit(10));
        let end = self.current_pos();
        let s = &self.0[start..end];
        s.parse()
            .map(SExpr::Int)
            .map_err(|_| ReadError::IntParseError)
    }

    fn read_boolean(&mut self) -> Result<SExpr, ReadError> {
        self.expect('#')?;
        if self.accept('t') {
            Ok(SExpr::Bool(true))
        } else if self.accept('f') {
            Ok(SExpr::Bool(false))
        } else {
            Err(ReadError::BoolParseError(self.1.peek().map(|&(_, ch)| ch)))
        }
    }

    fn read_symbol(&mut self) -> Result<SExpr, ReadError> {
        let start = self.current_pos();
        self.skip_while(|c| {
            c != '(' && c != ')' && c != '[' && c != ']' && !char::is_whitespace(c)
        });
        let end = self.current_pos();
        let s = &self.0[start..end];
        Ok(SExpr::Symbol(s.to_string()))
    }

    fn read_string(&mut self) -> Result<SExpr, ReadError> {
        self.expect('"')?;
        let start = self.current_pos();
        self.skip_while(|c| c != '"');
        let end = self.current_pos();
        self.expect('"')?;
        let s = &self.0[start..end];
        Ok(SExpr::Str(s.to_string()))
    }

    fn read_list(&mut self) -> Result<SExpr, ReadError> {
        let closing = if self.accept('(') {
            ')'
        } else if self.accept('[') {
            ']'
        } else {
            return Err(ReadError::CharacterExpected(
                '(',
                self.1.peek().map(|&(_, ch)| ch),
            ));
        };
        let mut values = Vec::new();
        while !self.accept(closing) {
            values.push(self.read()?);
            self.skip_while(char::is_whitespace);
        }
        Ok(SExpr::List(values))
    }

    fn read(&mut self) -> Result<SExpr, ReadError> {
        self.skip_while(char::is_whitespace);
        match self.1.peek() {
            Some(&(_, '(' | '[')) => self.read_list(),
            Some(&(_, '#')) => self.read_boolean(),
            Some(&(_, '"')) => self.read_string(),
            Some(&(_, '\'')) => {
                self.1.next();
                Ok(SExpr::List(vec![SExpr::Symbol("quote".to_string()), self.read()?]))
            }
            Some(&(_, ch)) if ch.is_digit(10) => self.read_number(),
            Some(_) => self.read_symbol(),
            None => Err(ReadError::EOFFound),
        }
    }

    fn read_whole(&mut self) -> Result<SExpr, ReadError> {
        let res = self.read();
        self.skip_while(char::is_whitespace);
        match self.1.peek() {
            Some(&(_, ch)) => Err(ReadError::EOFExpected(ch)),
            None => res
        }
    }
}

impl FromStr for SExpr {
    type Err = ReadError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SchemeParser::new(s).read_whole()
    }
}

impl Display for SExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            SExpr::Int(i) => write!(f, "{}", i),
            SExpr::Bool(b) => write!(f, "{}", b),
            SExpr::Str(s) => write!(f, "\"{}\"", s),
            SExpr::Symbol(s) => write!(f, "{}", s),
            SExpr::List(xs) => write!(f, "({})", xs.iter().format(" ")),
        }
    }
}
