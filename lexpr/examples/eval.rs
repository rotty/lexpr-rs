/// A simple S-expression evaluator.
use std::{
    collections::HashMap,
    fmt::{self, Display, Write},
    io::{self, BufRead},
};

use gc::{Finalize, Gc, GcCell};
use lexpr::Number;

macro_rules! make_error {
    ($fmt:literal) => { Gc::new(Value::String($fmt.into())) };
    ($fmt:literal, $($args:expr),*) => { Gc::new(Value::String(format!($fmt, $($args),*).into())) }
}

/// Operations produce either a success or an error value.
type OpResult = Result<Gc<Value>, Gc<Value>>;

pub enum Params {
    Any(Box<str>),
    Exact(Vec<Box<str>>),
    AtLeast(Vec<Box<str>>, Box<str>),
}

impl Params {
    pub fn new(v: &lexpr::Value) -> Result<Self, SyntaxError> {
        use lexpr::Value::*;
        match v {
            Cons(cell) => match cell.to_ref_vec() {
                (params, Null) => Ok(Params::Exact(param_list(&params)?)),
                (params, rest) => Ok(Params::AtLeast(param_list(&params)?, param_rest(rest)?)),
            },
            _ => Ok(Params::Any(param_rest(v)?)),
        }
    }
    pub fn bind(
        &self,
        args: &[Gc<Value>],
        env: Gc<GcCell<Env>>,
    ) -> Result<Gc<GcCell<Env>>, Gc<Value>> {
        match self {
            Params::Any(rest_name) => {
                let mut env = Env::new(env);
                env.bind(rest_name, Value::list(args.into_iter().cloned()));
                Ok(Gc::new(GcCell::new(env)))
            }
            Params::Exact(names) => {
                if names.len() != args.len() {
                    Err(make_error!(
                        "parameter length mismatch; got ({}), expected ({})",
                        ShowSlice(args),
                        ShowSlice(names)
                    ))
                } else {
                    let mut env = Env::new(env);
                    for (name, value) in names.iter().zip(args) {
                        env.bind(name, value.clone());
                    }
                    Ok(Gc::new(GcCell::new(env)))
                }
            }
            Params::AtLeast(names, rest_name) => {
                if names.len() > args.len() {
                    Err(make_error!(
                        "too few parameters; got ({}), expected ({})",
                        ShowSlice(args),
                        ShowSlice(names)
                    ))
                } else {
                    let mut env = Env::new(env);
                    env.bind(rest_name, Value::list(args.into_iter().cloned()));
                    for (name, value) in names.iter().zip(args) {
                        env.bind(name, value.clone());
                    }
                    Ok(Gc::new(GcCell::new(env)))
                }
            }
        }
    }
}

fn param_list(params: &[&lexpr::Value]) -> Result<Vec<Box<str>>, SyntaxError> {
    params
        .into_iter()
        .map(|p| {
            p.as_symbol()
                .ok_or(SyntaxError::ExpectedSymbol)
                .map(Into::into)
        })
        .collect()
}

fn param_rest(rest: &lexpr::Value) -> Result<Box<str>, SyntaxError> {
    rest.as_symbol()
        .ok_or(SyntaxError::ExpectedSymbol)
        .map(Into::into)
}

pub enum Value {
    Number(Number),
    String(Box<str>),
    Null,
    Cons(Box<[Gc<Value>; 2]>),
    Symbol(Box<str>), // TODO: interning
    PrimOp(Box<Fn(&[Gc<Value>]) -> OpResult>),
    Closure {
        params: Params,
        body: Vec<lexpr::Value>,
        env: Gc<GcCell<Env>>,
    },
}

impl Value {
    fn prim_op<F>(f: F) -> Self
    where
        F: Fn(&[Gc<Value>]) -> OpResult + 'static,
    {
        Value::PrimOp(Box::new(f))
    }

    fn list<I>(elts: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Gc<Value>>,
    {
        unimplemented!()
    }

    fn number<T>(n: T) -> Self
    where
        T: Into<Number>,
    {
        Value::Number(n.into())
    }

    fn as_number(&self) -> Option<&lexpr::Number> {
        match self {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }
}

impl From<&lexpr::Value> for Value {
    fn from(v: &lexpr::Value) -> Self {
        use lexpr::Value::*;
        match v {
            Number(n) => Value::Number(n.clone()),
            String(s) => Value::String(s.clone()),
            Symbol(s) => Value::Symbol(s.clone()),
            Cons(cell) => {
                let (car, cdr) = cell.as_pair();
                Value::Cons(Box::new([Gc::new(car.into()), Gc::new(cdr.into())]))
            }
            Null => Value::Null,
            _ => unimplemented!(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Should probably use a more "Rusty" representation
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::PrimOp(_) => write!(f, "#<prim-op>"),
            Value::Closure { .. } => write!(f, "#<closure>"),
            Value::Null => write!(f, "()"),
            Value::Cons(cell) => write_cons(f, cell),
            Value::String(s) => lexpr::Value::string(s.as_ref()).fmt(f),
        }
    }
}

fn write_cons(f: &mut fmt::Formatter, cell: &[Gc<Value>; 2]) -> fmt::Result {
    use std::ops::Deref;

    f.write_char('(')?;
    cell[0].fmt(f)?;
    let mut next = &cell[1];
    loop {
        match next.deref() {
            Value::Null => break,
            Value::Cons(cell) => {
                f.write_char(' ')?;
                cell[0].fmt(f)?;
                next = &cell[1];
            }
            value => {
                f.write_str(" . ")?;
                value.fmt(f)?;
                break;
            }
        }
    }
    f.write_char(')')?;
    Ok(())
}

impl gc::Finalize for Value {
    fn finalize(&self) {}
}

unsafe impl gc::Trace for Value {
    unsafe fn trace(&self) {
        match self {
            Value::Cons(cell) => {
                cell[0].trace();
                cell[1].trace();
            }
            Value::Closure { env, .. } => {
                env.trace();
            }
            _ => {}
        }
    }
    unsafe fn root(&self) {
        match self {
            Value::Cons(cell) => {
                cell[0].root();
                cell[1].root();
            }
            Value::Closure { env, .. } => {
                env.root();
            }
            _ => {}
        }
    }
    unsafe fn unroot(&self) {
        match self {
            Value::Cons(cell) => {
                cell[0].unroot();
                cell[1].unroot();
            }
            Value::Closure { env, .. } => {
                env.unroot();
            }
            _ => {}
        }
    }
    fn finalize_glue(&self) {
        self.finalize();
        match self {
            Value::Cons(cell) => {
                cell[0].finalize();
                cell[1].finalize();
            }
            Value::Closure { env, .. } => {
                env.finalize();
            }
            _ => {}
        }
    }
}

#[derive(Default, Clone)]
pub struct Env {
    parent: Option<Gc<GcCell<Env>>>,
    bindings: HashMap<Box<str>, Gc<Value>>,
}

impl Env {
    pub fn new(parent: Gc<GcCell<Env>>) -> Self {
        Env {
            parent: Some(parent),
            bindings: Default::default(),
        }
    }
    pub fn bind(&mut self, name: &str, value: impl Into<Gc<Value>>) {
        self.bindings.insert(name.into(), value.into());
    }
    pub fn lookup(&self, name: &str) -> Option<Gc<Value>> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().lookup(name)))
    }
}

impl gc::Finalize for Env {
    fn finalize(&self) {}
}

unsafe impl gc::Trace for Env {
    unsafe fn trace(&self) {
        if let Some(parent) = &self.parent {
            parent.trace();
        }
        for value in self.bindings.values() {
            value.trace()
        }
    }
    unsafe fn root(&self) {
        if let Some(parent) = &self.parent {
            parent.root();
        }
        for value in self.bindings.values() {
            value.root()
        }
    }
    unsafe fn unroot(&self) {
        if let Some(parent) = &self.parent {
            parent.unroot();
        }
        for value in self.bindings.values() {
            value.unroot()
        }
    }
    fn finalize_glue(&self) {
        self.finalize();
        if let Some(parent) = &self.parent {
            parent.finalize();
        }
        for value in self.bindings.values() {
            value.finalize()
        }
    }
}

/// Primitives.
mod prim {
    use gc::Gc;
    use num_traits::{CheckedAdd, CheckedMul, CheckedSub};

    use super::{Number, OpResult, Value};

    fn invalid_argument(arg: &Value, expected: &str) -> Gc<Value> {
        make_error!("invalid argument: {}, expected {}", arg, expected)
    }

    fn too_few_arguments(procedure: &str) -> Gc<Value> {
        make_error!("too few arguments to `{}'", procedure)
    }

    fn arithmetic_overflow(operation: &str, arg1: &Number, arg2: &Number) -> Gc<Value> {
        make_error!(
            "arithmetic overflow in {} of {} and {}",
            operation,
            arg1,
            arg2
        )
    }

    pub fn plus(args: &[Gc<Value>]) -> OpResult {
        if let Some((first, rest)) = args.split_first() {
            let mut sum = first
                .as_number()
                .ok_or_else(|| invalid_argument(first, "number"))?
                .clone();
            for elt in rest {
                let n = elt
                    .as_number()
                    .ok_or_else(|| invalid_argument(elt, "number"))?;
                sum = sum
                    .checked_add(n)
                    .ok_or_else(|| arithmetic_overflow("addition", &sum, n))?;
            }
            Ok(Value::Number(sum).into())
        } else {
            Ok(Value::number(0).into())
        }
    }

    pub fn minus(args: &[Gc<Value>]) -> OpResult {
        if let Some((first, rest)) = args.split_first() {
            let mut sum = first
                .as_number()
                .ok_or_else(|| invalid_argument(first, "number"))?
                .clone();
            for elt in rest {
                let n = elt
                    .as_number()
                    .ok_or_else(|| invalid_argument(elt, "number"))?;
                sum = sum
                    .checked_sub(n)
                    .ok_or_else(|| arithmetic_overflow("addition", &sum, n))?;
            }
            Ok(Value::Number(sum).into())
        } else {
            Err(too_few_arguments("-"))
        }
    }

    pub fn times(args: &[Gc<Value>]) -> OpResult {
        if let Some((first, rest)) = args.split_first() {
            let mut sum = first
                .as_number()
                .ok_or_else(|| invalid_argument(first, "number"))?
                .clone();
            for elt in rest {
                let n = elt
                    .as_number()
                    .ok_or_else(|| invalid_argument(elt, "number"))?;
                sum = sum
                    .checked_mul(n)
                    .ok_or_else(|| arithmetic_overflow("multiplication", &sum, n))?;
            }
            Ok(Value::Number(sum).into())
        } else {
            Ok(Value::number(1).into())
        }
    }
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedSymbol,
    ImproperList(Vec<lexpr::Value>, lexpr::Value),
    NonList(lexpr::Value),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::ExpectedSymbol => write!(f, "expected symbol"),
            SyntaxError::ImproperList(elts, tail) => {
                write!(f, "improper list `({} . {})'", ShowSlice(&elts), tail)
            }
            SyntaxError::NonList(value) => write!(f, "non-list `{}'", value),
        }
    }
}

struct ShowSlice<'a, T>(&'a [T]);

impl<'a, T> fmt::Display for ShowSlice<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, value) in self.0.iter().enumerate() {
            value.fmt(f)?;
            if i + 1 < self.0.len() {
                f.write_char(' ')?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for SyntaxError {}

fn proper_list(expr: &lexpr::Value) -> Result<Vec<&lexpr::Value>, SyntaxError> {
    match expr {
        lexpr::Value::Cons(cell) => match cell.to_ref_vec() {
            (args, tail) => {
                if tail != &lexpr::Value::Null {
                    Err(SyntaxError::ImproperList(
                        args.into_iter().cloned().collect(),
                        tail.clone(),
                    ))
                } else {
                    Ok(args)
                }
            }
        },
        lexpr::Value::Null => Ok(Vec::new()),
        value => Err(SyntaxError::NonList(value.clone())),
    }
}

fn syntax_error(e: SyntaxError) -> Gc<Value> {
    make_error!("syntax error: {}", e)
}

pub fn eval(expr: &lexpr::Value, env: Gc<GcCell<Env>>) -> OpResult {
    match expr {
        lexpr::Value::Symbol(sym) => env
            .borrow_mut()
            .lookup(&sym)
            .ok_or_else(|| make_error!("unbound identifier `{}'", sym)),
        lexpr::Value::Cons(cell) => {
            let (first, rest) = cell.as_pair();
            match first.as_symbol() {
                Some("quote") => {
                    let args = proper_list(rest).map_err(syntax_error)?;
                    if args.len() != 1 {
                        return Err(make_error!("`quote' expects a single form"));
                    }
                    Ok(Gc::new(args[0].into()))
                }
                Some("lambda") => {
                    let args = proper_list(rest).map_err(syntax_error)?;
                    if args.len() < 2 {
                        return Err(make_error!("`lambda` expects at least two forms"));
                    }
                    let params = Params::new(args[0]).map_err(syntax_error)?;
                    let body = args[1..].into_iter().map(|e| (*e).clone()).collect();
                    let closure = Value::Closure {
                        params,
                        body,
                        env: env.clone(),
                    };
                    Ok(Gc::new(closure))
                }
                Some("define") => {
                    let args = proper_list(rest).map_err(syntax_error)?;
                    if args.len() < 2 {
                        return Err(make_error!("`define` expects at least two forms"));
                    }
                    match args[0] {
                        lexpr::Value::Symbol(sym) => {
                            if args.len() != 2 {
                                return Err(make_error!(
                                    "`define` for variable expects one value form"
                                ));
                            }
                            let value = eval(&args[1], env.clone())?;
                            env.borrow_mut().bind(&sym, value);
                            Ok(Gc::new(Value::Null))
                        }
                        lexpr::Value::Cons(cell) => {
                            let ident = cell.car().as_symbol().ok_or_else(|| {
                                make_error!("invalid function `define': non-identifier")
                            })?;
                            let params = Params::new(cell.cdr()).map_err(syntax_error)?;
                            let body = args[1..].into_iter().map(|e| (*e).clone()).collect();
                            env.borrow_mut().bind(
                                &ident,
                                Value::Closure {
                                    params,
                                    body,
                                    env: env.clone(),
                                },
                            );
                            Ok(Gc::new(Value::Null))
                        }
                        _ => return Err(make_error!("invalid `define' form")),
                    }
                }
                _ => {
                    let op = eval(first, env.clone())?;
                    let arg_exprs = proper_list(rest).map_err(syntax_error)?;
                    let args = arg_exprs
                        .into_iter()
                        .map(|arg| eval(arg, env.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    apply(op, &args)
                }
            }
        }
        lexpr::Value::Number(n) => Ok(Value::Number(n.clone()).into()),
        lexpr::Value::String(s) => Ok(Value::String(s.clone()).into()),
        _ => unimplemented!(),
    }
}

pub fn apply(op: Gc<Value>, args: &[Gc<Value>]) -> OpResult {
    match &*op {
        Value::PrimOp(ref op) => op(args),
        Value::Closure { params, body, env } => {
            let env = params.bind(args, env.clone())?;
            for (i, expr) in body.into_iter().enumerate() {
                if i + 1 == body.len() {
                    return eval(expr, env.clone());
                }
                eval(expr, env.clone())?;
            }
            unreachable!()
        }
        _ => Err(make_error!(
            "non-applicable object in operator position: {}",
            op
        )),
    }
}

fn main() -> io::Result<()> {
    let mut env = Env::default();
    env.bind("+", Value::prim_op(prim::plus));
    env.bind("-", Value::prim_op(prim::minus));
    env.bind("*", Value::prim_op(prim::times));
    let env = Gc::new(GcCell::new(env));
    let input = io::BufReader::new(io::stdin());
    for line in input.lines() {
        let line = line?;
        match eval(&line.parse().unwrap(), env.clone()) {
            Ok(value) => println!("{}", value),
            Err(e) => println!("; error: {}", e),
        }
    }
    Ok(())
}
