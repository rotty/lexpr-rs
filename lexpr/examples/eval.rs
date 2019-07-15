/// A simple S-expression evaluator.
use std::{
    collections::HashMap,
    env,
    fmt::{self, Display, Write},
    fs,
    io::{self, BufRead},
    path::Path,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell};
use lexpr::Number;

macro_rules! make_error {
    ($fmt:literal) => { Gc::new(Value::String($fmt.into())) };
    ($fmt:literal, $($args:expr),*) => { Gc::new(Value::String(format!($fmt, $($args),*).into())) }
}

/// Operations produce either a success or an error value.
type OpResult = Result<Gc<Value>, Gc<Value>>;

#[derive(Debug)]
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
    Bool(bool),
    Null,
    Cons(Box<[Gc<Value>; 2]>),
    Symbol(Box<str>), // TODO: interning
    PrimOp(Box<Fn(&[Gc<Value>]) -> OpResult>),
    Closure {
        params: Rc<Params>,
        body: Gc<Ast>,
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

    fn is_true(&self) -> bool {
        if let Value::Bool(v) = self {
            *v
        } else {
            true
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
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
            Value::Bool(b) => f.write_str(if *b { "#t" } else { "#f" }),
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

macro_rules! impl_value_trace_body {
    ($this:ident, $method:ident) => {
        match $this {
            Value::Cons(cell) => {
                cell[0].$method();
                cell[1].$method();
            }
            Value::Closure { env, body, .. } => {
                body.$method();
                env.$method();
            }
            _ => {}
        }
    }
}

unsafe impl gc::Trace for Value {
    unsafe fn trace(&self) {
        impl_value_trace_body!(self, trace);
    }
    unsafe fn root(&self) {
        impl_value_trace_body!(self, root);
    }
    unsafe fn unroot(&self) {
        impl_value_trace_body!(self, unroot);
    }
    fn finalize_glue(&self) {
        self.finalize();
        impl_value_trace_body!(self, finalize_glue);
    }
}

// TODO: This should not need to contain `Gc` values; we could turn these into
// `Rc`, if we find some solution for the `Datum`, which currently contains a
// `Gc`, requiring all other `Ast` smart pointers into being `Gc` as well.
#[derive(Debug)]
pub enum Ast {
    Datum(Gc<Value>),
    Lambda {
        params: Rc<Params>,
        body: Gc<Ast>,
    },
    Define {
        ident: Box<str>,
        expr: Gc<Ast>,
    },
    If {
        cond: Gc<Ast>,
        consequent: Gc<Ast>,
        alternative: Gc<Ast>,
    },
    Begin(Vec<Gc<Ast>>),
    Apply {
        op: Gc<Ast>,
        operands: Vec<Gc<Ast>>,
    },
    EnvRef(Box<str>),
}

impl Ast {
    pub fn new(expr: &lexpr::Value) -> Result<Ast, Gc<Value>> {
        match expr {
            lexpr::Value::Null => Err(make_error!("empty application")),
            lexpr::Value::Nil => Err(make_error!("#nil unsupported")),
            lexpr::Value::Char(_) => Err(make_error!("characters are currently unsupported")),
            lexpr::Value::Keyword(_) => Err(make_error!("keywords are currently unsupported")),
            lexpr::Value::Bytes(_) => Err(make_error!("byte vectors are currently unsupported")),
            lexpr::Value::Vector(_) => Err(make_error!("vectors are currently unsupported")),
            lexpr::Value::Bool(b) => Ok(Ast::Datum(Value::Bool(*b).into())),
            lexpr::Value::Number(n) => Ok(Ast::Datum(Value::Number(n.clone()).into())),
            lexpr::Value::String(s) => Ok(Ast::Datum(Value::String(s.clone()).into())),
            lexpr::Value::Symbol(sym) => Ok(Ast::env_ref(sym.clone())),
            lexpr::Value::Cons(cell) => {
                let (first, rest) = cell.as_pair();
                match first.as_symbol() {
                    Some("quote") => {
                        let args = proper_list(rest).map_err(syntax_error)?;
                        if args.len() != 1 {
                            return Err(make_error!("`quote' expects a single form"));
                        }
                        Ok(Ast::Datum(Gc::new(args[0].into())))
                    }
                    Some("lambda") => {
                        let args = proper_list(rest).map_err(syntax_error)?;
                        if args.len() < 2 {
                            return Err(make_error!("`lambda` expects at least two forms"));
                        }
                        Ok(Ast::Lambda {
                            params: Rc::new(Params::new(args[0]).map_err(syntax_error)?),
                            body: Gc::new(Ast::begin(&args[1..])?),
                        })
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
                                Ok(Ast::Define {
                                    ident: sym.clone(),
                                    expr: Gc::new(Ast::new(&args[1])?),
                                })
                            }
                            lexpr::Value::Cons(cell) => {
                                let ident = cell.car().as_symbol().ok_or_else(|| {
                                    make_error!("invalid function `define': non-identifier")
                                })?;
                                Ok(Ast::Define {
                                    ident: ident.into(),
                                    expr: Ast::lambda(cell.cdr(), &args[1..])?.into(),
                                })
                            }
                            _ => Err(make_error!("invalid `define' form")),
                        }
                    }
                    Some("if") => {
                        let args = proper_list(rest).map_err(syntax_error)?;
                        if args.len() != 3 {
                            return Err(make_error!("`if` expects at exactly three forms"));
                        }
                        Ok(Ast::If {
                            cond: Ast::new(&args[0])?.into(),
                            consequent: Ast::new(&args[1])?.into(),
                            alternative: Ast::new(&args[2])?.into(),
                        })
                    }
                    _ => {
                        let arg_exprs = proper_list(rest).map_err(syntax_error)?;
                        Ok(Ast::Apply {
                            op: Ast::new(first)?.into(),
                            operands: arg_exprs
                                .into_iter()
                                .map(|arg| Ok(Ast::new(arg)?.into()))
                                .collect::<Result<Vec<Gc<Ast>>, Gc<Value>>>()?,
                        })
                    }
                }
            }
        }
    }

    fn begin(exprs: &[&lexpr::Value]) -> Result<Self, Gc<Value>> {
        let ast_exprs = exprs
            .into_iter()
            .map(|e| Ok(Gc::new(Ast::new(*e)?)))
            .collect::<Result<_, Gc<Value>>>()?;
        Ok(Ast::Begin(ast_exprs))
    }

    fn env_ref(sym: impl Into<Box<str>>) -> Self {
        Ast::EnvRef(sym.into())
    }

    fn lambda(params: &lexpr::Value, body: &[&lexpr::Value]) -> Result<Self, Gc<Value>> {
        Ok(Ast::Lambda {
            params: Params::new(params).map_err(syntax_error)?.into(),
            body: Ast::begin(body)?.into(),
        })
    }
}

impl gc::Finalize for Ast {
    fn finalize(&self) {}
}

macro_rules! impl_ast_trace_body {
    ($this:ident, $method:ident) => {
        use Ast::*;
        match $this {
            Datum(value) => value.$method(),
            Lambda { body, .. } => body.$method(),
            Define { expr, .. } => expr.$method(),
            If { cond, consequent, alternative } => {
                cond.$method();
                consequent.$method();
                alternative.$method();
            }
            Begin(body) => {
                for expr in body {
                    expr.$method();
                }
            }
            Apply { op, operands } => {
                op.$method();
                for operand in operands {
                    operand.$method();
                }
            }
            EnvRef(_) => {},

        }
    }
}

unsafe impl gc::Trace for Ast {
    unsafe fn trace(&self) {
        impl_ast_trace_body!(self, trace);
    }
    unsafe fn root(&self) {
        impl_ast_trace_body!(self, root);
    }
    unsafe fn unroot(&self) {
        impl_ast_trace_body!(self, unroot);
    }
    fn finalize_glue(&self) {
        self.finalize();
        impl_ast_trace_body!(self, finalize_glue);
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
    use std::io::{self, Write};

    use gc::Gc;
    use num_traits::{CheckedAdd, CheckedMul, CheckedSub};

    use super::{Number, OpResult, Value};

    fn invalid_argument(arg: &Value, expected: &str) -> Gc<Value> {
        make_error!("invalid argument: {}, expected {}", arg, expected)
    }

    fn too_few_arguments(procedure: &str) -> Gc<Value> {
        make_error!("too few arguments to `{}'", procedure)
    }

    fn wrong_number_of_arguments(
        procedure: &str,
        expected: usize,
        args: &[Gc<Value>],
    ) -> Gc<Value> {
        make_error!(
            "wrong number of arguments to `{}': expected {}, got {}",
            procedure,
            expected,
            args.len()
        )
    }

    fn io_error(e: io::Error) -> Gc<Value> {
        make_error!("I/O error: {}", e)
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

    fn num_cmp<F>(args: &[Gc<Value>], cmp: F) -> OpResult
    where
        F: Fn(&Number, &Number) -> bool,
    {
        for w in args.windows(2) {
            let n1 = w[0]
                .as_number()
                .ok_or_else(|| invalid_argument(&w[0], "number"))?;
            let n2 = w[1]
                .as_number()
                .ok_or_else(|| invalid_argument(&w[1], "number"))?;
            if !cmp(n1, n2) {
                return Ok(Gc::new(Value::from(false)));
            }
        }
        Ok(Gc::new(Value::from(true)))
    }

    pub fn eq(args: &[Gc<Value>]) -> OpResult {
        num_cmp(args, Number::ge)
    }

    pub fn lt(args: &[Gc<Value>]) -> OpResult {
        num_cmp(args, Number::lt)
    }

    pub fn le(args: &[Gc<Value>]) -> OpResult {
        num_cmp(args, Number::le)
    }

    pub fn gt(args: &[Gc<Value>]) -> OpResult {
        num_cmp(args, Number::gt)
    }

    pub fn ge(args: &[Gc<Value>]) -> OpResult {
        num_cmp(args, Number::ge)
    }

    pub fn display(args: &[Gc<Value>]) -> OpResult {
        if args.len() != 1 {
            // TODO: support ports
            return Err(wrong_number_of_arguments("display", 1, args));
        }
        // TODO: we use the `Display` trait of `Value` here, which currently
        // uses `write` notation, not `display` notation.
        write!(io::stdout(), "{}", args[0]).map_err(io_error)?;
        Ok(Gc::new(Value::Null))
    }

    pub fn newline(args: &[Gc<Value>]) -> OpResult {
        if args.len() != 0 {
            // TODO: support ports
            return Err(wrong_number_of_arguments("newline", 0, args));
        }
        write!(io::stdout(), "\n").map_err(io_error)?;
        Ok(Gc::new(Value::Null))
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

fn eval_ast(ast: Gc<Ast>, env: Gc<GcCell<Env>>) -> OpResult {
    let mut env = env;
    let mut ast = ast;
    loop {
        match eval_step(ast, env)? {
            Thunk::Resolved(v) => break Ok(v),
            Thunk::Eval(thunk_ast, thunk_env) => {
                ast = thunk_ast;
                env = thunk_env;
            }
        }
    }
}

pub fn eval(expr: &lexpr::Value, env: Gc<GcCell<Env>>) -> OpResult {
    let ast = Ast::new(expr)?;
    eval_ast(Gc::new(ast), env)
}

fn eval_step(ast: Gc<Ast>, env: Gc<GcCell<Env>>) -> Result<Thunk, Gc<Value>> {
    match &*ast {
        Ast::EnvRef(ident) => env
            .borrow_mut()
            .lookup(ident)
            .map(Thunk::Resolved)
            .ok_or_else(|| make_error!("unbound identifier `{}'", ident)),
        Ast::Datum(value) => Ok(Thunk::Resolved(value.clone())),
        Ast::Lambda { params, body } => {
            let closure = Value::Closure {
                params: Rc::clone(params),
                body: Gc::clone(body),
                env: env.clone(),
            };
            Ok(Thunk::Resolved(Gc::new(closure)))
        }
        Ast::Define { ident, expr } => {
            let value = eval_ast(Gc::clone(expr), env.clone())?;
            env.borrow_mut().bind(ident, value);
            Ok(Thunk::Resolved(Gc::new(Value::Null)))
        }
        Ast::If {
            cond,
            consequent,
            alternative,
        } => {
            let cond = eval_ast(Gc::clone(cond), env.clone())?;
            if cond.is_true() {
                Ok(Thunk::Eval(Gc::clone(consequent), env))
            } else {
                Ok(Thunk::Eval(Gc::clone(alternative), env))
            }
        }
        Ast::Apply { op, operands } => {
            let op = eval_ast(Gc::clone(op), env.clone())?;
            let operands = operands
                .into_iter()
                .map(|operand| eval_ast(Gc::clone(operand), env.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            apply(&op, &operands)
        }
        Ast::Begin(body) => {
            for (i, expr) in body.into_iter().enumerate() {
                if i + 1 == body.len() {
                    // TODO: unfortunate clone of `expr` here
                    return Ok(Thunk::Eval(expr.clone(), env.clone()));
                }
                eval_ast(Gc::clone(expr), env.clone())?;
            }
            unreachable!()
        }
    }
}

pub enum Thunk {
    Resolved(Gc<Value>),
    Eval(Gc<Ast>, Gc<GcCell<Env>>),
}

pub fn apply(op: &Value, args: &[Gc<Value>]) -> Result<Thunk, Gc<Value>> {
    match op {
        Value::PrimOp(ref op) => Ok(Thunk::Resolved(op(args)?)),
        Value::Closure { params, body, env } => {
            let env = params.bind(args, env.clone())?;
            eval_step(Gc::clone(body), env)
        }
        _ => Err(make_error!(
            "non-applicable object in operator position: {}",
            op
        )),
    }
}

#[derive(Debug)]
pub enum EvalError {
    Io(io::Error),
    Parse(lexpr::parse::Error),
    Runtime(Gc<Value>),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use EvalError::*;
        match self {
            Io(e) => write!(f, "I/O error: {}", e),
            Parse(e) => write!(f, "parse error: {}", e),
            Runtime(e) => write!(f, "runtime error: {}", e),
        }
    }
}

impl std::error::Error for EvalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use EvalError::*;
        match self {
            Io(e) => Some(e),
            Parse(e) => Some(e),
            Runtime(_) => None,
        }
    }
}

impl From<lexpr::parse::Error> for EvalError {
    fn from(e: lexpr::parse::Error) -> Self {
        EvalError::Parse(e)
    }
}

impl From<io::Error> for EvalError {
    fn from(e: io::Error) -> Self {
        EvalError::Io(e)
    }
}

impl From<Gc<Value>> for EvalError {
    fn from(e: Gc<Value>) -> Self {
        EvalError::Runtime(e)
    }
}

pub fn load(path: impl AsRef<Path>, env: Gc<GcCell<Env>>) -> Result<(), EvalError> {
    let file = fs::File::open(path)?;
    let mut parser = lexpr::Parser::from_reader(file);
    while let Some(expr) = parser.parse()? {
        eval(&expr, env.clone())?;
    }
    Ok(())
}

fn main() -> Result<(), EvalError> {
    let mut env = Env::default();

    env.bind("+", Value::prim_op(prim::plus));
    env.bind("-", Value::prim_op(prim::minus));
    env.bind("*", Value::prim_op(prim::times));
    env.bind("<", Value::prim_op(prim::lt));
    env.bind("<=", Value::prim_op(prim::le));
    env.bind(">", Value::prim_op(prim::gt));
    env.bind(">=", Value::prim_op(prim::ge));
    env.bind("==", Value::prim_op(prim::eq));
    env.bind("display", Value::prim_op(prim::display));
    env.bind("newline", Value::prim_op(prim::newline));

    let env = Gc::new(GcCell::new(env));
    let args: Vec<_> = env::args_os().skip(1).collect();
    if args.len() == 0 {
        let input = io::BufReader::new(io::stdin());
        for line in input.lines() {
            let line = line?;
            match eval(&line.parse().unwrap(), env.clone()) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("; error: {}", e),
            }
        }
    } else {
        for filename in &args {
            load(filename, env.clone())?;
        }
    }
    Ok(())
}
