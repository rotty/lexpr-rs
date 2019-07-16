use std::{
    fmt::{self, Write},
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell};

use crate::{eval::Env, value::Value};

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
            If {
                cond,
                consequent,
                alternative,
            } => {
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
            EnvRef(_) => {}
        }
    };
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
