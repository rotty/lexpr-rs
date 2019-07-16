use std::{collections::HashMap, fmt, io, rc::Rc};

use gc::{Gc, GcCell, Finalize};

use crate::{ast::Ast, value::Value, OpResult};

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

impl fmt::Display for EvalError {
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
