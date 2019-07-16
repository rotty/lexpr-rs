use std::{
    fmt::{self, Write, Display},
    rc::Rc,
};

use gc::{Gc, GcCell, Finalize};
use lexpr::Number;

use crate::{
    ast::{Ast, Params},
    eval::Env,
    OpResult,
};

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
    pub fn prim_op<F>(f: F) -> Self
    where
        F: Fn(&[Gc<Value>]) -> OpResult + 'static,
    {
        Value::PrimOp(Box::new(f))
    }

    pub fn list<I>(elts: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Gc<Value>>,
    {
        unimplemented!()
    }

    pub fn number<T>(n: T) -> Self
    where
        T: Into<Number>,
    {
        Value::Number(n.into())
    }

    pub fn as_number(&self) -> Option<&lexpr::Number> {
        match self {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }

    pub fn is_true(&self) -> bool {
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
    };
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
