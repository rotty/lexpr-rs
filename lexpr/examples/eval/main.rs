use std::{io::{self, BufRead}, env, fs, path::Path};

use gc::{Gc, GcCell};
use lexpr::Number;

macro_rules! make_error {
    ($fmt:literal) => { Gc::new(Value::String($fmt.into())) };
    ($fmt:literal, $($args:expr),*) => { Gc::new($crate::Value::String(format!($fmt, $($args),*).into())) }
}

mod ast;
mod eval;
mod prim;
mod value;

use eval::{eval, Env, EvalError};
use value::Value;

/// Operations produce either a success or an error value.
type OpResult = Result<Gc<Value>, Gc<Value>>;

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
