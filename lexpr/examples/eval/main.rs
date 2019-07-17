use std::{
    env, fs,
    io::{self, BufRead},
    iter,
    path::Path,
};

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

use ast::Ast;
use eval::{eval, Env, EvalError};
use value::Value;

/// Operations produce either a success or an error value.
type OpResult = Result<Gc<Value>, Gc<Value>>;

macro_rules! prim_op {
    ($name:tt, $func:expr) => {
        ($name, Value::prim_op($name, $func))
    };
}

fn eval_toplevel<I, F>(source: I, mut sink: F) -> Result<(), EvalError>
where
    I: Iterator<Item = Result<lexpr::Value, EvalError>>,
    F: FnMut(Result<Gc<Value>, EvalError>) -> Result<(), EvalError>,
{
    let env = vec![
        prim_op!("+", prim::plus),
        prim_op!("-", prim::minus),
        prim_op!("*", prim::times),
        prim_op!("<", prim::lt),
        prim_op!("<=", prim::le),
        prim_op!(">", prim::gt),
        prim_op!(">=", prim::ge),
        prim_op!("=", prim::eq),
        prim_op!("display", prim::display),
        prim_op!("newline", prim::newline),
    ];
    let (env, mut stack) = Env::new_root(env);
    let env = Gc::new(GcCell::new(env));

    for expr in source {
        let res = expr.and_then(|expr| Ok(Ast::definition(&expr, &mut stack)?));
        if let Some(ast) = res.transpose() {
            let res = stack
                .resolve_rec(env.clone())
                .map_err(Into::into)
                .and_then(|_| ast.and_then(|ast| Ok(eval(Gc::new(ast), env.clone())?)));
            sink(res)?;
        }
    }
    Ok(())
}

fn load(path: impl AsRef<Path>) -> Result<(), EvalError> {
    let file = fs::File::open(path)?;
    let mut parser = lexpr::Parser::from_reader(file);
    eval_toplevel(
        iter::from_fn(move || parser.parse().map_err(Into::into).transpose()),
        |res| {
            let _ = res?;
            Ok(())
        },
    )?;
    Ok(())
}

fn main() -> Result<(), EvalError> {
    let args: Vec<_> = env::args_os().skip(1).collect();
    if args.len() == 0 {
        let input = io::BufReader::new(io::stdin());
        eval_toplevel(
            input.lines().map(|line| {
                let line = line?;
                Ok(line.parse()?)
            }),
            |res| {
                match res {
                    Ok(value) => println!("{}", value),
                    Err(e) => println!("; error: {}", e),
                }
                Ok(())
            },
        )?;
    } else {
        for filename in &args {
            load(filename)?;
        }
    }
    Ok(())
}
