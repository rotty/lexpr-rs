use quickcheck::{Arbitrary, Gen, QuickCheck, StdGen};
use quickcheck_macros::quickcheck;
use rand::{seq::SliceRandom, Rng};

use std::str;

use crate as lexpr;

use lexpr::{Atom, Number, Value};

enum ValueKind {
    Atom,
    List,
    ImproperList,
}

fn gen_value<G: Gen>(g: &mut G, depth: usize) -> Value {
    use ValueKind::*;
    let choices = if depth >= g.size() {
        &[Atom] as &[ValueKind]
    } else {
        &[Atom, List, ImproperList]
    };
    match choices.choose(g).unwrap() {
        Atom => Value::Atom(Arbitrary::arbitrary(g)),
        List => Value::List((0..g.size()).map(|_| gen_value(g, depth + 1)).collect()),
        ImproperList => {
            let size = {
                let s = g.size();
                g.gen_range(1, s)
            };
            Value::ImproperList(
                (0..size).map(|_| gen_value(g, depth + 1)).collect(),
                Arbitrary::arbitrary(g),
            )
        }
    }
}

impl Arbitrary for Value {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        gen_value(g, 0)
    }
    fn shrink(&self) -> Box<Iterator<Item = Value>> {
        use Value::*;
        let nothing = || Box::new(None.into_iter());
        match self {
            List(elements) => Box::new(elements.clone().shrink().map(List)),
            ImproperList(elements, tail) => {
                let tail = tail.clone();
                Box::new(
                    elements
                        .clone()
                        .shrink()
                        .filter(|elts| elts.len() > 0)
                        .map(move |elts| ImproperList(elts, tail.clone())),
                )
            }
            _ => nothing(),
        }
    }
}

enum AtomKind {
    Nil,
    Bool,
    Number,
    String,
    Symbol,
    Keyword,
}

impl Arbitrary for Atom {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        use AtomKind::*;
        let choices = [Nil, Bool, Number, String, Symbol, Keyword];
        match choices.choose(g).unwrap() {
            Nil => Atom::Nil,
            Bool => Atom::Bool(g.gen()),
            Number => Atom::Number(Arbitrary::arbitrary(g)),
            String => {
                let choices = ["", "foo", "\"", "\t"];
                Atom::String(choices.choose(g).unwrap().to_string())
            }
            Symbol => {
                let choices = ["foo", "a-symbol", "$?:!"];
                Atom::Symbol(choices.choose(g).unwrap().to_string())
            }
            Keyword => {
                let choices = ["foo", "a-keyword", "$?:!"];
                Atom::Keyword(choices.choose(g).unwrap().to_string())
            }
        }
    }
}

enum NumberKind {
    I64,
    U64,
    F64,
}

impl Arbitrary for Number {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        use NumberKind::*;
        let choices = [I64, U64, F64];
        match choices.choose(g).unwrap() {
            I64 => Number::from(i64::arbitrary(g)),
            U64 => Number::from(u64::arbitrary(g)),
            F64 => Number::from(f64::arbitrary(g)),
        }
    }
}

#[quickcheck]
fn write_parse_number(input: Number) -> bool {
    let mut bytes = Vec::new();
    input.write(&mut bytes).expect("conversion to bytes failed");
    let parsed = lexpr::from_slice(&bytes).expect("parsing failed");
    input == *parsed.as_number().expect("parsed as a non-number")
}

#[test]
fn write_number() {
    let mut bytes = Vec::new();
    Number::from(-11.287888289184039).write(&mut bytes).expect("conversion to bytes failed");
    assert_eq!(str::from_utf8(&bytes).unwrap(), "-11.287888289184039");
}

#[test]
fn print_parse_roundtrip_default() {
    fn prop(input: Value) -> bool {
        let string = lexpr::to_string(&input).expect("conversion to string failed");
        let output = lexpr::from_str(&string).expect("parsing failed");
        input == output
    }
    QuickCheck::new()
        .tests(100)
        .max_tests(200)
        .gen(StdGen::new(::rand::thread_rng(), 3))
        .quickcheck(prop as fn(Value) -> bool);
}
