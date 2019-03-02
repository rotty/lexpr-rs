//! Test serialization to the `lexpr::Value` type

use std::fmt::Debug;

use serde_derive::{Deserialize, Serialize};

use lexpr::{sexp, Value};
use serde_lexpr::{from_value, to_value};

fn test_serde<T>(thing: &T, expected: &Value)
where
    T: serde::Serialize + serde::de::DeserializeOwned + PartialEq + Debug,
{
    let value = to_value(thing).unwrap();
    assert_eq!(&value, expected);
    let deserialized: T = from_value(&value).unwrap();
    assert_eq!(&deserialized, thing);
}

#[test]
fn test_int() {
    let n = 4223;
    test_serde(&n, &sexp!(4223));
}

#[test]
fn test_vec() {
    let empty: Vec<u32> = vec![];
    test_serde(&empty, &sexp!(()));
    test_serde(&vec![1, 2, 3, 4], &sexp!((1 2 3 4)));
}

#[test]
fn test_unit() {
    test_serde(&(), &sexp!(#nil));
}

#[test]
fn test_tuples() {
    // TODO: This should use vectors, once supported
    let tuple = (1, "Hello".to_string(), true);
    test_serde(&tuple, &sexp!((1 "Hello" #t)));
}

#[test]
fn test_option() {
    let none: Option<String> = None;
    test_serde(&none, &sexp!(()));
    test_serde(&Some("Hello".to_string()), &sexp!(("Hello")));
}

#[test]
fn test_basic_enum() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Animal {
        Dog,
        Cat,
        Kangaroo,
    }
    test_serde(&Animal::Dog, &sexp!(Dog));
    test_serde(&Animal::Cat, &sexp!(Cat));
    test_serde(&Animal::Kangaroo, &sexp!(Kangaroo));
}

#[test]
fn test_tuple_enum() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Greeting {
        Hello,
        GoodBye,
        Other(String),
        Counted(u32, String),
    }
    test_serde(&Greeting::Hello, &sexp!(Hello));
    test_serde(&Greeting::GoodBye, &sexp!(GoodBye));
    test_serde(
        &Greeting::Other("Farewell".into()),
        &sexp!((Other . "Farewell")),
    );
    test_serde(
        &Greeting::Counted(42, "Have a nice day".into()),
        &sexp!((Counted 42 "Have a nice day")),
    );
}

#[test]
fn test_complex_enum() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Complex {
        Unit,
        OptionSingleton(Option<String>),
        Struct {
            s: Option<String>,
            v: Vec<u32>,
            t: (u32, f64, String),
        },
    }
    test_serde(&Complex::Unit, &sexp!(Unit));
    test_serde(&Complex::OptionSingleton(None), &sexp!((OptionSingleton)));
    test_serde(
        &Complex::OptionSingleton(Some("hello".into())),
        &sexp!((OptionSingleton "hello")),
    );
    let s = Complex::Struct {
        s: Some("hello".into()),
        v: vec![1, 2, 3],
        t: (23, 1.23, "good bye".to_string()),
    };
    test_serde(
        &s,
        &sexp!((Struct (s "hello") (v 1 2 3) (t 23 1.23 "good bye"))),
    );
}

#[test]
fn test_basic_struct() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Basic {
        foo: i64,
        bar: String,
        baz: bool,
    }
    let thing = Basic {
        foo: -324,
        bar: "Hello World".to_owned(),
        baz: true,
    };
    test_serde(
        &thing,
        &sexp!(((foo . -324) (bar . "Hello World") (baz . #t))),
    );
}
