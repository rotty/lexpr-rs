//! Test serialization to the `lexpr::Value` type

use std::fmt::Debug;

use serde_derive::{Deserialize, Serialize};

use lexpr::{sexp, Value};
use serde_lexpr::{error::Category, from_str, from_value, to_value};
use std::collections::HashMap;
use std::collections::HashSet;

fn test_serde<T>(thing: &T, expected: &Value)
where
    T: serde::Serialize + serde::de::DeserializeOwned + PartialEq + Debug,
{
    let value = to_value(thing).unwrap();
    assert_eq!(&value, expected);
    let deserialized: T = from_value(&value).unwrap();
    assert_eq!(&deserialized, thing);
}

fn deser_error<T>(s: &str) -> Option<(Category, Option<(usize, usize)>)>
where
    T: serde::de::DeserializeOwned,
{
    from_str::<T>(s)
        .err()
        .map(|e| (e.classify(), e.location().map(|l| (l.line(), l.column()))))
}

#[test]
fn test_int() {
    let n = 4223;
    test_serde(&n, &sexp!(4223));
}

#[test]
fn test_char() {
    test_serde(&'c', &sexp!('c'));
}

#[test]
fn test_bytes() {
    let bytes = b"abc";
    test_serde(
        &serde_bytes::ByteBuf::from(bytes.as_ref()),
        &Value::from(&bytes[..]),
    );
}

#[test]
fn test_vec() {
    let empty: Vec<u32> = vec![];
    test_serde(&empty, &sexp!(()));
    test_serde(&vec![1, 2, 3, 4], &sexp!((1 2 3 4)));
}

#[test]
fn test_hashmap() {
    let mut hm: HashMap<String, u32> = HashMap::new();
    test_serde(&hm, &sexp!(()));
    hm.insert("one".to_string(), 1);
    test_serde(&hm, &sexp!((("one" . 1))));
}

#[test]
fn test_hashset() {
    let mut hs: HashSet<String> = HashSet::new();
    test_serde(&hs, &sexp!(()));
    hs.insert("one".to_string());
    test_serde(&hs, &sexp!(("one")));
}

#[test]
fn test_unit() {
    test_serde(&(), &sexp!(()));
}

#[test]
fn test_tuples() {
    let tuple = (1, "Hello".to_string(), true);
    test_serde(&tuple, &sexp!(#(1 "Hello" #t)));
}

#[test]
fn test_deser_list_as_tuple() {
    let tuple: (u32, String) = from_value(&sexp!((42 "Answer"))).unwrap();
    assert_eq!(tuple, (42, "Answer".to_string()));
}

#[test]
fn test_deser_vector() {
    let v: Vec<u32> = from_value(&sexp!(#(42 23))).unwrap();
    assert_eq!(v, vec![42, 23]);
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
        &sexp!((Struct (s "hello") (v . (1 2 3)) (t . #(23 1.23 "good bye")))),
    );
}

#[test]
fn test_unit_struct() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Unit;
    test_serde(&Unit, &sexp!(()));
}

#[test]
fn test_empty_tuple_struct() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Unit();
    test_serde(&Unit(), &sexp!(#()));
}

#[test]
fn test_newtype_struct() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Newtype(u32);
    test_serde(&Newtype(42), &sexp!(42));
}

#[test]
fn test_empty_struct() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Unit{};
    test_serde(&Unit{}, &sexp!(()));
}

#[test]
fn test_empty_tuple_variant() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Empty { Tuplish() }
    test_serde(&Empty::Tuplish{}, &sexp!((Tuplish)));
}

#[test]
fn test_empty_struct_variant() {
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Empty { Structish{} }
    test_serde(&Empty::Structish{}, &sexp!((Structish)));
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

#[test]
fn test_parse_error_eof() {
    assert_eq!(
        deser_error::<String>("\""),
        Some((Category::Eof, Some((1, 1))))
    );
    assert_eq!(
        deser_error::<String>("\"\\x43"),
        Some((Category::Eof, Some((1, 5))))
    );
}
