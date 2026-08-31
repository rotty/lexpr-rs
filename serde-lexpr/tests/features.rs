//! Feature tests for serde-lexpr.
//!
//! This module tests all optional features including:
//! - `tuple_as_list`: Serialize tuples as lists instead of vectors
//! - `bool_as_tnil`: Use t/nil for booleans instead of #t/#f
//! - `sym_as_lower`: Case-insensitive symbol handling (lowercase normalization)
//!
//! These features enable compatibility with various Lisp dialects
//! (AutoLISP, Common Lisp, Emacs Lisp, etc.).

use serde_derive::{Deserialize, Serialize};
use serde_lexpr::{from_str, to_string};

// =============================================================================
// tuple_as_list feature tests
// =============================================================================

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
struct Person(String, u8);

#[test]
#[cfg(feature = "tuple_as_list")]
fn tuple_serializes_as_list() {
    let person = Person("John".to_string(), 20);
    let result = to_string(&person).unwrap();
    assert_eq!(result, r#"("John" 20)"#);
}

#[test]
#[cfg(not(feature = "tuple_as_list"))]
fn tuple_serializes_as_vector() {
    let person = Person("John".to_string(), 20);
    let result = to_string(&person).unwrap();
    assert_eq!(result, r#"#("John" 20)"#);
}

#[test]
fn tuple_deserializes_from_list() {
    let person: Person = from_str(r#"("John" 20)"#).unwrap();
    assert_eq!(person, Person("John".to_string(), 20));
}

#[test]
#[cfg(feature = "tuple_as_list")]
fn nested_tuples_serialize_as_lists() {
    #[derive(Serialize)]
    struct Data {
        person: Person,
        coords: (i32, i32),
    }

    let data = Data {
        person: Person("Alice".to_string(), 30),
        coords: (10, 20),
    };

    let result = to_string(&data).unwrap();
    assert!(result.contains(r#"(person "Alice" 30)"#));
    assert!(result.contains("(coords 10 20)"));
    assert!(!result.contains('#'));
}

#[test]
#[cfg(not(feature = "tuple_as_list"))]
fn nested_tuples_serialize_as_vectors() {
    #[derive(Serialize)]
    struct Data {
        person: Person,
        coords: (i32, i32),
    }

    let data = Data {
        person: Person("Alice".to_string(), 30),
        coords: (10, 20),
    };

    let result = to_string(&data).unwrap();
    assert!(result.contains(r#"(person . #("Alice" 30))"#));
    assert!(result.contains("(coords . #(10 20))"));
}

#[test]
fn vec_always_serializes_as_list() {
    let vec = vec![1, 2, 3];
    let result = to_string(&vec).unwrap();
    assert_eq!(result, "(1 2 3)");
    assert!(!result.contains('#'));
}

#[test]
fn struct_always_serializes_as_list() {
    #[derive(Serialize)]
    struct Named {
        name: String,
        age: u8,
    }

    let data = Named {
        name: "Bob".to_string(),
        age: 25,
    };

    let result = to_string(&data).unwrap();
    assert!(result.contains("(name"));
    assert!(result.contains("(age"));
    assert!(!result.contains('#'));
}

// =============================================================================
// bool_as_tnil feature tests
// =============================================================================

#[test]
#[cfg(feature = "bool_as_tnil")]
fn bool_as_tnil_true() {
    let value = true;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "t");

    // Roundtrip test
    let parsed: bool = from_str("t").unwrap();
    assert_eq!(parsed, true);

    // Case sensitivity depends on sym_as_lower feature
    #[cfg(feature = "sym_as_lower")]
    {
        let parsed_upper: bool = from_str("T").unwrap();
        assert_eq!(parsed_upper, true);
    }
    #[cfg(not(feature = "sym_as_lower"))]
    {
        let parsed_upper: Result<bool, _> = from_str("T");
        assert!(parsed_upper.is_err());
    }
}

#[test]
#[cfg(not(feature = "bool_as_tnil"))]
fn scheme_bool_true() {
    // Scheme: #t is true
    let value = true;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "#t");

    let parsed: bool = from_str("#t").unwrap();
    assert_eq!(parsed, true);
}

#[test]
#[cfg(feature = "bool_as_tnil")]
fn bool_as_tnil_false() {
    let value = false;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "nil");

    // Roundtrip test
    let parsed: bool = from_str("nil").unwrap();
    assert_eq!(parsed, false);

    // Null as false
    let parsed_null: bool = from_str("()").unwrap();
    assert_eq!(parsed_null, false);

    // Case sensitivity depends on sym_as_lower feature
    #[cfg(feature = "sym_as_lower")]
    {
        let parsed_upper: bool = from_str("NIL").unwrap();
        assert_eq!(parsed_upper, false);
    }
    #[cfg(not(feature = "sym_as_lower"))]
    {
        let parsed_upper: Result<bool, _> = from_str("NIL");
        assert!(parsed_upper.is_err());
    }
}

#[test]
#[cfg(not(feature = "bool_as_tnil"))]
fn scheme_bool_false() {
    // Scheme: #f is false
    let value = false;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "#f");

    let parsed: bool = from_str("#f").unwrap();
    assert_eq!(parsed, false);
}

#[test]
fn nil_as_option_none() {
    // Lisp: nil/() represents empty/null
    let value: Option<i32> = None;
    let result = to_string(&value).unwrap();
    println!("Option::None serializes to: {}", result);

    let parsed: Option<i32> = from_str("()").unwrap();
    assert_eq!(parsed, None);
}

#[test]
fn symbol_case_handling() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[cfg_attr(feature = "sym_as_lower", serde(rename_all = "lowercase"))]
    enum Command {
        Open,
        Close,
        Save,
    }

    let cmd = Command::Open;
    let result = to_string(&cmd).unwrap();

    #[cfg(feature = "sym_as_lower")]
    {
        assert_eq!(result, "open");

        // Case-insensitive with sym_as_lower
        let parsed: Command = from_str("open").unwrap();
        assert_eq!(parsed, Command::Open);

        let parsed_upper: Command = from_str("OPEN").unwrap();
        assert_eq!(parsed_upper, Command::Open);

        let parsed_mixed: Command = from_str("Open").unwrap();
        assert_eq!(parsed_mixed, Command::Open);
    }
    #[cfg(not(feature = "sym_as_lower"))]
    {
        assert_eq!(result, "Open");

        // Case-sensitive without sym_as_lower
        let parsed: Command = from_str("Open").unwrap();
        assert_eq!(parsed, Command::Open);
    }
}

#[test]
fn string_serialization() {
    // Lisp: strings are quoted with double quotes
    let value = "Hello Lisp";
    let result = to_string(&value).unwrap();
    assert_eq!(result, r#""Hello Lisp""#);

    let parsed: String = from_str(r#""Hello Lisp""#).unwrap();
    assert_eq!(parsed, "Hello Lisp");
}

#[test]
fn string_with_escapes() {
    // Lisp: strings can contain escaped characters
    let value = "Line1\nLine2\tTabbed";
    let result = to_string(&value).unwrap();
    println!("Escaped string: {}", result);

    let parsed: String = from_str(&result).unwrap();
    assert_eq!(parsed, value);
}

#[test]
fn integer_serialization() {
    // Lisp: integers are just numbers
    let value = 42;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "42");

    let parsed: i32 = from_str("42").unwrap();
    assert_eq!(parsed, 42);
}

#[test]
fn negative_integer() {
    let value = -123;
    let result = to_string(&value).unwrap();
    assert_eq!(result, "-123");

    let parsed: i32 = from_str("-123").unwrap();
    assert_eq!(parsed, -123);
}

#[test]
fn float_serialization() {
    // Lisp: real numbers use decimal point
    let value = 3.14159;
    let result = to_string(&value).unwrap();
    println!("Float serializes to: {}", result);

    let parsed: f64 = from_str("3.14159").unwrap();
    assert!((parsed - 3.14159).abs() < 0.00001);
}

#[test]
fn list_of_numbers() {
    // Lisp: (1 2 3 4 5)
    let value = vec![1, 2, 3, 4, 5];
    let result = to_string(&value).unwrap();
    assert_eq!(result, "(1 2 3 4 5)");

    let parsed: Vec<i32> = from_str("(1 2 3 4 5)").unwrap();
    assert_eq!(parsed, vec![1, 2, 3, 4, 5]);
}

#[test]
fn list_of_strings() {
    // Lisp: ("apple" "banana" "cherry")
    let value = vec!["apple", "banana", "cherry"];
    let result = to_string(&value).unwrap();
    assert_eq!(result, r#"("apple" "banana" "cherry")"#);

    let parsed: Vec<String> = from_str(r#"("apple" "banana" "cherry")"#).unwrap();
    assert_eq!(parsed, vec!["apple", "banana", "cherry"]);
}

#[test]
fn nested_list() {
    // Lisp: ((1 2) (3 4) (5 6))
    let value = vec![vec![1, 2], vec![3, 4], vec![5, 6]];
    let result = to_string(&value).unwrap();
    assert_eq!(result, "((1 2) (3 4) (5 6))");

    let parsed: Vec<Vec<i32>> = from_str("((1 2) (3 4) (5 6))").unwrap();
    assert_eq!(parsed, vec![vec![1, 2], vec![3, 4], vec![5, 6]]);
}

#[test]
fn association_list() {
    // Lisp: ((name . "John") (age . 30))
    use std::collections::HashMap;

    let mut map = HashMap::new();
    map.insert("name", "John");
    map.insert("age", "30");

    let result = to_string(&map).unwrap();
    println!("HashMap serializes to: {}", result);
    // HashMap order is not guaranteed, but format should be assoc list
    assert!(result.contains("name"));
    assert!(result.contains("John"));
}

#[test]
fn struct_as_assoc_list() {
    // Lisp: ((x . 10) (y . 20))
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Point {
        x: i32,
        y: i32,
    }

    let point = Point { x: 10, y: 20 };
    let result = to_string(&point).unwrap();
    println!("Struct serializes to: {}", result);

    // Check it's in assoc list format
    assert!(result.contains("(x"));
    assert!(result.contains("(y"));

    let parsed: Point = from_str(&result).unwrap();
    assert_eq!(parsed, point);
}

#[test]
fn mixed_types_in_list() {
    // Lisp can have heterogeneous lists, but Rust needs enum
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(untagged)]
    enum Value {
        Int(i32),
        Str(String),
        Float(f64),
    }

    let values = vec![
        Value::Int(42),
        Value::Str("hello".to_string()),
        Value::Float(3.14),
    ];

    let result = to_string(&values).unwrap();
    println!("Mixed list: {}", result);

    let parsed: Vec<Value> = from_str(&result).unwrap();
    assert_eq!(parsed, values);
}

// =============================================================================
// sym_as_lower feature tests
// =============================================================================

#[test]
#[cfg(feature = "sym_as_lower")]
fn nested_struct() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Inner {
        value: i32,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Outer {
        name: String,
        inner: Inner,
    }

    // Test with various case combinations
    let data = Outer {
        name: "test".to_string(),
        inner: Inner { value: 42 },
    };

    let serialized = to_string(&data).unwrap();
    println!("Serialized: {}", serialized);

    // Deserialize with different cases
    let parsed1: Outer = from_str(r#"((name . "test") (inner . ((value . 42))))"#).unwrap();
    assert_eq!(parsed1, data);

    let parsed2: Outer = from_str(r#"((NAME . "test") (INNER . ((VALUE . 42))))"#).unwrap();
    assert_eq!(parsed2, data);

    let parsed3: Outer = from_str(r#"((Name . "test") (Inner . ((Value . 42))))"#).unwrap();
    assert_eq!(parsed3, data);
}

#[test]
#[cfg(feature = "sym_as_lower")]
fn nested_enum() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "lowercase")]
    enum Status {
        Active,
        Inactive,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "lowercase")]
    enum Command {
        Start { status: Status },
        Stop,
    }

    let cmd = Command::Start {
        status: Status::Active,
    };
    let serialized = to_string(&cmd).unwrap();
    println!("Serialized: {}", serialized);

    // Different cases should all work
    let parsed1: Command = from_str(r#"(start (status . active))"#).unwrap();
    assert_eq!(parsed1, cmd);

    let parsed2: Command = from_str(r#"(START (STATUS . ACTIVE))"#).unwrap();
    assert_eq!(parsed2, cmd);

    let parsed3: Command = from_str(r#"(Start (Status . Active))"#).unwrap();
    assert_eq!(parsed3, cmd);
}

#[test]
#[cfg(feature = "sym_as_lower")]
fn mixed_nested() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "lowercase")]
    enum Action {
        Create,
        Delete,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Config {
        action: Action,
        count: i32,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Request {
        config: Config,
    }

    let req = Request {
        config: Config {
            action: Action::Create,
            count: 5,
        },
    };

    let serialized = to_string(&req).unwrap();
    println!("Serialized: {}", serialized);

    // All these should work
    let parsed1: Request = from_str(r#"((config . ((action . create) (count . 5))))"#).unwrap();
    assert_eq!(parsed1, req);

    let parsed2: Request = from_str(r#"((CONFIG . ((ACTION . CREATE) (COUNT . 5))))"#).unwrap();
    assert_eq!(parsed2, req);

    let parsed3: Request = from_str(r#"((Config . ((Action . Create) (Count . 5))))"#).unwrap();
    assert_eq!(parsed3, req);
}

#[test]
#[cfg(feature = "sym_as_lower")]
fn deeply_nested() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Level3 {
        deep_value: String,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Level2 {
        level3: Level3,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Level1 {
        level2: Level2,
    }

    let data = Level1 {
        level2: Level2 {
            level3: Level3 {
                deep_value: "nested".to_string(),
            },
        },
    };

    // All case variations
    let parsed: Level1 =
        from_str(r#"((LEVEL2 . ((LEVEL3 . ((DEEP_VALUE . "nested"))))))"#).unwrap();
    assert_eq!(parsed, data);
}
