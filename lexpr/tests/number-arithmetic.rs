use lexpr::Number;

#[test]
fn u64_add() {
    for (n1, n2, expected) in vec![(0u64, 0u64, 0u64),
                                   (42, 23, 65)] {
        assert_eq!(Number::from(n1) + Number::from(n2), Number::from(expected));
    }
}

#[test]
fn i64_add() {
    for (n1, n2, expected) in vec![(0i64, 0i64, 0i64),
                                   (42, -23, 19),
                                   (0, i64::min_value(), i64::min_value())] {
        assert_eq!(Number::from(n1) + Number::from(n2), Number::from(expected));
    }
}
