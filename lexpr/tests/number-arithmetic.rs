use lexpr::Number;

#[test]
fn u64_add() {
    for (n1, n2, expected) in vec![(0u64, 0u64, 0u64),
                                   (42, 23, 65)] {
        assert_eq!(Number::from(n1) + Number::from(n2), Number::from(expected));
    }
}
