fn main() {
    let data = lexpr::sexp!((#"lucky-number" . 42));

    println!("data: {}", data);
}
