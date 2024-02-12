use criterion::*;

use lexpr::{from_str, from_str_custom, parse};

fn bench_float_parsing(c: &mut Criterion) {
    c.bench_function("float parsing", |b| {
        b.iter(|| from_str(black_box("-1.360438755021694e308")))
    });
}

fn bench_parsing_keyword_default(c: &mut Criterion) {
    c.bench_function("keyword parsing (default settings)", |b| {
        b.iter(|| from_str(black_box("#:some-keyword")))
    });
}

fn bench_parsing_keyword_all_styles(c: &mut Criterion) {
    use parse::KeywordSyntax::*;
    c.bench_function("keyword parsing (all styles)", |b| {
        let options = parse::Options::default().with_keyword_syntaxes([
            ColonPrefix,
            ColonPostfix,
            Octothorpe,
        ]);
        b.iter(|| {
            for input in ["#:octo", ":prefix", "postfix:"] {
                from_str_custom(black_box(input), options).unwrap();
            }
        })
    });
}

fn bench_write_bytes(c: &mut Criterion) {
    let bytes = lexpr::Value::bytes(vec![123u8; 1024]);
    c.bench_function("byte vector serialization", |b| {
        b.iter(|| lexpr::to_string(black_box(&bytes)))
    });
}

fn bench_write_string(c: &mut Criterion) {
    let value = lexpr::Value::string("A string\nwith\nsome escapes");
    c.bench_function("short string with escapes serialization", |b| {
        b.iter(|| lexpr::to_string(black_box(&value)))
    });
}

criterion_group! {
    name = parse_benches;
    config = Criterion::default();
    targets = bench_float_parsing, bench_parsing_keyword_default, bench_parsing_keyword_all_styles,
}
criterion_group! {
    name = write_benches;
    config = Criterion::default();
    targets = bench_write_bytes, bench_write_string,
}
criterion_main!(parse_benches, write_benches);
