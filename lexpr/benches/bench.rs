use criterion::*;

use lexpr::{from_str, from_str_custom, parse};

fn bench_float_parsing(c: &mut Criterion) {
    c.bench_function("float parsing", |b| {
        b.iter(|| black_box(from_str("-1.360438755021694e308")))
    });
}

fn bench_parsing_keyword_default(c: &mut Criterion) {
    c.bench_function("keyword parsing (default settings)", |b| {
        b.iter(|| black_box(from_str("#:some-keyword")))
    });
}

fn bench_parsing_keyword_all_styles(c: &mut Criterion) {
    use parse::KeywordSyntax::*;
    c.bench_function("keyword parsing (all styles)", |b| {
        let options = parse::Options::default().with_keyword_syntaxes(&[
            ColonPrefix,
            ColonPostfix,
            Octothorpe,
        ]);
        b.iter(|| black_box(from_str_custom("#:octo :prefix postfix:", options)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = bench_float_parsing, bench_parsing_keyword_default, bench_parsing_keyword_all_styles
}
criterion_main!(benches);
