#![feature(test)]

extern crate test;

use test::{black_box, Bencher};

use lexpr::{from_str, from_str_custom, parse};

#[bench]
fn bench_float_parsing(b: &mut Bencher) {
    b.iter(|| black_box(from_str("-1.360438755021694e308")));
}

#[bench]
fn bench_parsing_keyword_default(b: &mut Bencher) {
    b.iter(|| black_box(from_str("#:some-keyword")))
}

#[bench]
fn bench_parsing_keyword_all_styles(b: &mut Bencher) {
    use parse::KeywordStyle::*;
    let options =
        parse::Options::default().with_keyword_styles(&[ColonPrefix, ColonPostfix, Octothorpe]);
    b.iter(|| black_box(from_str_custom("#:octo :prefix postfix:", options.clone())));
}
