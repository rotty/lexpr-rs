use std::io;

use lexpr::{
    print::{self, Formatter, VectorType},
    Value,
};
use pretty::{BoxAllocator, DocAllocator, DocBuilder};

struct PrettyPrinter<D> {
    allocator: D,
    bump: bumpalo::Bump,
}

impl<D> PrettyPrinter<D> {
    pub fn new(allocator: D) -> Self {
        Self {
            allocator,
            bump: bumpalo::Bump::new(),
        }
    }

    fn to_doc<'b, F>(
        &'b self,
        value: &Value,
        scratch: &mut Vec<u8>,
        formatter: &mut F,
    ) -> DocBuilder<'b, D>
    where
        D: DocAllocator<'b>,
        D::Doc: Clone,
        F: Formatter,
    {
        match value {
            Value::Nil => self.format_text(scratch, |scratch| {
                formatter.write_nil(scratch).unwrap();
            }),
            Value::Null => self.format_text(scratch, |scratch| {
                formatter.write_null(scratch).unwrap();
            }),
            Value::Bool(b) => self.format_text(scratch, |scratch| {
                formatter.write_bool(scratch, *b).unwrap();
            }),
            Value::Number(n) => self.format_text(scratch, |scratch| {
                formatter.write_number(scratch, n).unwrap();
            }),
            Value::Char(c) => self.format_text(scratch, |scratch| {
                formatter.write_char(scratch, *c).unwrap();
            }),
            Value::String(_s) => todo!(),
            Value::Symbol(s) => self.format_text(scratch, |scratch| {
                formatter.write_symbol(scratch, s).unwrap();
            }),
            Value::Keyword(s) => self.format_text(scratch, |scratch| {
                formatter.write_keyword(scratch, s).unwrap();
            }),
            Value::Bytes(b) => self.format_text(scratch, |scratch| {
                formatter.write_bytes(scratch, b).unwrap();
            }),
            Value::Cons(cell) => {
                let open = self.format_text(scratch, |scratch| {
                    formatter.begin_list(scratch).unwrap();
                });
                let close = self.format_text(scratch, |scratch| {
                    formatter.end_list(scratch).unwrap();
                });
                let mut items = Vec::with_capacity(4);
                let mut iter = cell.list_iter();
                for element in iter.by_ref() {
                    items.push(self.to_doc(element, scratch, formatter));
                }
                if let Some(rest) = iter.next() {
                    items.push(self.allocator.text("."));
                    items.push(self.to_doc(rest, scratch, formatter));
                }
                open.append(
                    self.allocator
                        .intersperse(items, self.allocator.softline())
                        .align(),
                )
                .append(close)
            }
            Value::Vector(items) => {
                let open = self.format_text(scratch, |scratch| {
                    formatter
                        .begin_vector(VectorType::Generic, scratch)
                        .unwrap();
                });
                let close = self.format_text(scratch, |scratch| {
                    formatter.end_vector(scratch).unwrap();
                });
                open.append(
                    self.allocator
                        .intersperse(
                            items
                                .iter()
                                .map(|item| self.to_doc(item, scratch, formatter)),
                            self.allocator.softline(),
                        )
                        .align(),
                )
                .append(close)
            }
        }
    }

    fn format_text<'b, W>(&'b self, scratch: &mut Vec<u8>, fmt: W) -> DocBuilder<'b, D>
    where
        D: DocAllocator<'b>,
        W: FnOnce(&mut Vec<u8>),
    {
        scratch.clear();
        fmt(scratch);
        let text = self.bump.alloc_str(std::str::from_utf8(scratch).unwrap());
        self.allocator.text(&*text)
    }

    pub fn pretty<'b, F>(&'b self, value: &Value, mut formatter: F) -> DocBuilder<'b, D>
    where
        D: DocAllocator<'b>,
        D::Doc: Clone,
        F: Formatter,
    {
        let mut scratch = Vec::new();
        self.to_doc(value, &mut scratch, &mut formatter)
    }
}

fn main() {
    let printer = PrettyPrinter::new(BoxAllocator);
    for value in [
        Value::vector(0..100),
        Value::vector(0..10),
        Value::list(0..100),
        Value::list(0..10),
        Value::append(0..10, Value::symbol("rest")),
        Value::append(0..100, Value::symbol("rest")),
    ] {
        let pretty = printer.pretty(&value, print::DefaultFormatter);
        pretty.render(80, &mut io::stdout()).unwrap();
        println!();
    }
}
