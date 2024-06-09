use anyhow::Context as _;
use clap::{Parser, ValueEnum};
use std::{
    fs::File,
    io::{self, BufRead, BufReader, BufWriter},
    path::PathBuf,
};

#[derive(Parser, Debug)]
struct Args {
    input: Option<PathBuf>,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(short = 'f', long = "from")]
    from_syntax: Option<FromSyntax>,
    #[arg(short = 't', long = "to")]
    to_syntax: Option<ToSyntax>,
}

impl Args {
    fn parse_options(&self) -> lexpr::parse::Options {
        match self.from_syntax.unwrap_or_default() {
            FromSyntax::R6RS => lexpr::parse::Options::new(),
            FromSyntax::Guile => lexpr::parse::Options::default(),
            FromSyntax::Racket => {
                lexpr::parse::Options::default().with_racket_hash_percent_symbols(true)
            }
            FromSyntax::Elisp => lexpr::parse::Options::elisp(),
            FromSyntax::Kicad => lexpr::parse::Options::new().with_leading_digit_symbols(true),
        }
    }
    fn print_options(&self) -> lexpr::print::Options {
        match self.to_syntax.unwrap_or_default() {
            ToSyntax::R6RS => lexpr::print::Options::default(),
            ToSyntax::Elisp => lexpr::print::Options::elisp(),
        }
    }
}

#[derive(ValueEnum, Default, Clone, Copy, Debug)]
enum FromSyntax {
    #[default]
    R6RS,
    Guile,
    Racket,
    Elisp,
    Kicad,
}

#[derive(ValueEnum, Default, Clone, Copy, Debug)]
enum ToSyntax {
    #[default]
    R6RS,
    Elisp,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let input: Box<dyn BufRead> = if let Some(path) = &args.input {
        let file = File::open(path)
            .with_context(|| format!("cannot open input file {}", path.display()))?;
        Box::new(BufReader::new(file))
    } else {
        Box::new(io::stdin().lock())
    };
    let output: Box<dyn io::Write> = if let Some(path) = &args.output {
        let file = File::create_new(path)
            .with_context(|| format!("cannot open output file {}", path.display()))?;
        Box::new(BufWriter::new(file))
    } else {
        Box::new(io::stdout().lock())
    };
    let mut parser = lexpr::Parser::from_reader_custom(input, args.parse_options());
    let mut printer = lexpr::Printer::with_options(output, args.print_options());
    for value in parser.value_iter() {
        let value = value?;
        printer.print(&value)?;
    }
    Ok(())
}
