use std::io::Write;
use std::str::FromStr;

use failure::{format_err, Error};
use lexpr::{parse, print, Parser, Printer};
use structopt::StructOpt;

#[derive(Debug)]
struct FromFormat(parse::Options);

impl FromStr for FromFormat {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut options = parse::Options::default();
        for setting in s.split(',') {
            let parts = setting.split(':').collect::<Vec<_>>();
            if parts.len() == 1 {
                match setting {
                    "guile" => options = parse::Options::default(),
                    "elisp" => options = parse::Options::elisp(),
                    _ => return Err(format_err!("unknown parsing dialect: {}", setting)),
                }
            } else if parts.len() == 2 {
                match parts[0] {
                    "keywords" => {
                        use parse::KeywordSyntax::*;
                        let syntax = match parts[1] {
                            "prefix" => ColonPrefix,
                            "postfix" => ColonPostfix,
                            "octothorpe" | "hash" => Octothorpe,
                            name => {
                                return Err(format_err!("unknown keyword parsing syntax: {}", name));
                            }
                        };
                        options = options.with_keyword_syntax(syntax);
                    }
                    "nil-symbol" | "nil" => {
                        use parse::NilSymbol::*;
                        let style = match parts[1] {
                            "empty-list" => EmptyList,
                            "default" => Default,
                            "special" => Special,
                            name => {
                                return Err(format_err!(
                                    "unknown nil symbol parsing style: {}",
                                    name
                                ));
                            }
                        };
                        options = options.with_nil_symbol(style);
                    }
                    "t-symbol" | "t" => {
                        use parse::TSymbol::*;
                        let style = match parts[1] {
                            "true" => True,
                            "default" => Default,
                            name => {
                                return Err(format_err!("unknown t symbol parsing style: {}", name));
                            }
                        };
                        options = options.with_t_symbol(style);
                    }
                    _ => unimplemented!(),
                }
            } else {
                return Err(format_err!(
                    "`from` format specification item malformed: {}",
                    setting
                ));
            }
        }
        Ok(FromFormat(options))
    }
}

#[derive(Debug)]
struct ToFormat(print::Options);

impl FromStr for ToFormat {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut options = print::Options::default();
        for setting in s.split(',') {
            let parts = setting.split(':').collect::<Vec<_>>();
            if parts.len() == 1 {
                match setting {
                    "guile" => options = print::Options::default(),
                    "elisp" => options = print::Options::elisp(),
                    _ => return Err(format_err!("unknown print dialect: {}", setting)),
                }
            } else if parts.len() == 2 {
                match parts[0] {
                    "keywords" => {
                        use print::KeywordSyntax::*;
                        let syntax = match parts[1] {
                            "prefix" => ColonPrefix,
                            "postfix" => ColonPostfix,
                            "octothorpe" | "hash" => Octothorpe,
                            name => {
                                return Err(format_err!("unknown keyword print syntax: {}", name));
                            }
                        };
                        options = options.with_keyword_syntax(syntax);
                    }
                    "booleans" | "bool" => {
                        use print::BoolSyntax::*;
                        let syntax = match parts[1] {
                            "token" => Token,
                            "symbol" => Symbol,
                            name => {
                                return Err(format_err!("unknown boolean print syntax: {}", name));
                            }
                        };
                        options = options.with_bool_syntax(syntax);
                    }
                    "nil" => {
                        use print::NilSyntax::*;
                        let syntax = match parts[1] {
                            "token" => Token,
                            "symbol" => Symbol,
                            "empty-list" => EmptyList,
                            "false" => False,
                            name => {
                                return Err(format_err!("unknown boolean print syntax: {}", name));
                            }
                        };
                        options = options.with_nil_syntax(syntax);
                    }
                    name => {
                        return Err(format_err!("unknown print option: {}", name));
                    }
                }
            } else {
                return Err(format_err!(
                    "`to` format specification item malformed: {}",
                    setting
                ));
            }
        }
        Ok(ToFormat(options))
    }
}

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(short = "f", long = "from")]
    from: Option<FromFormat>,

    #[structopt(short = "t", long = "to")]
    to: Option<ToFormat>,
}

impl Opt {
    fn get_from(&self) -> parse::Options {
        self.from
            .as_ref()
            .map(|from| from.0.clone())
            .unwrap_or_else(|| parse::Options::default())
    }
    fn get_to(&self) -> print::Options {
        self.to
            .as_ref()
            .map(|to| to.0.clone())
            .unwrap_or_else(|| print::Options::default())
    }
}

fn main() -> Result<(), Error> {
    let stdin = std::io::stdin();
    let stdin_handle = stdin.lock();
    let stdout = std::io::stdout();
    let stdout_handle = stdout.lock();

    let opt = Opt::from_args();

    let parse_options = opt.get_from();
    let print_options = opt.get_to();

    let mut parser = Parser::from_reader_custom(stdin_handle, parse_options);
    let mut printer = Printer::with_options(stdout_handle, print_options);
    while let Some(value) = parser.parse()? {
        printer.print(&value)?;
        printer.write_all(b"\n")?;
    }
    Ok(())
}
