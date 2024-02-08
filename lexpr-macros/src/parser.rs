use crate::value::Value;

use proc_macro2::{Delimiter, Literal, Spacing, TokenStream, TokenTree};

#[derive(Debug)]
struct Parser {
    tokens: Vec<TokenTree>,
    index: usize,
}

#[derive(Debug)]
pub enum ParseError {
    ExpectedStringLiteral(Literal),
    UnexpectedToken(TokenTree),
    UnexpectedChar(char),
    UnexpectedDelimiter(Delimiter),
    UnexpectedEnd,
}

impl Parser {
    fn new(tokens: Vec<TokenTree>) -> Self {
        Parser { tokens, index: 0 }
    }

    fn next_token(&mut self) -> Option<&TokenTree> {
        if self.index == self.tokens.len() {
            return None;
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        Some(token)
    }

    fn token(&mut self) -> Result<&TokenTree, ParseError> {
        self.next_token().ok_or(ParseError::UnexpectedEnd)
    }

    fn peek(&mut self) -> Option<&TokenTree> {
        if self.index == self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.index])
    }

    fn eat_token(&mut self) {
        assert!(self.index < self.tokens.len());
        self.index += 1;
    }

    fn parse(&mut self) -> Result<Value, ParseError> {
        match self.token()? {
            TokenTree::Punct(punct) => match punct.as_char() {
                '#' => self.parse_octothorpe(),
                ',' => Ok(Value::Unquoted(self.token()?.clone())),
                c @ ('!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '='
                | '>' | '?' | '@' | '^' | '_' | '~') => {
                    match punct.spacing() {
                        // TODO: A lone '.' is not an identfier
                        Spacing::Joint => Ok(Value::Symbol(self.parse_identifier(c.to_string()))),
                        Spacing::Alone => match c {
                            '-' => match self.peek() {
                                Some(TokenTree::Literal(lit)) => {
                                    let lit = lit.clone();
                                    self.eat_token();
                                    Ok(Value::Negated(lit))
                                }
                                _ => Ok(Value::Symbol(c.to_string())),
                            },
                            ':' => match self.peek() {
                                Some(TokenTree::Literal(lit)) => {
                                    let name = string_literal(lit)?;
                                    self.eat_token();
                                    Ok(Value::Keyword(name))
                                },
                                Some(TokenTree::Ident(ident)) => Ok(Value::Keyword(ident.to_string())),
                                _ => Ok(Value::Symbol(c.to_string())),
                            },
                            ':' => Ok(Value::Keyword(self.parse_identifier(String::new()))),
                            _ => Ok(Value::Symbol(c.to_string())),
                        },
                    }
                }
                c => Err(ParseError::UnexpectedChar(c)),
            },
            TokenTree::Literal(literal) => Ok(Value::Literal(literal.clone())),
            TokenTree::Ident(ident) => Ok(Value::Symbol(ident.to_string())),
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => parse_list(group.stream()),
                delim => Err(ParseError::UnexpectedDelimiter(delim)),
            },
        }
    }

    fn parse_identifier(&mut self, prefix: String) -> String {
        let mut identifier = prefix;
        while let Some(token) = self.peek() {
            match token {
                TokenTree::Punct(punct) => match punct.as_char() {
                    // This is the combination of `<special initial>`
                    // and `<special subsequent>` from R7RS, removing
                    // `_`, which is not a Rust punctuation character,
                    // but (part of) an identfier.
                    '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
                    | '?' | '@' | '^' | '~' => {
                        identifier.push(punct.as_char());
                        let spacing = punct.spacing();
                        self.eat_token();
                        match spacing {
                            Spacing::Joint => {}
                            Spacing::Alone => break,
                        }
                    }
                    _ => break,
                },
                TokenTree::Ident(part) => {
                    identifier.push_str(&part.to_string());
                    self.eat_token();
                    break;
                }
                _ => break,
            }
        }
        identifier
    }

    fn parse_octothorpe(&mut self) -> Result<Value, ParseError> {
        let token = self.token()?;
        match token {
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => match self.peek() {
                    Some(TokenTree::Literal(lit)) => {
                        let name = string_literal(lit)?;
                        self.eat_token();
                        Ok(Value::Keyword(name))
                    }
                    Some(_) => Ok(Value::Keyword(self.parse_identifier(String::new()))),
                    None => Err(ParseError::UnexpectedEnd),
                },
                c => Err(ParseError::UnexpectedChar(c)),
            },
            TokenTree::Literal(lit) => Ok(Value::Symbol(string_literal(lit)?)),
            TokenTree::Ident(ident) => {
                let name = ident.to_string();
                match name.as_str() {
                    "t" => Ok(Value::Bool(true)),
                    "f" => Ok(Value::Bool(false)),
                    "nil" => Ok(Value::Nil),
                    _ => Err(ParseError::UnexpectedToken(token.clone())),
                }
            }
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Parenthesis => parse_vector(group.stream()),
                delim => Err(ParseError::UnexpectedDelimiter(delim)),
            },
        }
    }
}

fn string_literal(lit: &Literal) -> Result<String, ParseError> {
    let s = lit.to_string();
    if s.starts_with('"') {
        Ok(s[1..s.len() - 1].to_string())
    } else {
        Err(ParseError::ExpectedStringLiteral(lit.clone()))
    }
}

fn parse_list(tokens: TokenStream) -> Result<Value, ParseError> {
    let mut elements = vec![];
    let mut tail = None;
    let mut parser = Parser::new(tokens.into_iter().collect());
    while let Some(token) = parser.peek() {
        if let TokenTree::Punct(punct) = token {
            if punct.as_char() == '.' {
                if tail.is_some() {
                    return Err(ParseError::UnexpectedChar('.'));
                }
                parser.eat_token();
                tail = Some(parser.parse()?);
                continue;
            }
        }
        elements.push(parser.parse()?);
    }
    match tail {
        Some(rest) => match rest {
            Value::List(rest_list) => {
                elements.extend(rest_list);
                Ok(Value::List(elements))
            }
            Value::ImproperList(rest_list, rest) => {
                elements.extend(rest_list);
                Ok(Value::ImproperList(elements, rest))
            }
            _ => Ok(Value::ImproperList(elements, Box::new(rest))),
        },
        None => Ok(Value::List(elements)),
    }
}

fn parse_vector(tokens: TokenStream) -> Result<Value, ParseError> {
    let mut elements = vec![];
    let mut parser = Parser::new(tokens.into_iter().collect());
    while parser.peek().is_some() {
        elements.push(parser.parse()?);
    }
    Ok(Value::Vector(elements))
}

pub fn parse(tokens: TokenStream) -> Result<Value, ParseError> {
    let mut parser = Parser::new(tokens.into_iter().collect());
    parser.parse()
}
