use crate::value::Value;

use proc_macro2::{Delimiter, Literal, TokenStream, TokenTree};

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
                '-' => Ok(Value::Negated(self.parse_literal()?)),
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

    fn parse_literal(&mut self) -> Result<proc_macro2::Literal, ParseError> {
        match self.token()? {
            TokenTree::Literal(literal) => Ok(literal.clone()),
            tt => Err(ParseError::UnexpectedToken(tt.clone())),
        }
    }

    fn parse_symbol(&mut self) -> Result<String, ParseError> {
        match self.token()? {
            TokenTree::Literal(lit) => string_literal(lit),
            TokenTree::Ident(ident) => Ok(ident.to_string()),
            tt => Err(ParseError::UnexpectedToken(tt.clone())),
        }
    }

    fn parse_octothorpe(&mut self) -> Result<Value, ParseError> {
        let token = self.token()?;
        match token {
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' => Ok(Value::Keyword(self.parse_symbol()?)),
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
    while let Some(_) = parser.peek() {
        elements.push(parser.parse()?);
    }
    Ok(Value::Vector(elements))
}

pub fn parse(tokens: TokenStream) -> Result<Value, ParseError> {
    let mut parser = Parser::new(tokens.into_iter().collect());
    parser.parse()
}
