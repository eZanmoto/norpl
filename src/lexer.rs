// Copyright 2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::str::CharIndices;

#[derive(Clone, Debug)]
pub enum Token {
    Ident(String),
    IntLiteral(i64),
    InterpolatedStrLiteral(String),
    QuotedStrLiteral(String),
    UnquotedStrLiteral(String),

    Else,
    False,
    Fn,
    For,
    If,
    Import,
    In,
    Null,
    Return,
    True,
    While,

    Bang,
    BraceClose,
    BraceOpen,
    BracketClose,
    BracketOpen,
    Colon,
    Comma,
    Div,
    Dot,
    Equals,
    GreaterThan,
    LessThan,
    Mod,
    Mul,
    ParenClose,
    ParenOpen,
    QuestionMark,
    Semicolon,
    Sub,
    Sum,

    AndAnd,
    BangEquals,
    ColonColon,
    ColonEquals,
    ColonParenOpen,
    DashGreaterThan,
    DivEquals,
    DollarEquals,
    DotDot,
    EqualsEquals,
    GreaterThanEquals,
    LessThanEquals,
    MulEquals,
    OrOr,
    QuestionQuestion,
    SubEquals,
    SumEquals,
}

#[derive(Debug)]
pub enum LexError {
    Unexpected(Location, char),
    UnexpectedEofAfterDollar(Location),
}

pub struct Lexer<'input> {
    raw_chars: &'input str,
    chars: CharIndices<'input>,
    index: usize,
    cur: Option<(usize, char)>,

    line: usize,
    col: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(chars: &'input str) -> Self {
        let mut char_indices = chars.char_indices();

        let cur = char_indices.next();
        let mut line = 1;
        if let Some((_, '\n')) = cur {
            line += 1;
        }

        Lexer{
            raw_chars: &chars,
            chars: char_indices,
            index: 0,
            cur,

            line,
            col: 1,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == '#' {
                while let Some(c_) = self.peek_char() {
                    if c_ == '\n' {
                        break;
                    }
                    self.next_char();
                }
            } else {
                if !c.is_ascii_whitespace() {
                    return;
                }
                self.next_char();
            }
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        if let Some((_, c)) = self.cur {
            Some(c)
        } else {
            None
        }
    }

    fn next_char(&mut self) {
        if let Some((i, c)) = self.chars.next() {
            self.index = i;
            self.cur = Some((i, c));

            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        } else {
            self.index = self.raw_chars.len();
            self.cur = None;
        }
    }

    fn loc(&mut self) -> Location {
        (self.line, self.col)
    }

    fn next_keyword_or_unquoted_str_literal(&mut self) -> Span {
        let start = self.index;
        let start_loc = self.loc();

        while let Some(c) = self.peek_char() {
            if !c.is_ascii_alphanumeric() && c != '_' {
                break;
            }
            self.next_char();
        }
        let end = self.index;
        let end_loc = self.loc();

        let t =
            match &self.raw_chars[start..end] {
                "else" => Token::Else,
                "fn" => Token::Fn,
                "for" => Token::For,
                "if" => Token::If,
                "import" => Token::Import,
                "in" => Token::In,
                "null" => Token::Null,
                "return" => Token::Return,
                "while" => Token::While,
                "true" => Token::True,
                "false" => Token::False,

                s => Token::UnquotedStrLiteral(s.to_string()),
            };

        (start_loc, t, end_loc)
    }

    fn next_int(&mut self) -> Span {
        let start = self.index;
        let start_loc = self.loc();

        while let Some(c) = self.peek_char() {
            if !c.is_ascii_digit() {
                break;
            }
            self.next_char();
        }
        let end = self.index;
        let end_loc = self.loc();

        let raw_int = &self.raw_chars[start..end];
        let int: i64 = raw_int.parse().unwrap();
        let t = Token::IntLiteral(int);

        (start_loc, t, end_loc)
    }

    fn next_dollar_equals(&mut self, start_loc: Location) -> Span {
        self.next_char();
        let end_loc = self.loc();

        (start_loc, Token::DollarEquals, end_loc)
    }

    fn next_ident(&mut self, start: usize, start_loc: Location) -> Span {
        while let Some(c) = self.peek_char() {
            if !c.is_ascii_alphanumeric() && c != '_' {
                break;
            }
            self.next_char();
        }
        let end = self.index;
        let end_loc = self.loc();

        let id = &self.raw_chars[start + 1..end];
        let t = Token::Ident(id.to_string());

        (start_loc, t, end_loc)
    }

    fn next_quoted_str_literal<F>(&mut self, new_str_token: F) -> Span
    where
        F: FnOnce(String) -> Token
    {
        let start = self.index;
        let start_loc = self.loc();

        self.next_char();

        while let Some(c) = self.peek_char() {
            self.next_char();
            if c == '\'' {
                break;
            }
        }
        let end = self.index;
        let end_loc = self.loc();

        let id = &self.raw_chars[(start + 1)..(end - 1)];

        let t = new_str_token(id.to_string());

        (start_loc, t, end_loc)
    }

    fn next_symbol_token(&mut self, c: char) -> Option<Span> {
        let start_loc = self.loc();

        if let Some(initial_t) = match_single_symbol_token(c) {
            self.next_char();
            let end_loc = self.loc();

            let next_char =
                if let Some(c) = self.peek_char() {
                    c
                } else {
                    return Some((start_loc, initial_t, end_loc));
                };

            let t =
                if let Some(t) = match_double_symbol_token(c, next_char) {
                    t
                } else {
                    return Some((start_loc, initial_t, end_loc));
                };

            self.next_char();
            let end_loc = self.loc();

            Some((start_loc, t, end_loc))
        } else {
            self.next_double_symbol_token(c)
        }
    }

    fn next_double_symbol_token(&mut self, c: char) -> Option<Span> {
        let start_loc = self.loc();

        self.next_char();
        let next_char =
            if let Some(c) = self.peek_char() {
                c
            } else {
                return None;
            };

        self.next_char();
        let end_loc = self.loc();

        let t =
            if let Some(t) = match_double_symbol_token(c, next_char) {
                t
            } else {
                return None
            };

        Some((start_loc, t, end_loc))
    }
}

pub type Span = (Location, Token, Location);

pub type Location = (usize, usize);

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Span, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace_and_comments();

        let c = self.peek_char()?;

        let result =
            if c.is_ascii_alphabetic() {
                Ok(self.next_keyword_or_unquoted_str_literal())
            } else if c.is_ascii_digit() {
                Ok(self.next_int())
            } else if c == '$' {
                // TODO Consider abstracting these out of this section in order
                // to keep clarity between the different blocks.
                let start = self.index;
                let start_loc = self.loc();

                self.next_char();

                let next_c =
                    if let Some(c) = self.peek_char() {
                        c
                    } else {
                        return Some(Err(LexError::UnexpectedEofAfterDollar(self.loc())))
                    };

                if next_c == '\'' {
                    Ok(self.next_quoted_str_literal(|s| Token::InterpolatedStrLiteral(s)))
                } else if next_c == '=' {
                    Ok(self.next_dollar_equals(start_loc))
                } else {
                    Ok(self.next_ident(start, start_loc))
                }
            } else if c == '\'' {
                Ok(self.next_quoted_str_literal(|s| Token::QuotedStrLiteral(s)))
            } else {
                if let Some(t) = self.next_symbol_token(c) {
                    Ok(t)
                } else {
                    Err(LexError::Unexpected(self.loc(), c))
                }
            };

        Some(result)
    }
}

fn match_single_symbol_token(c: char) -> Option<Token> {
    match c {
        '!' => Some(Token::Bang),
        '}' => Some(Token::BraceClose),
        '{' => Some(Token::BraceOpen),
        ']' => Some(Token::BracketClose),
        '[' => Some(Token::BracketOpen),
        ':' => Some(Token::Colon),
        ',' => Some(Token::Comma),
        '/' => Some(Token::Div),
        '.' => Some(Token::Dot),
        '=' => Some(Token::Equals),
        '>' => Some(Token::GreaterThan),
        '<' => Some(Token::LessThan),
        '%' => Some(Token::Mod),
        '*' => Some(Token::Mul),
        ')' => Some(Token::ParenClose),
        '(' => Some(Token::ParenOpen),
        '?' => Some(Token::QuestionMark),
        ';' => Some(Token::Semicolon),
        '-' => Some(Token::Sub),
        '+' => Some(Token::Sum),

        _ => None,
    }
}

fn match_double_symbol_token(a: char, b: char) -> Option<Token> {
    match (a, b) {
        ('&', '&') => Some(Token::AndAnd),
        ('!', '=') => Some(Token::BangEquals),
        (':', ':') => Some(Token::ColonColon),
        (':', '=') => Some(Token::ColonEquals),
        (':', '(') => Some(Token::ColonParenOpen),
        ('-', '>') => Some(Token::DashGreaterThan),
        ('/', '=') => Some(Token::DivEquals),
        ('.', '.') => Some(Token::DotDot),
        ('=', '=') => Some(Token::EqualsEquals),
        ('>', '=') => Some(Token::GreaterThanEquals),
        ('<', '=') => Some(Token::LessThanEquals),
        ('*', '=') => Some(Token::MulEquals),
        ('|', '|') => Some(Token::OrOr),
        ('?', '?') => Some(Token::QuestionQuestion),
        ('-', '=') => Some(Token::SubEquals),
        ('+', '=') => Some(Token::SumEquals),

        _ => None,
    }
}
