use std::{hash::Hash, io};

use crate::{
    char_reader::{CharReader, CharReaderSaver},
    string_storage::StringStorage,
};

mod string_interner;

use string_interner::StringInterner;

#[derive(Debug)]
pub struct TokenizationError {
    pub kind: TokenizationErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum TokenizationErrorKind {
    Unexpected,
    UnexpectedEof,
    Io(io::Error),
}

impl From<io::Error> for TokenizationError {
    fn from(err: io::Error) -> Self {
        TokenizationError {
            kind: TokenizationErrorKind::Io(err),
            span: None,
        }
    }
}

type Result<T> = std::result::Result<T, TokenizationError>;

pub struct Tokens<'s, R> {
    chars: R,
    strings: StringInterner<'s>,
    peek: Option<Token<'s>>,
}

#[derive(Debug, Clone)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'s> {
    /* Keywords */
    Def,
    Use,
    Val,
    Set,
    Type,
    Case,
    Else,
    For,
    In,

    /* Punctuation */
    Dot,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Bang,
    Pipe,
    Amp,
    ThinArrow,
    FatArrow,
    ColonEqual,
    Equal,
    NotEqual,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    AmpAmp,
    PipePipe,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    DotOpenBrace,
    OpenBrace,
    CloseBrace,

    Float(f64),
    Integer(u64),
    Name(Intern<'s>),
    String(Intern<'s>),
}

#[derive(Debug, Clone, Copy)]
pub struct Intern<'s>(pub &'s str);

impl<'s> PartialEq for Intern<'s> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'s> Eq for Intern<'s> {}

impl<'s> Hash for Intern<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const str as *const u8 as usize)
    }
}

impl<'s> Token<'s> {
    fn new(kind: TokenKind<'s>, span: Span) -> Token<'s> {
        Token { kind, span }
    }
}

impl<'s, R: CharReader> Tokens<'s, R> {
    pub fn of(chars: R, string_storage: &'s StringStorage) -> Tokens<'s, R> {
        Tokens {
            chars,
            strings: StringInterner::new(string_storage),
            peek: None,
        }
    }

    /// Reads the next token from in input stream.
    pub fn next(&mut self) -> Result<Option<Token<'s>>> {
        if let Some(peek) = self.peek.take() {
            return Ok(Some(peek));
        }

        while let Some((start, ch)) = self.chars.peek()? {
            return match ch {
                _ if ch.is_ascii_whitespace() => {
                    self.chars.next()?;
                    continue;
                }
                '.' => self.advance_double(TokenKind::Dot, |ch| match ch {
                    '{' => Some(TokenKind::DotOpenBrace),
                    _ => None,
                }),
                ',' => self.advance_single(TokenKind::Comma),
                ':' => self.advance_double(TokenKind::Colon, |ch| match ch {
                    ':' => Some(TokenKind::ColonColon),
                    '=' => Some(TokenKind::ColonEqual),
                    _ => None,
                }),
                ';' => self.advance_single(TokenKind::Semicolon),
                '!' => self.advance_double(TokenKind::Bang, |ch| match ch {
                    '=' => Some(TokenKind::NotEqual),
                    _ => None,
                }),
                '|' => self.advance_double(TokenKind::Pipe, |ch| match ch {
                    '|' => Some(TokenKind::PipePipe),
                    _ => None,
                }),
                '&' => self.advance_double(TokenKind::Amp, |ch| match ch {
                    '&' => Some(TokenKind::AmpAmp),
                    _ => None,
                }),
                '-' => self.advance_double(TokenKind::Minus, |ch| match ch {
                    '>' => Some(TokenKind::ThinArrow),
                    _ => None,
                }),
                '=' => self.advance_double(TokenKind::Equal, |ch| match ch {
                    '>' => Some(TokenKind::FatArrow),
                    _ => None,
                }),
                '>' => self.advance_double(TokenKind::Gt, |ch| match ch {
                    '=' => Some(TokenKind::GtEq),
                    _ => None,
                }),
                '<' => self.advance_double(TokenKind::Lt, |ch| match ch {
                    '=' => Some(TokenKind::LtEq),
                    _ => None,
                }),
                '+' => self.advance_single(TokenKind::Plus),
                '*' => self.advance_single(TokenKind::Star),
                '/' => self.advance_single(TokenKind::Slash),
                '%' => self.advance_single(TokenKind::Percent),
                '^' => self.advance_single(TokenKind::Caret),
                '(' => self.advance_single(TokenKind::OpenParen),
                ')' => self.advance_single(TokenKind::CloseParen),
                '[' => self.advance_single(TokenKind::OpenBracket),
                ']' => self.advance_single(TokenKind::CloseBracket),
                '{' => self.advance_single(TokenKind::OpenBrace),
                '}' => self.advance_single(TokenKind::CloseBrace),
                '"' => self.string(),
                _ if ch.is_alphabetic() || ch == '_' => self.name(),
                _ if ch.is_ascii_digit() => self.number(),
                _ => {
                    self.chars.next()?;
                    Err(TokenizationError {
                        kind: TokenizationErrorKind::Unexpected,
                        span: Some(Span {
                            start,
                            end: start + ch.len_utf8(),
                        }),
                    })
                }
            };
        }

        Ok(None)
    }

    pub fn peek(&mut self) -> Result<Option<&Token<'s>>> {
        if let Some(ref peek) = self.peek {
            Ok(Some(peek))
        } else {
            self.peek = self.next()?;
            Ok(self.peek.as_ref())
        }
    }
}

impl<'s, R: CharReader> Tokens<'s, R> {
    fn string(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };
        if ch != '"' {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::Unexpected,
                span: Some(Span {
                    start,
                    end: start + ch.len_utf8(),
                }),
            });
        }

        self.chars.next()?;

        let mut string = std::string::String::new();
        let mut slash = false;
        while let Some((curr, ch)) = self.chars.peek()? {
            if ch == '"' && !slash {
                break;
            }

            self.chars.next()?;
            if slash {
                match ch {
                    '"' => string += "\"",
                    '\0' => string += "\0",
                    't' => string += "\t",
                    'n' => string += "\n",
                    'r' => string += "\r",
                    '\\' => string += "\\",
                    _ => {
                        return Err(TokenizationError {
                            kind: TokenizationErrorKind::Unexpected,
                            span: Some(Span {
                                start: curr,
                                end: curr + ch.len_utf8(),
                            }),
                        })
                    }
                }
                slash = false
            } else if ch == '\\' {
                slash = true
            } else {
                string.push(ch);
            }
        }

        let Some((inner_end, end_ch)) = self.chars.next()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };

        Ok(Some(Token {
            kind: TokenKind::String(self.strings.intern(string)),
            span: Span {
                start,
                end: inner_end + end_ch.len_utf8(),
            },
        }))
    }

    fn name(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };
        if !ch.is_alphabetic() && ch != '_' {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::Unexpected,
                span: Some(Span {
                    start,
                    end: start + ch.len_utf8(),
                }),
            });
        }

        let mut saver = CharReaderSaver::with_capacity(&mut self.chars, 16);

        while let Some((_, ch)) = saver.peek()? {
            if !ch.is_alphanumeric() && ch != '_' {
                break;
            } else {
                saver.next()?;
            }
        }

        let name = saver.finish();
        let end = start + name.len();

        Ok(Some(Token {
            kind: match &*name {
                "def" => TokenKind::Def,
                "use" => TokenKind::Use,
                "val" => TokenKind::Val,
                "set" => TokenKind::Set,
                "type" => TokenKind::Type,
                "case" => TokenKind::Case,
                "else" => TokenKind::Else,
                "for" => TokenKind::For,
                "in" => TokenKind::In,
                _ => TokenKind::Name(self.strings.intern(name)),
            },
            span: Span { start, end },
        }))
    }

    fn number(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else { return Ok(None) };
        if !ch.is_ascii_digit() {
            return Ok(None);
        }

        let mut seen_point = false;

        let mut saver = CharReaderSaver::with_capacity(&mut self.chars, 16);

        while let Some((_, ch)) = saver.peek()? {
            if !seen_point && ch == '.' {
                seen_point = true;
                saver.next()?;
            } else if !ch.is_ascii_digit() {
                break;
            } else {
                saver.next()?;
            }
        }

        let saved = saver.finish();
        let end = start + saved.len();

        if seen_point {
            let Ok(value) = saved.parse::<f64>() else {
                unreachable!("Compiler bug: Unexpected error from parse::<f64>()")
            };

            Ok(Some(Token {
                kind: TokenKind::Float(value),
                span: Span { start, end },
            }))
        } else {
            let Ok(value) = saved.parse::<u64>() else {
                unreachable!("Compiler bug: Unexpected error from parse::<u128>()")
            };

            Ok(Some(Token {
                kind: TokenKind::Integer(value),
                span: Span { start, end },
            }))
        }
    }

    fn advance_single(&mut self, kind: TokenKind<'s>) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.next()? else { return Ok(None) };
        Ok(Some(Token {
            kind,
            span: Span {
                start,
                end: start + ch.len_utf8(),
            },
        }))
    }

    fn advance_double(
        &mut self,
        primary: TokenKind<'s>,
        secondary: impl FnOnce(char) -> Option<TokenKind<'s>>,
    ) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.next()? else { return Ok(None) };
        if let Some((peek_start, peek)) = self.chars.peek()? {
            if let Some(sec) = secondary(peek) {
                self.chars.next()?;
                return Ok(Some(Token {
                    kind: sec,
                    span: Span {
                        start,
                        end: peek_start + peek.len_utf8(),
                    },
                }));
            }
        }

        Ok(Some(Token {
            kind: primary,
            span: Span {
                start,
                end: start + ch.len_utf8(),
            },
        }))
    }
}
