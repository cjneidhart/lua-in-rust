//! This module contains functions which can tokenize a string input.

use std::error::Error;
use std::fmt::{self, Display, Formatter};

use crate::Token;
use crate::TokenType::{self, *};

#[derive(Debug)]
pub enum LexerError {
    UnclosedString(usize),
    InvalidCharacter(u8, usize),
    BadNumber(usize),
}
impl Error for LexerError {}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LexerError::*;
        match self {
            UnclosedString(i) => write!(f, "Unclosed string, starting at offset {}", i),
            InvalidCharacter(c, i) => write!(f, "Invalid character '{}' at offset {}", c, i),
            BadNumber(i) => write!(f, "Malformed number at offset {}", i),
        }
    }
}

pub struct TokenList<'a> {
    pub text: &'a [u8],
    pub tokens: Vec<Token>,
    //pub linebreaks: Vec<usize>,
}

pub type Result<T> = std::result::Result<T, LexerError>;

pub fn lex(input: &[u8]) -> Result<TokenList> {
    let lexer = Lexer {
        input,
        tok_start: 0,
        linebreaks: Vec::new(),
        pos: 0,
    };
    lexer.tokenize()
}

struct Lexer<'a> {
    input: &'a [u8],
    tok_start: usize,
    linebreaks: Vec<usize>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn tokenize(mut self) -> Result<TokenList<'a>> {
        let mut tokens = Vec::new();
        self.tok_start = self.pos;
        while let Some(c) = self.next() {
            self.tok_start = self.pos - 1;
            let tok_type = match c {
                b' ' | b'\n' | b'\r' => continue,
                b'+' => Plus,
                b'-' => Minus,
                b'*' => Star,
                b'/' => Slash,
                b'%' => Mod,
                b'^' => Caret,
                b'#' => Hash,
                b';' => Semi,
                b':' => Colon,
                b',' => Comma,
                b'(' => LParen,
                b')' => RParen,
                b'{' => LCurly,
                b'}' => RCurly,
                b']' => RSquare,
                b'.' => self.peek_dot()?,
                b'=' | b'<' | b'>' | b'~' => self.peek_equals(c)?,
                b'\'' => self.lex_string(true)?,
                b'\"' => self.lex_string(false)?,
                b'[' => {
                    if let Some(b'=') = self.peek() {
                        LSquare
                    } else {
                        panic!("Long strings are not supported yet.");
                    }
                }
                _ => {
                    if c.is_ascii_digit() {
                        self.lex_full_number(c)?
                    } else if c.is_ascii_alphabetic() || c == b'_' {
                        self.lex_word()
                    } else {
                        return Err(LexerError::InvalidCharacter(c, self.tok_start));
                    }
                }
            };
            tokens.push(self.make_token(tok_type));
        }

        let output = TokenList {
            tokens,
            text: self.input,
            //linebreaks: self.linebreaks,
        };
        Ok(output)
    }

    fn next(&mut self) -> Option<u8> {
        let output = self.peek();
        if let Some(c) = output {
            if c == b'\n' {
                self.linebreaks.push(self.pos);
            }
            self.pos += 1;
        }
        output
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).cloned()
    }

    /// Move a character forward, only if the current character matches
    /// `expected`.
    fn try_next(&mut self, expected: u8) -> bool {
        match self.peek() {
            Some(c) if c == expected => {
                self.next();
                true
            }
            _ => false,
        }
    }

    fn make_token(&self, typ: TokenType) -> Token {
        Token::new(typ, self.tok_start, (self.pos - self.tok_start) as u32)
    }

    /// The lexer just read a `.`. Determine whether it was a:
    /// - `Dot`: table access
    /// - `DotDot`: String concatenation
    /// - `DotDotDot`: Variadic arguments
    /// - Number: if it's the form `.4`, for example
    ///
    /// This will return `Err` if the number is invalid.
    fn peek_dot(&mut self) -> Result<TokenType> {
        let typ = match self.peek() {
            Some(b'.') => {
                self.next();
                if self.try_next(b'.') {
                    DotDotDot
                } else {
                    DotDot
                }
            }
            Some(c) if c.is_ascii_digit() => {
                self.next();
                self.lex_number_after_decimal()?;
                LiteralNumber
            }
            _ => Dot,
        };
        Ok(typ)
    }

    /// The lexer just read something which might be part of a two-character
    /// operator, with `=` as the second character.
    ///
    /// Return `Err` if the first character is `~` and it is not paired with a
    /// `=`
    fn peek_equals(&mut self, first_char: u8) -> Result<TokenType> {
        if self.try_next(b'=') {
            let typ = match first_char {
                b'=' => Equal,
                b'~' => NotEqual,
                b'<' => LessEqual,
                b'>' => GreaterEqual,
                _ => panic!("peek_equals was called with invalid first_char"),
            };
            Ok(typ)
        } else {
            match first_char {
                b'=' => Ok(Assign),
                b'<' => Ok(Less),
                b'>' => Ok(Greater),
                b'~' => Err(LexerError::InvalidCharacter(first_char, self.tok_start)),
                _ => panic!(),
            }
        }
    }

    /// Tokenize a 'short' literal string, AKA a string denoted by single or
    /// double quotes and not by two square brackets.
    fn lex_string(&mut self, is_single_quotes: bool) -> Result<TokenType> {
        let mut found_matching_quote = false;
        while let Some(c) = self.next() {
            if (is_single_quotes && c == b'\'') || (!is_single_quotes && c == b'\"') {
                found_matching_quote = true;
                break;
            } else if c == b'\\' {
                if self.next().is_none() {
                    return Err(LexerError::UnclosedString(self.tok_start));
                }
            } else if c == b'\n' {
                return Err(LexerError::UnclosedString(self.tok_start));
            }
        }

        if found_matching_quote {
            Ok(LiteralString)
        } else {
            Err(LexerError::UnclosedString(self.tok_start))
        }
    }

    /// Read in a number which starts with a digit (as opposed to a decimal
    /// point).
    fn lex_full_number(&mut self, first_digit: u8) -> Result<TokenType> {
        // Check for hex values
        if first_digit == b'0' && self.try_next(b'x') {
            // Has to be at least one digit
            match self.next() {
                Some(c) if c.is_ascii_hexdigit() => (),
                _ => return Err(LexerError::BadNumber(self.tok_start)),
            }
            while let Some(c) = self.peek() {
                if c.is_ascii_hexdigit() {
                    self.next();
                } else {
                    break;
                }
            }
            match self.peek() {
                Some(c) if c.is_ascii_alphabetic() => Err(LexerError::BadNumber(self.tok_start)),
                _ => Ok(LiteralHexNumber),
            }
        } else {
            // Read in the rest of the base
            self.lex_digits();

            // Handle the fraction and exponent components.
            if self.try_next(b'.') {
                match self.peek() {
                    Some(c) if c.is_ascii_digit() => self.lex_number_after_decimal()?,
                    _ => self.lex_exponent()?,
                }
            } else {
                self.lex_exponent()?
            }

            Ok(LiteralNumber)
        }
    }

    /// Read in a literal number which had no digits before the decimal point.
    fn lex_number_after_decimal(&mut self) -> Result<()> {
        self.lex_digits();
        self.lex_exponent()
    }

    /// Read in an unbroken sequence of digits.
    fn lex_digits(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Read in the optional exponent part of a literal number. Then, check
    /// for any trailing letters.
    fn lex_exponent(&mut self) -> Result<()> {
        if self.try_next(b'E') || self.try_next(b'e') {
            // The exponent might have a sign.
            if let Some(c) = self.peek() {
                if c == b'+' || c == b'-' {
                    self.next();
                }
            }

            self.lex_digits();
        }
        match self.peek() {
            Some(c) if c.is_ascii_alphabetic() => Err(LexerError::BadNumber(self.tok_start)),
            _ => Ok(()),
        }
    }

    fn lex_word(&mut self) -> TokenType {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == b'_' {
                self.next();
            } else {
                break;
            }
        }

        keyword_match(&self.input[self.tok_start..self.pos])
    }
}

fn keyword_match(s: &[u8]) -> TokenType {
    match s {
        b"and" => And,
        b"break" => Break,
        b"do" => Do,
        b"else" => Else,
        b"elseif" => ElseIf,
        b"end" => End,
        b"false" => False,
        b"for" => For,
        b"function" => Function,
        b"if" => If,
        b"in" => In,
        b"local" => Local,
        b"nil" => Nil,
        b"not" => Not,
        b"or" => Or,
        b"repeat" => Repeat,
        b"return" => Return,
        b"then" => Then,
        b"true" => True,
        b"until" => Until,
        b"while" => While,
        b"print" => Print,
        _ => Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const TOK: fn(TokenType, usize, u32) -> Token = Token::new;

    fn lex_force(s: &[u8]) -> Vec<Token> {
        lex(s).unwrap().tokens
    }

    #[test]
    fn test_lexer1() {
        let out = vec![Token {
            typ: LiteralNumber,
            start: 0,
            len: 2,
        }];
        assert_eq!(lex_force(b"50"), out);
    }

    #[test]
    fn test_lexer2() {
        let out = vec![
            TOK(Identifier, 0, 2),
            TOK(LiteralNumber, 3, 1),
            TOK(False, 5, 5),
        ];
        assert_eq!(out, lex_force(b"hi 4 false"));
    }

    #[test]
    fn test_lexer3() {
        let input = b"hi5";
        let out = vec![TOK(Identifier, 0, 3)];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer4() {
        let input = b"5 + 5";
        let out = vec![
            TOK(LiteralNumber, 0, 1),
            TOK(Plus, 2, 1),
            TOK(LiteralNumber, 4, 1),
        ];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer5() {
        let input = b"print 5 or 6;";
        let out = vec![
            TOK(Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(Or, 8, 2),
            TOK(LiteralNumber, 11, 1),
            TOK(Semi, 12, 1),
        ];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer6() {
        let input = b"t = {x = 3}";
        let out = vec![
            TOK(Identifier, 0, 1),
            TOK(TokenType::Assign, 2, 1),
            TOK(LCurly, 4, 1),
            TOK(Identifier, 5, 1),
            TOK(TokenType::Assign, 7, 1),
            TOK(LiteralNumber, 9, 1),
            TOK(RCurly, 10, 1),
        ];
        assert_eq!(lex_force(input), out);
    }
}
