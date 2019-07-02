//! This module contains functions which can tokenize a string input.

use std::iter::Peekable;
use std::str::CharIndices;

use crate::Error;
use crate::ErrorKind;
use crate::Result;
use crate::Token;
use crate::TokenType::{self, *};

/// A `TokenStream` is a wrapper around a `Lexer`. It provides a lookahead buffer and several
/// helper methods.
#[derive(Debug)]
pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    lookahead: Option<Token>,
}

/// A `Lexer` handles the raw conversion of characters to tokens.
#[derive(Debug)]
pub struct Lexer<'a> {
    pos: usize,
    linebreaks: Vec<usize>,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        TokenStream {
            lexer: Lexer::new(source),
            lookahead: None,
        }
    }

    /// Return the next Token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        match self.lookahead.take() {
            Some(tok) => Ok(Some(tok)),
            None => self.lexer.next_token(),
        }
    }

    pub fn peek(&mut self) -> Result<Option<&Token>> {
        if self.lookahead.is_none() {
            self.lookahead = self.lexer.next_token()?;
        }
        Ok(self.lookahead.as_ref())
    }

    /// Return whether the next token is of the expected type, without popping it.
    pub fn peek_type(&mut self) -> Result<Option<&TokenType>> {
        match self.peek()? {
            Some(tok) => Ok(Some(&tok.typ)),
            None => Ok(None),
        }
    }

    pub fn peek_type_is(&mut self, expected_type: TokenType) -> Result<bool> {
        match self.peek_type()? {
            Some(typ) if *typ == expected_type => Ok(true),
            _ => Ok(false),
        }
    }

    /// Checks the next token's type. If it matches `typ`, it is popped off and
    /// returned as `Some`. Else, we return `None`.
    pub fn try_pop(&mut self, expected_type: TokenType) -> Result<Option<Token>> {
        match self.peek()? {
            Some(t) if t.typ == expected_type => self.next(),
            _ => Ok(None),
        }
    }

    pub fn push_back(&mut self, tok: Token) {
        match self.lookahead {
            Some(_) => {
                panic!("Can't push token back after peeking.");
            }
            None => {
                self.lookahead = Some(tok);
            }
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            iter: source.char_indices().peekable(),
            linebreaks: Vec::new(),
            pos: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        let _starts_line = self.consume_whitespace();
        let tok_start = self.pos;
        if let Some(first_char) = self.next_char() {
            let tok_type = match first_char {
                '+' => Plus,
                '-' => Minus,
                '*' => Star,
                '/' => Slash,
                '%' => Mod,
                '^' => Caret,
                '#' => Hash,
                ';' => Semi,
                ':' => Colon,
                ',' => Comma,
                '(' => LParen,
                ')' => RParen,
                '{' => LCurly,
                '}' => RCurly,
                ']' => RSquare,

                '.' => self.peek_dot(tok_start)?,

                '=' | '<' | '>' | '~' => self.peek_equals(tok_start, first_char)?,

                '\'' => self.lex_string(true, tok_start)?,
                '\"' => self.lex_string(false, tok_start)?,
                '[' => {
                    if let Some('=') | Some('[') = self.peek_char() {
                        panic!("Long strings are not supported yet.");
                    } else {
                        LSquare
                    }
                }

                _ if first_char.is_ascii_digit() => self.lex_full_number(tok_start, first_char)?,

                _ if first_char.is_ascii_alphabetic() || first_char == '_' => {
                    self.lex_word(first_char)
                }

                _ => unimplemented!(),
            };
            let len = self.pos - tok_start;
            Ok(Some(Token::new(tok_type, tok_start, len as u32)))
        } else {
            Ok(None)
        }
    }

    // fn tokenize(mut self) -> Result<TokenList<'a>> {
    //     let mut tokens = VecDeque::new();
    //     self.consume_whitespace();
    //     while let Some((start, c)) = self.iter.next() {
    //         let (num_additional_bytes, tok_type) = match c {
    //             '+' => (0, Plus),
    //             '-' => (0, Minus),
    //             '*' => (0, Star),
    //             '/' => (0, Slash),
    //             '%' => (0, Mod),
    //             '^' => (0, Caret),
    //             '#' => (0, Hash),
    //             ';' => (0, Semi),
    //             ':' => (0, Colon),
    //             ',' => (0, Comma),
    //             '(' => (0, LParen),
    //             ')' => (0, RParen),
    //             '{' => (0, LCurly),
    //             '}' => (0, RCurly),
    //             ']' => (0, RSquare),
    //             '.' => self.peek_dot()?,
    //             '=' | '<' | '>' | '~' => self.peek_equals(c)?,
    //             '\'' => self.lex_string(true, start)?,
    //             '\"' => self.lex_string(false, start)?,
    //             '[' => {
    //                 if let Some('=') | Some('[') = self.peek() {
    //                     panic!("Long strings are not supported yet.");
    //                 } else {
    //                     (0, LSquare)
    //                 }
    //             }
    //             _ => {
    //                 if c.is_ascii_digit() {
    //                     self.lex_full_number(c)?
    //                 } else if c.is_ascii_alphabetic() || c == '_' {
    //                     self.lex_word()
    //                 } else {
    //                     return Err(LexerError::InvalidCharacter(c, self.tok_start));
    //                 }
    //             }
    //         };
    //         let tok = Token::new(tok_type, start, num_additional_bytes + 1);
    //         tokens.push(tok);
    //         self.consume_whitespace();
    //     }

    //     let output = TokenList {
    //         tokens,
    //         text: self.input,
    //         //linebreaks: self.linebreaks,
    //     };
    //     Ok(output)
    // }

    fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, c)| *c)
    }

    fn next_char(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((pos, c)) => {
                self.pos = pos + c.len_utf8();
                if c == '\n' {
                    self.linebreaks.push(pos);
                }
                Some(c)
            }
            None => None,
        }
    }

    /// Consume any whitespace characters
    fn consume_whitespace(&mut self) -> bool {
        let mut ret = false;
        while let Some(c) = self.peek_char() {
            if !c.is_ascii_whitespace() {
                break;
            }
            if c == '\n' {
                ret = true;
            }
            self.next_char();
        }
        ret
    }

    /// Move a character forward, only if the current character matches
    /// `expected`.
    fn try_next(&mut self, expected: char) -> bool {
        match self.peek_char() {
            Some(c) if c == expected => {
                self.next_char();
                true
            }
            _ => false,
        }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        // TODO get actual numbers
        Error::new(kind)
    }

    /// The lexer just read a `.`. Determine whether it was a:
    /// - `Dot`: table access
    /// - `DotDot`: String concatenation
    /// - `DotDotDot`: Variadic arguments
    /// - `Number`: if it's the form `.4`, for example\
    fn peek_dot(&mut self, tok_start: usize) -> Result<TokenType> {
        let typ = match self.peek_char() {
            Some('.') => {
                self.next_char();
                if self.try_next('.') {
                    DotDotDot
                } else {
                    DotDot
                }
            }
            Some(c) if c.is_ascii_digit() => {
                self.next_char();
                self.lex_number_after_decimal(tok_start)?;
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
    fn peek_equals(&mut self, _tok_start: usize, first_char: char) -> Result<TokenType> {
        if self.try_next('=') {
            let typ = match first_char {
                '=' => Equal,
                '~' => NotEqual,
                '<' => LessEqual,
                '>' => GreaterEqual,
                _ => panic!("peek_equals was called with first_char = {}", first_char),
            };
            Ok(typ)
        } else {
            match first_char {
                '=' => Ok(Assign),
                '<' => Ok(Less),
                '>' => Ok(Greater),
                '~' => Err(self.error(ErrorKind::InvalidCharacter)),
                _ => panic!("peek_equals was called with first_char = {}", first_char),
            }
        }
    }

    /// Tokenize a 'short' literal string, AKA a string denoted by single or
    /// double quotes and not by two square brackets.
    fn lex_string(&mut self, is_single_quotes: bool, _tok_start: usize) -> Result<TokenType> {
        while let Some(c) = self.next_char() {
            if (is_single_quotes && c == '\'') || (!is_single_quotes && c == '\"') {
                return Ok(LiteralString);
            } else if c == '\\' {
                // TODO make backslash-escapes actually work. For now, we just
                // ignore the next character, which is the correct behavior for
                // newlines and quotes, but not escapes like '\n'.
                self.next_char();
            } else if c == '\n' {
                return Err(self.error(ErrorKind::UnclosedString));
            }
        }

        Err(self.error(ErrorKind::UnclosedString))
    }

    /// Read in a number which starts with a digit (as opposed to a decimal
    /// point).
    fn lex_full_number(&mut self, tok_start: usize, first_char: char) -> Result<TokenType> {
        // Check for hex values
        if first_char == '0' && self.try_next('x') {
            // Has to be at least one digit
            match self.next_char() {
                Some(c) if c.is_ascii_hexdigit() => (),
                _ => return Err(self.error(ErrorKind::BadNumber)),
            };
            // Read the rest of the numbers
            while let Some(c) = self.peek_char() {
                if c.is_ascii_hexdigit() {
                    self.next_char();
                } else {
                    break;
                }
            }

            match self.peek_char() {
                Some(c) if c.is_ascii_hexdigit() => Err(self.error(ErrorKind::BadNumber)),
                _ => Ok(LiteralHexNumber),
            }
        } else {
            // Read in the rest of the base
            self.lex_digits();

            // Handle the fraction and exponent components.
            if self.try_next('.') {
                match self.peek_char() {
                    Some(c) if c.is_ascii_digit() => self.lex_number_after_decimal(tok_start)?,
                    _ => self.lex_exponent(tok_start)?,
                }
            } else {
                self.lex_exponent(tok_start)?
            }

            Ok(LiteralNumber)
        }
    }

    /// Read in a literal number which had no digits before the decimal point.
    fn lex_number_after_decimal(&mut self, tok_start: usize) -> Result<()> {
        self.lex_digits();
        self.lex_exponent(tok_start)
    }

    /// Read in an unbroken sequence of digits.
    fn lex_digits(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    /// Read in the optional exponent part of a literal number. Then, check
    /// for any trailing letters.
    fn lex_exponent(&mut self, _tok_start: usize) -> Result<()> {
        if self.try_next('E') || self.try_next('e') {
            // The exponent might have a sign.
            if let Some(c) = self.peek_char() {
                if c == '+' || c == '-' {
                    self.next_char();
                }
            }

            self.lex_digits();
        }
        match self.peek_char() {
            Some(c) if c.is_ascii_hexdigit() => Err(self.error(ErrorKind::BadNumber)),
            _ => Ok(()),
        }
    }

    fn lex_word(&mut self, first_char: char) -> TokenType {
        let mut word = String::new();
        word.push(first_char);
        while let Some(c) = self.peek_char() {
            if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_' {
                word.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        keyword_match(&word)
    }
}

fn keyword_match(s: &str) -> TokenType {
    match s {
        "and" => And,
        "break" => Break,
        "do" => Do,
        "else" => Else,
        "elseif" => ElseIf,
        "end" => End,
        "false" => False,
        "for" => For,
        "function" => Function,
        "if" => If,
        "in" => In,
        "local" => Local,
        "nil" => Nil,
        "not" => Not,
        "or" => Or,
        "repeat" => Repeat,
        "return" => Return,
        "then" => Then,
        "true" => True,
        "until" => Until,
        "while" => While,
        "print" => Print,
        _ => Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const TOK: fn(TokenType, usize, u32) -> Token = Token::new;

    fn lex(input: &str) -> Result<Vec<Token>> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(tok) = lexer.next_token()? {
            tokens.push(tok);
        }
        Ok(tokens)
    }

    fn lex_force(s: &str) -> Vec<Token> {
        lex(s).unwrap()
    }

    #[test]
    fn test_lexer1() {
        let out = vec![Token {
            typ: LiteralNumber,
            start: 0,
            len: 2,
        }];
        assert_eq!(lex_force("50"), out);
    }

    #[test]
    fn test_lexer2() {
        let out = vec![
            TOK(Identifier, 0, 2),
            TOK(LiteralNumber, 3, 1),
            TOK(False, 5, 5),
        ];
        assert_eq!(out, lex_force("hi 4 false"));
    }

    #[test]
    fn test_lexer3() {
        let input = "hi5";
        let out = vec![TOK(Identifier, 0, 3)];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer4() {
        let input = "5 + 5";
        let out = vec![
            TOK(LiteralNumber, 0, 1),
            TOK(Plus, 2, 1),
            TOK(LiteralNumber, 4, 1),
        ];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer5() {
        let input = "print 5 or 6;";
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
        let input = "t = {x = 3}";
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

    #[test]
    fn test_lexer7() {
        let input = "0x5rad";
        let out = vec![TOK(LiteralHexNumber, 0, 3), TOK(Identifier, 3, 3)];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test_lexer8() {
        let input = "print {x = 5,}";
        let out = vec![
            TOK(Print, 0, 5),
            TOK(LCurly, 6, 1),
            TOK(Identifier, 7, 1),
            TOK(TokenType::Assign, 9, 1),
            TOK(LiteralNumber, 11, 1),
            TOK(Comma, 12, 1),
            TOK(RCurly, 13, 1),
        ];
        assert_eq!(lex_force(input), out);
    }
}
