//! This module contains functions which can tokenize a string input.

use std::iter::Peekable;
use std::str::CharIndices;

use crate::Error;
use crate::ErrorKind;
use crate::Result;

use super::Token;
use super::TokenType::{self, *};

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
    /// The starting position of the next character.
    pos: usize,
    /// `linebreaks[i]` is the byte offset of the start of line `i`.
    linebreaks: Vec<usize>,
    iter: Peekable<CharIndices<'a>>,
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        TokenStream {
            lexer: Lexer::new(source),
            lookahead: None,
        }
    }

    /// Return the next Token.
    pub fn next(&mut self) -> Result<Token> {
        match self.lookahead.take() {
            Some(token) => Ok(token),
            None => self.lexer.next_token(),
        }
    }

    pub fn peek(&mut self) -> Result<&Token> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.next_token()?);
        }
        Ok(self.lookahead.as_ref().unwrap())
    }

    /// Return the type of the next token without popping it.
    pub fn peek_type(&mut self) -> Result<TokenType> {
        Ok(self.peek()?.typ)
    }

    //pub fn peek_type_is(&mut self, expected_type: TokenType) -> Result<bool> {
    pub fn check_type(&mut self, expected_type: TokenType) -> Result<bool> {
        Ok(self.peek_type()? == expected_type)
    }

    /// Checks the next token's type. If it matches `typ`, it is popped off and
    /// returned as `Some`. Else, we return `None`.
    pub fn try_pop(&mut self, expected_type: TokenType) -> Result<Option<Token>> {
        if self.check_type(expected_type)? {
            Ok(Some(self.next().unwrap()))
        } else {
            Ok(None)
        }
    }

    pub fn line_and_column(&self, pos: usize) -> (usize, usize) {
        self.lexer.line_and_col(pos)
    }

    pub fn pos(&self) -> usize {
        match &self.lookahead {
            Some(token) => token.start,
            None => self.lexer.pos,
        }
    }

    pub fn src(&self) -> &str {
        self.lexer.source
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let linebreaks = vec![0];
        Lexer {
            iter: source.char_indices().peekable(),
            linebreaks,
            pos: 0,
            source,
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let _starts_line = self.consume_whitespace();
        let tok_start = self.pos;
        if let Some(first_char) = self.next_char() {
            let tok_type = match first_char {
                '+' => Plus,
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

                '-' => {
                    if self.try_next('-') {
                        return self.comment();
                    } else {
                        Minus
                    }
                }

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
            Ok(Token::new(tok_type, tok_start, len as u32))
        } else {
            Ok(Token::new(TokenType::EndOfFile, self.pos, 0))
        }
    }

    fn comment(&mut self) -> Result<Token> {
        // TODO multi-line comments
        while let Some(c) = self.next_char() {
            if c == '\n' {
                return self.next_token();
            }
        }
        Ok(Token::new(TokenType::EndOfFile, self.pos, 0))
    }

    fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, c)| *c)
    }

    fn next_char(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((pos, c)) => {
                self.pos = pos + c.len_utf8();
                if c == '\n' {
                    self.linebreaks.push(self.pos);
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
        let (line_num, column) = self.line_and_col(self.pos);
        Error::new(kind, line_num, column)
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

    fn line_and_col(&self, pos: usize) -> (usize, usize) {
        let iter = self.linebreaks.windows(2).enumerate();
        for (line_num, linebreak_pair) in iter {
            if pos < linebreak_pair[1] {
                let column = pos - linebreak_pair[0];
                // lines and columns start counting at 1
                return (line_num + 1, column + 1);
            }
        }
        let line_num = self.linebreaks.len() - 1;
        let column = pos - self.linebreaks.last().unwrap();
        (line_num + 1, column + 1)
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
        _ => Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, tokens: &[(TokenType, usize, u32)], lines: &[usize]) {
        let mut lexer = Lexer::new(input);
        let mut tokens = tokens
            .iter()
            .map(|(typ, start, len)| Token::new(*typ, *start, *len));
        loop {
            let actual = lexer.next_token().unwrap();
            if actual.typ == TokenType::EndOfFile {
                break;
            }
            let expected = tokens.next().unwrap();
            assert_eq!(expected, actual);
        }
        assert!(tokens.next().is_none());
        assert_eq!(lines, lexer.linebreaks.as_slice());
    }

    fn check_line(input: &str, tokens: &[(TokenType, usize, u32)]) {
        check(input, tokens, &[0]);
    }

    #[test]
    fn test_lexer01() {
        let tokens = &[(LiteralNumber, 0, 2)];
        check_line("50", tokens);
    }

    #[test]
    fn test_lexer02() {
        let input = "hi 4 false";
        let tokens = &[(Identifier, 0, 2), (LiteralNumber, 3, 1), (False, 5, 5)];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer03() {
        let input = "hi5";
        let tokens = &[(Identifier, 0, 3)];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer04() {
        let input = "5 + 5";
        let tokens = &[(LiteralNumber, 0, 1), (Plus, 2, 1), (LiteralNumber, 4, 1)];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer05() {
        let input = "print 5 or 6;";
        let tokens = &[
            (Identifier, 0, 5),
            (LiteralNumber, 6, 1),
            (Or, 8, 2),
            (LiteralNumber, 11, 1),
            (Semi, 12, 1),
        ];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer06() {
        let input = "t = {x = 3}";
        let tokens = &[
            (Identifier, 0, 1),
            (Assign, 2, 1),
            (LCurly, 4, 1),
            (Identifier, 5, 1),
            (Assign, 7, 1),
            (LiteralNumber, 9, 1),
            (RCurly, 10, 1),
        ];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer07() {
        let input = "0x5rad";
        let tokens = &[(LiteralHexNumber, 0, 3), (Identifier, 3, 3)];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer08() {
        let input = "print {x = 5,}";
        let tokens = &[
            (Identifier, 0, 5),
            (LCurly, 6, 1),
            (Identifier, 7, 1),
            (Assign, 9, 1),
            (LiteralNumber, 11, 1),
            (Comma, 12, 1),
            (RCurly, 13, 1),
        ];
        check_line(input, tokens);
    }

    #[test]
    fn test_lexer09() {
        let input = "print()\nsome_other_function(an_argument)\n";
        let tokens = &[
            (Identifier, 0, 5),
            (LParen, 5, 1),
            (RParen, 6, 1),
            (Identifier, 8, 19),
            (LParen, 27, 1),
            (Identifier, 28, 11),
            (RParen, 39, 1),
        ];
        let linebreaks = &[0, 8, 41];
        check(input, tokens, linebreaks);
    }

    #[test]
    fn test_lexer10() {
        let input = "\n\n2\n456\n";
        let tokens = &[(LiteralNumber, 2, 1), (LiteralNumber, 4, 3)];
        let linebreaks = &[0, 1, 2, 4, 8];
        check(input, tokens, linebreaks);
    }

    #[test]
    fn test_lexer11() {
        let input = "-- basic test\nprint('hi' --comment\n )\n";
        let tokens = &[
            (Identifier, 14, 5),
            (LParen, 19, 1),
            (LiteralString, 20, 4),
            (RParen, 36, 1),
        ];
        let linebreaks = &[0, 14, 35, 38];
        check(input, tokens, linebreaks);
    }
}
