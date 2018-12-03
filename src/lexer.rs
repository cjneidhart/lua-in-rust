//! This module contains functions which can tokenize a string input.

use std::iter::Peekable;
use std::str::Chars;
use token::Token;
use token::Token::*;


#[derive(Debug)]
pub enum LexerError {
    UnclosedString,
    InvalidCharacter,
    BadEscape,
    Other,
}

pub type Result<T> = ::std::result::Result<T, LexerError>;

pub fn lex(input: &str) -> Result<Vec<Token>> {
    let mut output = Vec::<Token>::new();
    let mut chars = input.chars().peekable();
    loop {
        let tok = tokenize(&mut chars)?;
        if let Token::Eof = tok {
            output.push(tok);
            break;
        } else {
            output.push(tok);
        }
    }

    Ok(output)
}

/// Pull a single token off of the iterator, and return it.
fn tokenize(input: &mut Peekable<Chars>) -> Result<Token> {
    if let Some(c) = input.next() {
        match c {
            ' ' | '\n' | '\r' => tokenize(input),
            '+' => Ok(Plus),
            '-' => Ok(Minus),
            '*' => Ok(Star),
            '/' => Ok(Slash),
            '%' => Ok(Mod),
            '^' => Ok(Caret),
            '#' => Ok(Hash),
            ';' => Ok(Semi),
            ':' => Ok(Colon),
            ',' => Ok(Comma),
            '(' => Ok(LParen),
            ')' => Ok(RParen),
            '{' => Ok(LCurly),
            '}' => Ok(RCurly),
            '[' => Ok(LSquare),
            ']' => Ok(RSquare),
            '.' => Ok(peek_dot(input)),
            '=' | '<' | '>' | '~' => peek_equals(c, input),
            '\'' => lex_string(input, true),
            '\"' => lex_string(input, false),
            _ => if c.is_digit(10) {
                lex_number(c, input)
            } else if c.is_alphabetic() || c == '_' {
                lex_ident(c, input)
            } else {
                Err(LexerError::InvalidCharacter)
            }
        }
    } else {
        Ok(Eof)
    }
}

/// Lex an identifier, and check it for keyword status.
fn lex_ident(c: char, input: &mut Peekable<Chars>) -> Result<Token> {
    let mut s = String::new();
    s.push(c);
    loop {
        let should_continue = match input.peek() {
            Some(c) => c.is_alphanumeric() || *c == '_',
            None => false,
        };

        if should_continue {
            s.push(input.next().unwrap());
        } else {
            break;
        }
    }

    Ok(keyword_match(s))
}

/// Check if a String matches with a keyword.
fn keyword_match(s: String) -> Token {
    match s.as_str() {
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
        _ => Identifier(s),
    }
}

/// Parse in a number literal. Currently only supports decimal integers.
fn lex_number(c: char, input: &mut Peekable<Chars>) -> Result<Token> {
    let mut s = String::new();
    s.push(c);
    loop {
        let should_continue = match input.peek() {
            Some(c) => c.is_digit(10),
            None => false,
        };

        if should_continue {
            s.push(input.next().unwrap());
        } else {
            break;
        }
    }

    Ok(LiteralNumber(s.parse().unwrap()))
}

/// Tokenize a literal string. Octal escapes and raw strings are not supported.
fn lex_string(input: &mut Peekable<Chars>, is_single_quotes: bool) -> Result<Token> {
    let mut output = String::new();
    loop {
        if let Some(c) = input.next() {
            if (is_single_quotes && c == '\'') || (!is_single_quotes && c == '\"') {
                break;
            } else if c == '\\' {
                if let Some(c2) = input.next() {
                    match c2 {
                        '\'' | '\"' => output.push(c2),
                        'a' => output.push('\x07'),
                        'b' => output.push('\x08'),
                        'f' => output.push('\x0C'),
                        'n' => output.push('\n'),
                        'r' => output.push('\r'),
                        't' => output.push('\t'),
                        'v' => output.push('\x0B'),
                        _ => return Err(LexerError::BadEscape),
                    }
                } else {
                    return Err(LexerError::UnclosedString)
                }
            } else {
                output.push(c);
            }
        } else {
            return Err(LexerError::UnclosedString);
        }
    }

    Ok(LiteralString(output))
}

/// The lexer just read something which might be part of a 2-character operator,
/// with '=' as the second character.
fn peek_equals(first_char: char, input: &mut Peekable<Chars>) -> Result<Token> {
    if let Some('=') = input.peek() {
        let _ = input.next();
        match first_char {
            '=' => Ok(Equal),
            '~' => Ok(NotEqual),
            '<' => Ok(LessEqual),
            '>' => Ok(GreaterEqual),
            _ => panic!(),
        }
    } else {
        match first_char {
            '=' => Ok(Assign),
            '<' => Ok(Less),
            '>' => Ok(Greater),
            '~' => Err(LexerError::InvalidCharacter),
            _ => panic!(),
        }
    }
}

/// The lexer just read a '.'. Determine whether it was a:
/// - Dot: table access
/// - DotDot: String concatenation
/// - DotDotDot: Variadic arguments
fn peek_dot(input: &mut Peekable<Chars>) -> Token {
    if let Some('.') = input.peek() {
        let _ = input.next();
        if let Some('.') = input.peek() {
            let _ = input.next();
            DotDotDot
        } else {
            DotDot
        }
    } else {
        Dot
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_force(s: &str) -> Vec<Token> {
        lex(s).unwrap()
    }

    #[test]
    fn test1() {
        assert_eq!(vec![Token::LiteralNumber(50.0), Eof], lex_force("50"));
    }

    #[test]
    fn test2() {
        assert_eq!(vec![Identifier("hi".to_string()), LiteralNumber(4.0), False, Eof], lex_force("hi 4false"));
    }

    #[test]
    fn test3() {
        let input = "hi5";
        let out = vec![Identifier("hi5".to_string()), Eof];
        assert_eq!(lex_force(input), out);
    }

    #[test]
    fn test4() {
        let input = "5 + 5";
        let out = vec![LiteralNumber(5.0), Plus, LiteralNumber(5.0), Eof];
        assert_eq!(lex_force(input), out);
    }
}