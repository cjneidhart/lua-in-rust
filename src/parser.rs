use std::result;

use simple_types::Token;
use simple_types::Instr;

#[derive(Debug)]
pub enum ParseError {
    Unsupported,
    Expect,
    ExprEof,
    Other,
}

type Result<T> = result::Result<T, ParseError>;

/// Tracks the current state, to make parsing easier.
struct Parser {
    input: Vec<Token>,
    output: Vec<Instr>,
    lookahead: Option<Token>,
}

impl Parser {
    /// Parse the input as a single expression.
    fn parse_expr(mut self) -> Result<Vec<Instr>> {
        self.parse_logic()?;
        match self.lookahead {
            Some(Token::Eof) => Ok(self.output),
            _ => Err(ParseError::ExprEof),
        }
    }

    /// Attempt to parse a logical expression. Precedence 7.
    ///
    /// `and`, `or`
    fn parse_logic(&mut self) -> Result<()> {
        self.parse_comparison()?;
        loop {
            if let Some(Token::And) = self.lookahead {
                self.next();
                self.parse_comparison()?;
                self.push(Instr::And);
            } else if let Some(Token::Or) = self.lookahead {
                self.next();
                self.parse_comparison()?;
                self.push(Instr::Or);
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Parse a comparison expression. Precedence 6.
    ///
    /// `==`, `~=`, `<`, `<=`, `>`, `>=`
    fn parse_comparison(&mut self) -> Result<()> {
        self.parse_concat()?;
        loop {
            if let Some(Token::Less) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Less);
            } else if let Some(Token::LessEqual) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::LessEqual);
            } else if let Some(Token::Greater) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Greater);
            } else if let Some(Token::GreaterEqual) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::GreaterEqual);
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Parse a string concatenation expression. Precedence 5.
    ///
    /// `..`
    fn parse_concat(&mut self) -> Result<()> {
        self.parse_addition()?;
        if let Some(Token::DotDot) = self.lookahead {
            self.next();
            self.parse_concat()?;
            self.push(Instr::Concat);
        }

        Ok(())
    }

    /// Parse an addition expression. Precedence 4.
    ///
    /// `+`, `-`
    fn parse_addition(&mut self) -> Result<()> {
        self.parse_multiplication()?;
        loop {
            if let Some(Token::Plus) = self.lookahead {
                self.next();
                self.parse_multiplication()?;
                self.push(Instr::Add);
            } else if let Some(Token::Minus) = self.lookahead {
                self.next();
                self.parse_multiplication()?;
                self.push(Instr::Subtract);
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Parse a multiplication expression. Precedence 3.
    ///
    /// `*`, `/`, `%`
    fn parse_multiplication(&mut self) -> Result<()> {
        self.parse_unary()?;
        loop {
            if let Some(Token::Star) = self.lookahead {
                self.next();
                self.parse_unary()?;
                self.push(Instr::Multiply);
            } else if let Some(Token::Slash) = self.lookahead {
                self.next();
                self.parse_unary()?;
                self.push(Instr::Divide);
            } else if let Some(Token::Mod) = self.lookahead {
                self.next();
                self.parse_unary()?;
                self.push(Instr::Mod);
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Parse a unary expression. Precedence 2. Note the `^` operator has a
    /// higher precedence than unary operators.
    ///
    /// `not`, `#`, `-`
    fn parse_unary(&mut self) -> Result<()> {
        //let lookahead = &self.lookahead;
        if let Some(Token::Not) = self.lookahead {
            self.next();
            self.parse_pow()?;
            self.push(Instr::Not);
        } else if let Some(Token::Hash) = self.lookahead {
            self.next();
            self.parse_pow()?;
            self.push(Instr::Length);
        } else if let Some(Token::Minus) = self.lookahead {
            self.next();
            self.parse_pow()?;
            self.push(Instr::Negate);
        } else {
            self.parse_pow()?;
        }

        Ok(())
    }

    /// Parse an exponentiation expression. Right-associative, Precedence 1.
    ///
    /// `^`
    fn parse_pow(&mut self) -> Result<()> {
        self.parse_other_exp()?;
        if let Some(Token::Caret) = self.lookahead {
            self.next();
            self.parse_unary()?;
            self.push(Instr::Pow);
        }

        Ok(())
    }

    /// Parse an expression, after eliminating any operators. This can be:
    ///
    /// * An identifier
    /// * A table lookup (`table[key]` or `table.key`)
    /// * A function call
    /// * A literal number
    /// * A literal string
    /// * A function definition
    /// * One of the keywords `nil`, `false` or `true
    fn parse_other_exp(&mut self) -> Result<()> {
        match self.next() {
            Some(Token::LParen) => {
                self.next();
                self.parse_logic()?;
                self.expect(Token::RParen)?;
            }
            Some(Token::LiteralNumber(n)) => self.push(Instr::PushNum(n)),
            Some(Token::Nil) => self.push(Instr::PushNil),
            Some(Token::False) => self.push(Instr::PushBool(false)),
            Some(Token::True) => self.push(Instr::PushBool(true)),
            Some(Token::LiteralString(s)) => self.push(Instr::PushString(s)),
            Some(Token::DotDotDot) | Some(Token::Function) | Some(Token::Identifier(_)) => {
                return Err(ParseError::Unsupported);
            }
            _ => {
                return Err(ParseError::Other);
            }
        }

        Ok(())
    }

    /// Helper function, advances the input and returns the old lookahead.
    fn next(&mut self) -> Option<Token> {
        let mut out = self.input.pop();
        ::std::mem::swap(&mut self.lookahead, &mut out);
        out
    }

    /// Adds an instruction to the output.
    fn push(&mut self, instr: Instr) {
        self.output.push(instr);
    }

    /// Pulls a token off the input and checks it against `tok`.
    fn expect(&mut self, tok: Token) -> Result<()> {
        match self.next() {
            Some(ref t) if *t == tok => Ok(()),
            _ => Err(ParseError::Expect),
        }
    }
}

pub fn parse_expr(tokens: Vec<Token>) -> result::Result<Vec<Instr>, ParseError> {
    let output: Vec<Instr> = Vec::new();
    let mut input: Vec<Token> = tokens.iter().rev().cloned().collect();
    let lookahead = input.pop();
    let p = Parser {
        input,
        output,
        lookahead,
    };

    p.parse_expr()
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::Token::*;
    use self::Instr::*;

    #[test]
    fn test1() {
        let input = vec![LiteralNumber(5.0), Plus, LiteralNumber(6.0), Eof];
        let out = vec![Instr::PushNum(5.0), Instr::PushNum(6.0), Instr::Add];
        assert_eq!(parse_expr(input).unwrap(), out);
    }

    #[test]
    fn test2() {
        let input = vec![Minus, LiteralNumber(5.0), Caret, LiteralNumber(2.0), Eof];
        let out = vec![PushNum(5.0), PushNum(2.0), Pow, Negate];
        assert_eq!(parse_expr(input).unwrap(), out);
    }

    #[test]
    fn test3() {
        let input = vec![LiteralNumber(5.0), Plus, True, DotDot, LiteralString("hi".to_string()), Eof];
        let out = vec![PushNum(5.0), PushBool(true), Add, PushString("hi".to_string()), Concat];
        assert_eq!(parse_expr(input).unwrap(), out);
    }

    #[test]
    fn test4() {
        let input = vec![LiteralNumber(1.0), DotDot, LiteralNumber(2.0), Plus, LiteralNumber(3.0), Eof];
        let output = vec![PushNum(1.0), PushNum(2.0), PushNum(3.0), Add, Concat];
        assert_eq!(parse_expr(input).unwrap(), output);
    }

    #[test]
    fn test5() {
        let input = vec![LiteralNumber(2.0), Caret, Minus, LiteralNumber(3.0), Eof];
        let output = vec![PushNum(2.0), PushNum(3.0), Negate, Pow];
        assert_eq!(parse_expr(input).unwrap(), output);
    }
}