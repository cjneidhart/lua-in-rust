use std::mem::swap;
use std::result;

use crate::instr::Instr;
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<String>,
}

#[derive(Debug)]
pub enum ParseError {
    StatementStart(Token),
    Unsupported,
    Expect(Token),
    ExprEof,
    TooManyNumbers,
    TooManyStrings,
    Other,
}

type Result<T> = result::Result<T, ParseError>;

/// Tracks the current state, to make parsing easier.
#[derive(Debug)]
struct Parser {
    input: Vec<Token>,
    output: Vec<Instr>,
    lookahead: Option<Token>,
    string_literals: Vec<String>,
    number_literals: Vec<f64>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let mut input = tokens.into_iter().rev().collect::<Vec<_>>();
        let lookahead = input.pop();
        Parser {
            input,
            lookahead,
            output: Vec::new(),
            string_literals: Vec::new(),
            number_literals: Vec::new(),
        }
    }

    fn parse_chunk(mut self) -> Result<Chunk> {
        self.parse_statements()?;

        let c = Chunk {
            code: self.output,
            number_literals: self.number_literals,
            string_literals: self.string_literals,
        };

        Ok(c)
    }

    fn parse_statements(&mut self) -> Result<()> {
        loop {
            match self.lookahead {
                None | Some(Token::Eof) | Some(Token::End) => break,
                _ => self.parse_stmt()?,
            };
        }

        Ok(())
    }

    fn parse_stmt(&mut self) -> Result<()> {
        match self.lookahead {
            Some(Token::Identifier(_)) => self.parse_assign()?,
            Some(Token::If) => self.parse_if()?,
            Some(Token::Print) => self.parse_print()?,
            Some(ref t) => {
                return Err(ParseError::StatementStart(t.clone()));
            }
            None => {
                return Ok(());
            }
        };

        if let Some(Token::Semi) = self.lookahead {
            self.next();
        }

        Ok(())
    }

    fn parse_if(&mut self) -> Result<()> {
        self.next();
        self.parse_expr()?;

        self.expect(Token::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;

        // TODO else, elseif
        self.expect(Token::End)?;

        let body_len = self.output.len() as isize;
        old_output.push(Instr::BranchFalse(body_len));
        old_output.append(&mut self.output);
        self.output = old_output;

        Ok(())
    }

    fn parse_assign(&mut self) -> Result<()> {
        let name = match self.next() {
            Some(Token::Identifier(name)) => name,
            _ => return Err(ParseError::Other),
        };
        let i = find_or_add(&mut self.string_literals, name);
        self.push(Instr::PushString(i));
        self.expect(Token::Assign)?;
        self.parse_expr()?;
        self.push(Instr::Assign);
        Ok(())
    }

    fn parse_print(&mut self) -> Result<()> {
        self.expect(Token::Print)?;
        self.parse_expr()?;
        self.push(Instr::Print);
        Ok(())
    }

    /// Parse the input as a single expression.
    fn parse_expr(&mut self) -> Result<()> {
        self.parse_or()
    }

    /// Attempt to parse an 'or' expression. Precedence 8.
    fn parse_or(&mut self) -> Result<()> {
        self.parse_and()?;
        loop {
            if let Some(Token::Or) = self.lookahead {
                self.next();
                let mut old_output = Vec::new();
                swap(&mut self.output, &mut old_output);
                self.push(Instr::Pop);
                self.parse_and()?;
                let right_side_len = self.output.len();
                old_output.push(Instr::BranchTrueKeep(right_side_len as isize));
                old_output.append(&mut self.output);
                self.output = old_output;
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Attempt to parse an 'and' expression. Precedence 7.
    fn parse_and(&mut self) -> Result<()> {
        self.parse_comparison()?;
        loop {
            if let Some(Token::And) = self.lookahead {
                self.next();
                let mut old_output = Vec::new();
                swap(&mut self.output, &mut old_output);
                // If it doesn't short circuit, we discard the left value.
                self.push(Instr::Pop);
                self.parse_comparison()?;
                let right_side_len = self.output.len();
                old_output.push(Instr::BranchFalseKeep(right_side_len as isize));
                old_output.append(&mut self.output);
                self.output = old_output;
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
            } else if let Some(Token::Equal) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Equal);
            } else if let Some(Token::NotEqual) = self.lookahead {
                self.next();
                self.parse_concat()?;
                self.push(Instr::NotEqual);
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
            self.parse_unary()?;
            self.push(Instr::Not);
        } else if let Some(Token::Hash) = self.lookahead {
            self.next();
            self.parse_unary()?;
            self.push(Instr::Length);
        } else if let Some(Token::Minus) = self.lookahead {
            self.next();
            self.parse_unary()?;
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
        self.parse_primary()?;
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
    fn parse_primary(&mut self) -> Result<()> {
        match self.next() {
            Some(Token::LParen) => {
                self.parse_expr()?;
                self.expect(Token::RParen)?;
            }
            Some(Token::Identifier(name)) => {
                let i = find_or_add(&mut self.string_literals, name);
                self.push(Instr::PushString(i));
                self.push(Instr::GlobalLookup);
            }
            Some(Token::LiteralNumber(n)) => {
                let i = find_or_add(&mut self.number_literals, n);
                self.push(Instr::PushNum(i));
            }
            Some(Token::LiteralString(s)) => {
                let i = find_or_add(&mut self.string_literals, s);
                self.push(Instr::PushString(i));
            }
            Some(Token::Nil) => self.push(Instr::PushNil),
            Some(Token::False) => self.push(Instr::PushBool(false)),
            Some(Token::True) => self.push(Instr::PushBool(true)),

            Some(Token::DotDotDot) | Some(Token::Function) => {
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
            _ => Err(ParseError::Expect(tok)),
        }
    }
}

pub fn parse_chunk(tokens: Vec<Token>) -> result::Result<Chunk, ParseError> {
    let p = Parser::new(tokens);

    p.parse_chunk()
}

/// Returns the index of a number in the literals list, adding it if it does not exist.
fn find_or_add<T>(queue: &mut Vec<T>, x: T) -> usize
where
    T: PartialEq<T>,
{
    match queue.iter().position(|y| *y == x) {
        Some(i) => i,
        None => {
            let i = queue.len();
            queue.push(x);
            i
        }
    }
}

#[cfg(test)]
mod tests {
    use self::Instr::*;
    use self::Token::*;
    use super::*;

    fn check_it(input: Vec<Token>, output: Chunk) {
        assert_eq!(parse_chunk(input).unwrap(), output);
    }

    #[test]
    fn test1() {
        let input = vec![
            Token::Print,
            LiteralNumber(5.0),
            Plus,
            LiteralNumber(6.0),
            Eof,
        ];
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Add, Instr::Print],
            number_literals: vec![5.0, 6.0],
            string_literals: vec![],
        };
        check_it(input, out);
    }

    #[test]
    fn test2() {
        let input = vec![
            Token::Print,
            Minus,
            LiteralNumber(5.0),
            Caret,
            LiteralNumber(2.0),
            Eof,
        ];
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Pow, Negate, Instr::Print],
            number_literals: vec![5.0, 2.0],
            string_literals: vec![],
        };
        check_it(input, out);
    }

    #[test]
    fn test3() {
        let input = vec![
            Token::Print,
            LiteralNumber(5.0),
            Plus,
            True,
            DotDot,
            LiteralString("hi".to_string()),
            Eof,
        ];
        let out = Chunk {
            code: vec![
                PushNum(0),
                PushBool(true),
                Add,
                PushString(0),
                Concat,
                Instr::Print,
            ],
            number_literals: vec![5.0],
            string_literals: vec!["hi".to_string()],
        };
        check_it(input, out);
    }

    #[test]
    fn test4() {
        let input = vec![
            Token::Print,
            LiteralNumber(1.0),
            DotDot,
            LiteralNumber(2.0),
            Plus,
            LiteralNumber(3.0),
            Eof,
        ];
        let output = Chunk {
            code: vec![
                PushNum(0),
                PushNum(1),
                PushNum(2),
                Add,
                Concat,
                Instr::Print,
            ],
            number_literals: vec![1.0, 2.0, 3.0],
            string_literals: vec![],
        };
        check_it(input, output);
    }

    #[test]
    fn test5() {
        let input = vec![
            Token::Print,
            LiteralNumber(2.0),
            Caret,
            Minus,
            LiteralNumber(3.0),
            Eof,
        ];
        let output = Chunk {
            code: vec![PushNum(0), PushNum(1), Negate, Pow, Instr::Print],
            number_literals: vec![2.0, 3.0],
            string_literals: vec![],
        };
        check_it(input, output);
    }

    #[test]
    fn test6() {
        let input = vec![
            Token::Print,
            Token::Not,
            Token::Not,
            LiteralNumber(1.0),
            Eof,
        ];
        let output = Chunk {
            code: vec![PushNum(0), Instr::Not, Instr::Not, Instr::Print],
            number_literals: vec![1.0],
            string_literals: vec![],
        };
        check_it(input, output);
    }

    #[test]
    fn test7() {
        let input = vec![
            Token::Identifier("a".to_string()),
            Token::Assign,
            LiteralNumber(5.0),
            Eof,
        ];
        let output = Chunk {
            code: vec![PushString(0), PushNum(0), Instr::Assign],
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
        };
        check_it(input, output);
    }

    #[test]
    fn test8() {
        let input = vec![Token::Print, Token::True, Token::And, Token::False];
        let output = Chunk {
            code: vec![
                PushBool(true),
                BranchFalseKeep(2),
                Pop,
                PushBool(false),
                Instr::Print,
            ],
            number_literals: vec![],
            string_literals: vec![],
        };
        check_it(input, output);
    }

    #[test]
    fn test9() {
        let input = vec![Token::Print, LiteralNumber(5.0), Or, Nil, And, True];
        let code = vec![
            PushNum(0),
            BranchTrueKeep(5),
            Pop,
            PushNil,
            BranchFalseKeep(2),
            Pop,
            PushBool(true),
            Instr::Print,
        ];
        let output = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec![],
        };
        check_it(input, output);
    }

    #[test]
    fn test10() {
        let input = vec![
            If,
            True,
            Then,
            Identifier("a".to_string()),
            Token::Assign,
            LiteralNumber(5.0),
            End,
        ];
        let code = vec![
            PushBool(true),
            BranchFalse(3),
            PushString(0),
            PushNum(0),
            Instr::Assign,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
        };
        check_it(input, chunk);
    }

    #[test]
    fn test11() {
        let input = vec![
            If,
            True,
            Then,
            Identifier("a".to_string()),
            Token::Assign,
            LiteralNumber(5.0),
            If,
            True,
            Then,
            Identifier("b".to_string()),
            Token::Assign,
            LiteralNumber(4.0),
            End,
            End,
        ];
        let code = vec![
            PushBool(true),
            BranchFalse(8),
            PushString(0),
            PushNum(0),
            Instr::Assign,
            PushBool(true),
            BranchFalse(3),
            PushString(1),
            PushNum(1),
            Instr::Assign,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
        };
        check_it(input, chunk);
    }
}
