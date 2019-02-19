use std::mem::swap;
use std::result;
use std::u8;

use crate::instr::Instr;
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<String>,
    pub num_locals: u8,
}

#[derive(Debug)]
pub enum ParseError {
    StatementStart(Token),
    Unsupported,
    Unexpected(Token),
    Expect(Token),
    ExprEof,
    UnexpectedEof,
    TooManyNumbers,
    TooManyStrings,
    TooManyLocals,
    Complexity,
    Other,
}

type Result<T> = result::Result<T, ParseError>;

/// Tracks the current state, to make parsing easier.
#[derive(Debug)]
struct Parser {
    input: Vec<Token>,
    output: Vec<Instr>,
    string_literals: Vec<String>,
    number_literals: Vec<f64>,
    nest_level: i32,
    locals: Vec<(String, i32)>,
    num_locals: u8,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        let input = tokens.into_iter().rev().collect();
        Parser {
            input,
            output: Vec::new(),
            string_literals: Vec::new(),
            number_literals: Vec::new(),
            nest_level: 0,
            locals: Vec::new(),
            num_locals: 0,
        }
    }

    fn parse_chunk(mut self) -> Result<Chunk> {
        self.parse_statements()?;
        if let Some(x) = self.next() {
            Err(ParseError::Unexpected(x))
        } else {
            let c = Chunk {
                code: self.output,
                number_literals: self.number_literals,
                string_literals: self.string_literals,
                num_locals: self.num_locals,
            };

            Ok(c)
        }
    }

    fn parse_statements(&mut self) -> Result<()> {
        while let Some(token) = self.next() {
            use Token::*;
            match token {
                Identifier(name) => self.parse_ident_stmt(name)?,
                If => self.parse_if()?,
                Print => self.parse_print()?,
                While => self.parse_while()?,
                Repeat => self.parse_repeat()?,
                Do => self.parse_do()?,
                Local => self.parse_local()?,
                For => self.parse_for()?,
                _ => {
                    // Put the input token back if it doesn't start a statement
                    self.input.push(token);
                    break;
                }
            }
            if let Some(Semi) = self.peek() {
                self.next();
            }
        }

        Ok(())
    }

    /// Parse a statement which starts with an identifier. It could be an
    /// assignment or a lone function call.
    fn parse_ident_stmt(&mut self, name: String) -> Result<()> {
        match self.peek() {
            Some(Token::Comma) | Some(Token::Assign) => self.parse_assign(name),
            Some(Token::LParen) => self.parse_call_stmt(name),
            _ => Err(ParseError::Expect(Token::Assign)),
        }
    }

    fn parse_call_stmt(&mut self, name: String) -> Result<()> {
        self.parse_identifier(name)?;
        // Consume opening paren.
        self.next();
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<()> {
        let num_args = if let Some(Token::RParen) = self.peek() {
            0
        } else {
            self.parse_explist()?
        };
        self.push(Instr::Call(num_args));
        self.expect(Token::RParen)?;

        Ok(())
    }

    /// Parse a for loop, before we know whether it's generic (`for i in t do`) or
    /// numeric (`for i = 1,5 do`).
    fn parse_for(&mut self) -> Result<()> {
        let name = if let Some(Token::Identifier(name)) = self.next() {
            name
        } else {
            return Err(ParseError::Other);
        };
        self.nest_level += 1;
        self.expect(Token::Assign)?;
        self.parse_numeric_for(name)?;
        self.level_down();

        Ok(())
    }

    /// Parse a numeric for, starting with the first expression after the `=`.
    fn parse_numeric_for(&mut self, name: String) -> Result<()> {
        // The start(current), stop and step are stored in three "hidden" local slots.
        let current_index_slot = self.locals.len();
        self.add_local(String::new())?;
        self.add_local(String::new())?;
        self.add_local(String::new())?;

        // The actual local is in a fourth slot, so that it can be reassigned to.
        self.add_local(name)?;

        // First, all 3 control expressions are evaluated.
        self.parse_expr()?;
        self.expect(Token::Comma)?;
        self.parse_expr()?;
        // optional step value
        match self.next() {
            Some(Token::Comma) => {
                self.parse_expr()?;
                self.expect(Token::Do)?;
            }
            Some(Token::Do) => {
                let i = self.find_or_add_number(1.0)?;
                self.push(Instr::PushNum(i));
            }
            _ => {
                return Err(ParseError::Expect(Token::Comma));
            }
        }

        // The ForPrep command pulls three values off the stack and places them
        // into locals to use in the loop.
        self.push(Instr::ForPrep(current_index_slot));

        // body
        let old_len = self.output.len() as isize;
        self.parse_statements()?;
        self.expect(Token::End)?;
        self.push(Instr::ForLoop(
            current_index_slot,
            old_len - self.output.len() as isize - 1,
        ));

        Ok(())
    }

    fn parse_local(&mut self) -> Result<()> {
        let name = if let Some(Token::Identifier(name)) = self.next() {
            name
        } else {
            return Err(ParseError::Other);
        };
        self.add_local(name)?;

        if let Some(Token::Comma) = self.peek() {
            self.next();
            self.parse_local()
        } else {
            Ok(())
        }
    }

    fn parse_do(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.parse_statements()?;
        self.expect(Token::End)?;
        self.level_down();
        Ok(())
    }

    /// Lower the nesting level by one, discarding any locals from that block.
    fn level_down(&mut self) {
        while let Some((_, lvl)) = self.locals.last() {
            if *lvl == self.nest_level {
                self.locals.pop();
            } else {
                break;
            }
        }
        self.nest_level -= 1;
    }

    fn parse_repeat(&mut self) -> Result<()> {
        self.nest_level += 1;
        let body_start = self.output.len() as isize;
        self.parse_statements()?;
        self.expect(Token::Until)?;
        self.parse_expr()?;
        let expr_end = self.output.len() as isize;
        self.push(Instr::BranchFalse(body_start - (expr_end + 1)));
        self.level_down();
        Ok(())
    }

    fn parse_while(&mut self) -> Result<()> {
        self.nest_level += 1;
        let condition_start = self.output.len() as isize;
        self.parse_expr()?;
        self.expect(Token::Do)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;
        old_output.push(Instr::BranchFalse(self.output.len() as isize + 1));
        old_output.append(&mut self.output);
        self.output = old_output;

        self.expect(Token::End)?;
        self.push(Instr::Jump(
            condition_start - (self.output.len() as isize + 1),
        ));
        self.level_down();

        Ok(())
    }

    fn parse_if(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.parse_expr()?;

        self.expect(Token::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;

        // If there's another arm, add 1 to the branch instruction to skip the closing Jump.
        let body_len = self.output.len() as isize
            + match self.peek() {
                Some(Token::Else) | Some(Token::ElseIf) => 1,
                // If there's no End token, handle it later.
                _ => 0,
            };
        old_output.push(Instr::BranchFalse(body_len));
        old_output.append(&mut self.output);
        self.output = old_output;

        self.parse_if_end()
    }

    /// Parse either:
    /// - An 'elseif' condition and body, and any subsequent arms.
    /// - An 'else' body.
    /// - An 'end' token.
    ///
    /// Then handle the logic of closing those.
    fn parse_if_end(&mut self) -> Result<()> {
        self.level_down();
        if let Some(Token::ElseIf) = self.peek() {
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.parse_elseif()?;
            self.close_else_or_elseif(old_output);
        } else if let Some(Token::Else) = self.peek() {
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.parse_else()?;
            self.close_else_or_elseif(old_output);
        } else {
            self.expect(Token::End)?;
        }
        Ok(())
    }

    fn close_else_or_elseif(&mut self, mut old_output: Vec<Instr>) {
        let jump_len = self.output.len() as isize;
        old_output.push(Instr::Jump(jump_len));
        old_output.append(&mut self.output);
        self.output = old_output;
    }

    fn parse_elseif(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.next();
        self.parse_expr()?;
        self.expect(Token::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;
        let body_len = self.output.len() as isize
            + match self.peek() {
                Some(Token::Else) | Some(Token::ElseIf) => 1,
                // If there's no End token, handle it later.
                _ => 0,
            };
        old_output.push(Instr::BranchFalse(body_len));
        old_output.append(&mut self.output);
        self.output = old_output;

        self.parse_if_end()
    }

    fn parse_else(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.next();
        self.parse_statements()?;
        self.expect(Token::End)
    }

    fn parse_assign(&mut self, name: String) -> Result<()> {
        let mut assignments = Vec::new();
        assignments.push(self.parse_lvalue(name)?);

        while let Some(Token::Comma) = self.peek() {
            self.next();
            let name = if let Some(Token::Identifier(name)) = self.next() {
                name
            } else {
                return Err(ParseError::Expect(Token::Identifier(String::new())));
            };
            assignments.push(self.parse_lvalue(name)?);
        }

        self.expect(Token::Assign)?;
        let num_rvalues = self.parse_explist()?;

        // If the number of Lvalues and Rvalues is not equal, we adjust using `nil`.
        let diff = assignments.len() as isize - num_rvalues as isize;
        if diff == 0 {
            // Lvalues and Rvalues match up, no adjustment needed.
            for instrs in assignments.iter_mut().rev() {
                self.output.append(instrs);
            }
        } else if diff > 0 {
            // There are more Lvalues than Rvalues. Append `nil` expressions
            // for the remaining assignments.
            for _ in 0..diff {
                self.push(Instr::PushNil);
            }
            for instrs in assignments.iter_mut().rev() {
                self.output.append(instrs);
            }
        } else {
            // There are more Lvalues than Rvalues. Append `Pop` instructions
            // to discard the extra expressions.
            for _ in diff..0 {
                self.push(Instr::Pop);
            }
            for instrs in assignments.iter_mut().rev() {
                self.output.append(instrs);
            }
        }

        Ok(())
    }

    /// Parse an lvalue and return the instructions needed to access it.
    fn parse_lvalue(&mut self, name: String) -> Result<Vec<Instr>> {
        // Returns a Vec because later, lvalues could be complicated expressions
        // involving table indexing.
        let mut output = Vec::new();
        let opt_local_idx = find_last_local(&self.locals, name.as_str());
        match opt_local_idx {
            Some(i) => output.push(Instr::SetLocal(i as u8)),
            None => {
                let i = self.find_or_add_string(name)?;
                output.push(Instr::SetGlobal(i));
            }
        }
        Ok(output)
    }

    fn parse_print(&mut self) -> Result<()> {
        self.parse_expr()?;
        self.push(Instr::Print);
        Ok(())
    }

    /// Parse a comma-separated list of expressions. Trailing and leading
    /// commas are not allowed.
    fn parse_explist(&mut self) -> Result<u8> {
        // An explist has to have at least one expression.
        self.parse_expr()?;
        let mut output = 1;
        while let Some(Token::Comma) = self.peek() {
            if output == u8::MAX {
                return Err(ParseError::Complexity);
            }
            self.next();
            self.parse_expr()?;
            output += 1;
        }

        Ok(output)
    }

    /// Parse the input as a single expression.
    fn parse_expr(&mut self) -> Result<()> {
        self.parse_or()
    }

    /// Attempt to parse an 'or' expression. Precedence 8.
    fn parse_or(&mut self) -> Result<()> {
        self.parse_and()?;
        while let Some(Token::Or) = self.peek() {
            self.next();
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.push(Instr::Pop);
            self.parse_and()?;
            let right_side_len = self.output.len();
            old_output.push(Instr::BranchTrueKeep(right_side_len as isize));
            old_output.append(&mut self.output);
            self.output = old_output;
        }

        Ok(())
    }

    /// Attempt to parse an 'and' expression. Precedence 7.
    fn parse_and(&mut self) -> Result<()> {
        self.parse_comparison()?;
        while let Some(Token::And) = self.peek() {
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
        }

        Ok(())
    }

    /// Parse a comparison expression. Precedence 6.
    ///
    /// `==`, `~=`, `<`, `<=`, `>`, `>=`
    fn parse_comparison(&mut self) -> Result<()> {
        self.parse_concat()?;
        loop {
            if let Some(Token::Less) = self.peek() {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Less);
            } else if let Some(Token::LessEqual) = self.peek() {
                self.next();
                self.parse_concat()?;
                self.push(Instr::LessEqual);
            } else if let Some(Token::Greater) = self.peek() {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Greater);
            } else if let Some(Token::GreaterEqual) = self.peek() {
                self.next();
                self.parse_concat()?;
                self.push(Instr::GreaterEqual);
            } else if let Some(Token::Equal) = self.peek() {
                self.next();
                self.parse_concat()?;
                self.push(Instr::Equal);
            } else if let Some(Token::NotEqual) = self.peek() {
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
        if let Some(Token::DotDot) = self.peek() {
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
            if let Some(Token::Plus) = self.peek() {
                self.next();
                self.parse_multiplication()?;
                self.push(Instr::Add);
            } else if let Some(Token::Minus) = self.peek() {
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
            if let Some(Token::Star) = self.peek() {
                self.next();
                self.parse_unary()?;
                self.push(Instr::Multiply);
            } else if let Some(Token::Slash) = self.peek() {
                self.next();
                self.parse_unary()?;
                self.push(Instr::Divide);
            } else if let Some(Token::Mod) = self.peek() {
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
        if let Some(Token::Not) = self.peek() {
            self.next();
            self.parse_unary()?;
            self.push(Instr::Not);
        } else if let Some(Token::Hash) = self.peek() {
            self.next();
            self.parse_unary()?;
            self.push(Instr::Length);
        } else if let Some(Token::Minus) = self.peek() {
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
        if let Some(Token::Caret) = self.peek() {
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
                self.parse_after_prefixexp()?;
            }
            Some(Token::Identifier(name)) => {
                self.parse_identifier(name)?;
                self.parse_after_prefixexp()?;
            }
            Some(Token::LiteralNumber(n)) => {
                let i = self.find_or_add_number(n)?;
                self.push(Instr::PushNum(i));
            }
            Some(Token::LiteralString(s)) => {
                let i = self.find_or_add_string(s)?;
                self.push(Instr::PushString(i));
            }
            Some(Token::Nil) => self.push(Instr::PushNil),
            Some(Token::False) => self.push(Instr::PushBool(false)),
            Some(Token::True) => self.push(Instr::PushBool(true)),

            Some(Token::DotDotDot) | Some(Token::Function) => {
                return Err(ParseError::Unsupported);
            }
            Some(t) => {
                return Err(ParseError::Unexpected(t));
            }
            None => {
                return Err(ParseError::UnexpectedEof);
            }
        }

        Ok(())
    }

    /// Parse the operations which can come after a prefix expression: a
    /// function call, a table index, or a method call.
    ///
    /// A prefixexp is a variable, functioncall, or parenthesized expression.
    fn parse_after_prefixexp(&mut self) -> Result<()> {
        match self.next() {
            Some(Token::LParen) => self.parse_call_exp(),
            Some(x) => {
                self.input.push(x);
                Ok(())
            }
            None => Ok(()),
        }
    }

    fn parse_call_exp(&mut self) -> Result<()> {
        let num_args = if let Some(Token::RParen) = self.peek() {
            0
        } else {
            self.parse_explist()?
        };
        self.expect(Token::RParen)?;
        self.push(Instr::Call(num_args));
        Ok(())
    }

    fn parse_identifier(&mut self, name: String) -> Result<()> {
        match find_last_local(&self.locals, name.as_str()) {
            Some(i) => {
                self.push(Instr::GetLocal(i as u8));
            }
            None => {
                let i = self.find_or_add_string(name)?;
                self.push(Instr::GetGlobal(i));
            }
        }

        Ok(())
    }

    /// Return the next Token.
    fn next(&mut self) -> Option<Token> {
        self.input.pop()
    }

    /// Peek at the next Token.
    fn peek(&mut self) -> Option<&Token> {
        self.input.last()
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

    /// Creates a new local slot at the current nest_level.
    /// Fail if we have exceeded the maximum number of locals.
    fn add_local(&mut self, name: String) -> Result<()> {
        if self.locals.len() == u8::MAX as usize {
            Err(ParseError::TooManyLocals)
        } else {
            self.locals.push((name, self.nest_level));
            if self.locals.len() > self.num_locals as usize {
                self.num_locals += 1;
            }
            Ok(())
        }
    }

    fn find_or_add_string(&mut self, name: String) -> Result<u8> {
        find_or_add(&mut self.string_literals, name).ok_or(ParseError::TooManyNumbers)
    }

    fn find_or_add_number(&mut self, num: f64) -> Result<u8> {
        find_or_add(&mut self.number_literals, num).ok_or(ParseError::TooManyNumbers)
    }
}

pub fn parse_chunk(tokens: Vec<Token>) -> result::Result<Chunk, ParseError> {
    let p = Parser::new(tokens);

    p.parse_chunk()
}

fn find_last_local(locals: &[(String, i32)], name: &str) -> Option<usize> {
    let mut i = locals.len();
    while i > 0 {
        i -= 1;
        if locals[i].0 == name {
            return Some(i);
        }
    }

    None
}

/// Returns the index of a number in the literals list, adding it if it does not exist.
fn find_or_add<T>(queue: &mut Vec<T>, x: T) -> Option<u8>
where
    T: PartialEq<T>,
{
    match queue.iter().position(|y| *y == x) {
        Some(i) => Some(i as u8),
        None => {
            let i = queue.len();
            if i == u8::MAX as usize {
                None
            } else {
                queue.push(x);
                Some(i as u8)
            }
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
        let input = vec![Token::Print, LiteralNumber(5.0), Plus, LiteralNumber(6.0)];
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Add, Instr::Print],
            number_literals: vec![5.0, 6.0],
            string_literals: vec![],
            num_locals: 0,
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
        ];
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Pow, Negate, Instr::Print],
            number_literals: vec![5.0, 2.0],
            string_literals: vec![],
            num_locals: 0,
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
            num_locals: 0,
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
            num_locals: 0,
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
        ];
        let output = Chunk {
            code: vec![PushNum(0), PushNum(1), Negate, Pow, Instr::Print],
            number_literals: vec![2.0, 3.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(input, output);
    }

    #[test]
    fn test6() {
        let input = vec![Token::Print, Token::Not, Token::Not, LiteralNumber(1.0)];
        let output = Chunk {
            code: vec![PushNum(0), Instr::Not, Instr::Not, Instr::Print],
            number_literals: vec![1.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(input, output);
    }

    #[test]
    fn test7() {
        let input = vec![
            Token::Identifier("a".to_string()),
            Assign,
            LiteralNumber(5.0),
        ];
        let output = Chunk {
            code: vec![PushNum(0), SetGlobal(0)],
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
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
            num_locals: 0,
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
            num_locals: 0,
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
            Assign,
            LiteralNumber(5.0),
            End,
        ];
        let code = vec![PushBool(true), BranchFalse(2), PushNum(0), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
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
            Assign,
            LiteralNumber(5.0),
            If,
            True,
            Then,
            Identifier("b".to_string()),
            Assign,
            LiteralNumber(4.0),
            End,
            End,
        ];
        let code = vec![
            PushBool(true),
            BranchFalse(6),
            PushNum(0),
            SetGlobal(0),
            PushBool(true),
            BranchFalse(2),
            PushNum(1),
            SetGlobal(1),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test12() {
        let input = vec![
            If,
            True,
            Then,
            Identifier("a".to_string()),
            Assign,
            LiteralNumber(5.0),
            Else,
            Identifier("a".to_string()),
            Assign,
            LiteralNumber(4.0),
            End,
        ];
        let code = vec![
            PushBool(true),
            BranchFalse(3),
            PushNum(0),
            SetGlobal(0),
            Jump(2),
            PushNum(1),
            SetGlobal(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test13() {
        let input = vec![
            If,
            True,
            Then,
            Identifier("a".to_string()),
            Assign,
            LiteralNumber(5.0),
            ElseIf,
            LiteralNumber(6.0),
            Token::Equal,
            LiteralNumber(7.0),
            Then,
            Identifier("a".to_string()),
            Assign,
            LiteralNumber(3.0),
            Else,
            Identifier("a".to_string()),
            Assign,
            LiteralNumber(4.0),
            End,
        ];
        let code = vec![
            PushBool(true),
            BranchFalse(3),
            PushNum(0),
            SetGlobal(0),
            Jump(9),
            PushNum(1),
            PushNum(2),
            Instr::Equal,
            BranchFalse(3),
            PushNum(3),
            SetGlobal(0),
            Jump(2),
            PushNum(4),
            SetGlobal(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 6.0, 7.0, 3.0, 4.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test14() {
        let input = vec![
            While,
            Identifier("a".to_string()),
            Token::Less,
            LiteralNumber(10.0),
            Token::Do,
            Identifier("a".to_string()),
            Assign,
            Identifier("a".to_string()),
            Plus,
            LiteralNumber(1.0),
            End,
        ];
        let code = vec![
            GetGlobal(0),
            PushNum(0),
            Instr::Less,
            BranchFalse(5),
            GetGlobal(0),
            PushNum(1),
            Add,
            SetGlobal(0),
            Jump(-9),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![10.0, 1.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test15() {
        let input = vec![
            Repeat,
            Token::Print,
            LiteralNumber(5.0),
            Until,
            Identifier("a".to_string()),
            Token::Equal,
            Identifier("b".to_string()),
            Token::Print,
            LiteralNumber(4.0),
        ];
        let code = vec![
            PushNum(0),
            Instr::Print,
            GetGlobal(0),
            GetGlobal(1),
            Instr::Equal,
            BranchFalse(-6),
            PushNum(1),
            Instr::Print,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test16() {
        let input = vec![
            Local,
            Identifier("i".to_string()),
            Identifier("i".to_string()),
            Assign,
            LiteralNumber(2.0),
        ];
        let code = vec![PushNum(0), SetLocal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            string_literals: vec![],
            num_locals: 1,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test17() {
        let input = vec![
            Local,
            Identifier("i".to_string()),
            Comma,
            Identifier("j".to_string()),
            Token::Print,
            Identifier("j".to_string()),
        ];
        let code = vec![GetLocal(1), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec![],
            num_locals: 2,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test18() {
        let input = vec![
            Local,
            Identifier("i".to_string()),
            Do,
            Local,
            Identifier("i".to_string()),
            Token::Print,
            Identifier("i".to_string()),
            End,
            Token::Print,
            Identifier("i".to_string()),
        ];
        let code = vec![GetLocal(1), Instr::Print, GetLocal(0), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec![],
            num_locals: 2,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test19() {
        let input = vec![
            Do,
            Local,
            Identifier("i".to_string()),
            Token::Print,
            Identifier("i".to_string()),
            End,
            Token::Print,
            Identifier("i".to_string()),
        ];
        let code = vec![GetLocal(0), Instr::Print, GetGlobal(0), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec!["i".to_string()],
            num_locals: 1,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test20() {
        let input = vec![
            Local,
            Identifier("i".to_string()),
            If,
            False,
            Then,
            Local,
            Identifier("i".to_string()),
            Else,
            Token::Print,
            Identifier("i".to_string()),
            End,
        ];
        let code = vec![
            PushBool(false),
            BranchFalse(1),
            Jump(2),
            GetLocal(0),
            Instr::Print,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec![],
            num_locals: 2,
        };
        check_it(input, chunk);
    }

    // for i = 1,5 do print i end
    #[test]
    fn test21() {
        let input = vec![
            For,
            Identifier("i".to_string()),
            Token::Assign,
            LiteralNumber(1.0),
            Comma,
            LiteralNumber(5.0),
            Do,
            Token::Print,
            Identifier("i".to_string()),
            End,
        ];
        let code = vec![
            PushNum(0),
            PushNum(1),
            PushNum(0),
            ForPrep(0),
            GetLocal(3),
            Instr::Print,
            ForLoop(0, -3),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 5.0],
            string_literals: vec![],
            num_locals: 4,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test22() {
        let input = vec![
            Identifier("a".to_string()),
            Comma,
            Identifier("b".to_string()),
            Token::Assign,
            LiteralNumber(1.0),
        ];
        let code = vec![PushNum(0), PushNil, SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test23() {
        let input = vec![
            Identifier("a".to_string()),
            Comma,
            Identifier("b".to_string()),
            Token::Assign,
            LiteralNumber(1.0),
            Comma,
            LiteralNumber(2.0),
        ];
        let code = vec![PushNum(0), PushNum(1), SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test24() {
        let input = vec![
            Identifier("a".to_string()),
            Comma,
            Identifier("b".to_string()),
            Token::Assign,
            LiteralNumber(1.0),
            Comma,
            LiteralNumber(2.0),
            Comma,
            LiteralNumber(3.0),
        ];
        let code = vec![
            PushNum(0),
            PushNum(1),
            PushNum(2),
            Pop,
            SetGlobal(1),
            SetGlobal(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0, 3.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test25() {
        let input = vec![Identifier("puts".to_string()), LParen, RParen];
        let code = vec![GetGlobal(0), Call(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec!["puts".to_string()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }
}
