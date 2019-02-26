use std::mem::swap;
use std::result;
use std::str;
use std::u8;

use crate::instr::Instr;
use crate::lexer::TokenList;
use crate::token::{Token, TokenType};

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<Vec<u8>>,
    pub num_locals: u8,
}

#[derive(Debug)]
pub enum ParseError {
    Unsupported,
    Unexpected(Token),
    Expect(TokenType),
    UnexpectedEof,
    TooManyNumbers,
    TooManyStrings,
    TooManyLocals,
    Complexity,
}

pub fn parse_chunk(token_list: TokenList) -> Result<Chunk> {
    let p = Parser::new(token_list);

    p.parse_chunk()
}

type Result<T> = result::Result<T, ParseError>;

/// Tracks the current state, to make parsing easier.
#[derive(Debug, Default)]
struct Parser<'a> {
    /// The input token stream.
    input: Vec<Token>,
    /// The bytecode for the resulting Chunk.
    output: Vec<Instr>,
    /// The raw source code
    text: &'a [u8],
    string_literals: Vec<Vec<u8>>,
    number_literals: Vec<f64>,
    nest_level: i32,
    locals: Vec<(Vec<u8>, i32)>,
    /// The amount of local slots the resulting Chunk will have.
    num_locals: u8,
}

impl<'a> Parser<'a> {
    /// Basic constructor
    fn new(token_list: TokenList<'a>) -> Self {
        let input = token_list.tokens.into_iter().rev().collect();
        Parser {
            input,
            text: token_list.text,
            output: Vec::new(),
            string_literals: Vec::new(),
            number_literals: Vec::new(),
            nest_level: 0,
            locals: Vec::new(),
            num_locals: 0,
        }
    }

    // Helper functions

    /// Adds an instruction to the output.
    fn push(&mut self, instr: Instr) {
        self.output.push(instr);
    }

    /// Return the next Token.
    fn next(&mut self) -> Option<Token> {
        self.input.pop()
    }

    /// Peek at the next Token.
    fn peek(&mut self) -> Option<&Token> {
        self.input.last()
    }

    fn peek_type(&mut self, expected_typ: TokenType) -> bool {
        self.peek().map(|t| t.typ == expected_typ).unwrap_or(false)
    }

    /// Pulls a token off the input and checks it against `tok`. If it doesn't
    /// match, it returns an `Err`.
    fn expect(&mut self, expected: TokenType) -> Result<()> {
        match self.next() {
            Some(t) => {
                if t.typ == expected {
                    Ok(())
                } else {
                    Err(ParseError::Expect(expected))
                }
            }
            _ => Err(ParseError::Expect(expected)),
        }
    }

    /// Checks the next token's type. If it matches `typ`, it is popped off and
    /// we return `true`. Else, we do nothing and return `false`.
    fn try_pop(&mut self, expected_type: TokenType) -> Option<Token> {
        match self.peek() {
            Some(t) if t.typ == expected_type => self.next(),
            _ => None,
        }
    }

    fn has_next(&mut self, expected_type: TokenType) -> bool {
        self.try_pop(expected_type).is_some()
    }

    /// Expect an identifier token and get the actual identifier from the text.
    fn expect_identifier(&mut self) -> Result<Vec<u8>> {
        match self.try_pop(TokenType::Identifier) {
            Some(token) => {
                let Token { start, len, .. } = token;
                let name = self.text[start..(start + len as usize)].to_vec();
                Ok(name)
            }
            None => Err(ParseError::Expect(TokenType::Identifier)),
        }
    }

    /// Converts a string's offsets into an &[u8].
    fn get_string_from_text(&self, start: usize, len: u32) -> Vec<u8> {
        self.text[(start + 1)..(start + len as usize - 1)].to_vec()
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
            use TokenType::*;
            match token.typ {
                Identifier => {
                    let s = self.text[token.start..(token.start + token.len as usize)].to_vec();
                    self.parse_ident_stmt(&s)?
                }
                If => self.parse_if()?,
                Print => self.parse_print()?,
                While => self.parse_while()?,
                Repeat => self.parse_repeat()?,
                Do => self.parse_do()?,
                Local => self.parse_locals()?,
                For => self.parse_for()?,
                _ => {
                    // Put the input token back if it doesn't start a statement
                    self.input.push(token);
                    break;
                }
            }
            self.try_pop(TokenType::Semi);
        }

        Ok(())
    }

    fn parse_assign(&mut self, name: &[u8]) -> Result<()> {
        let mut assignments = Vec::new();
        assignments.push(self.parse_lvalue(name)?);

        while self.has_next(TokenType::Comma) {
            let name = self.expect_identifier()?;
            assignments.push(self.parse_lvalue(name.as_slice())?);
        }

        self.expect(TokenType::Assign)?;
        let num_rvalues = self.parse_explist()?;

        // If the number of Lvalues and Rvalues is not equal, we adjust using `nil`.
        let diff = assignments.len() as isize - num_rvalues as isize;
        if diff > 0 {
            // There are more Lvalues than Rvalues. Append `nil` expressions
            // for the remaining assignments.
            for _ in 0..diff {
                self.push(Instr::PushNil);
            }
        } else {
            // There are more Lvalues than Rvalues. Append `Pop` instructions
            // to discard the extra expressions.
            for _ in diff..0 {
                self.push(Instr::Pop);
            }
        }

        // Rvalues are evaluated left-to-right, but assignment happens right-to-left.
        // This is only noticeable with `__newindex` metamethods.
        for instrs in assignments.iter_mut().rev() {
            self.output.append(instrs);
        }

        Ok(())
    }

    fn parse_locals(&mut self) -> Result<()> {
        let start = self.locals.len() as u8;

        let name1 = self.expect_identifier()?;
        self.add_local(name1.as_slice())?;
        let mut num_names = 1;

        while self.has_next(TokenType::Comma) {
            let name = self.expect_identifier()?;
            self.add_local(name.as_slice())?;
            num_names += 1;
        }

        if self.has_next(TokenType::Assign) {
            let num_rvalues = self.parse_explist()? as isize;
            let diff = num_names - num_rvalues;
            if diff < 0 {
                for _ in diff..0 {
                    self.push(Instr::Pop);
                }
            } else if diff > 0 {
                for _ in 0..diff {
                    self.push(Instr::PushNil);
                }
            }
        } else {
            for _ in 0..num_names {
                self.push(Instr::PushNil);
            }
        }

        let stop = start + num_names as u8;
        for i in (start..stop).rev() {
            self.push(Instr::SetLocal(i))
        }

        Ok(())
    }

    /// Parse an lvalue and return the instructions needed to access it.
    fn parse_lvalue(&mut self, name: &[u8]) -> Result<Vec<Instr>> {
        // Returns a Vec because later, lvalues could be complicated expressions
        // involving table indexing.
        let mut output = Vec::new();
        let opt_local_idx = find_last_local(&self.locals, name);
        match opt_local_idx {
            Some(i) => output.push(Instr::SetLocal(i as u8)),
            None => {
                let i = self.find_or_add_string(name)?;
                output.push(Instr::SetGlobal(i));
            }
        }
        Ok(output)
    }

    /// Parse a statement which starts with an identifier. It could be an
    /// assignment or a lone function call.
    fn parse_ident_stmt(&mut self, name: &[u8]) -> Result<()> {
        match self.peek() {
            Some(t) => match t.typ {
                TokenType::Comma | TokenType::Assign => self.parse_assign(name),
                TokenType::LParen => self.parse_call_stmt(name),
                _ => Err(ParseError::Expect(TokenType::Assign)),
            },
            _ => Err(ParseError::Expect(TokenType::Assign)),
        }
    }

    fn parse_call_stmt(&mut self, name: &[u8]) -> Result<()> {
        self.parse_identifier(name)?;
        // Consume opening paren.
        self.next();
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<()> {
        let num_args = if let Some(Token {
            typ: TokenType::RParen,
            ..
        }) = self.peek()
        {
            0
        } else {
            self.parse_explist()?
        };
        self.push(Instr::Call(num_args));
        self.expect(TokenType::RParen)?;

        Ok(())
    }

    /// Parse a for loop, before we know whether it's generic (`for i in t do`) or
    /// numeric (`for i = 1,5 do`).
    fn parse_for(&mut self) -> Result<()> {
        let name = self.expect_identifier()?;
        self.nest_level += 1;
        self.expect(TokenType::Assign)?;
        self.parse_numeric_for(name.as_slice())?;
        self.level_down();

        Ok(())
    }

    /// Parse a numeric for, starting with the first expression after the `=`.
    fn parse_numeric_for(&mut self, name: &[u8]) -> Result<()> {
        // The start(current), stop and step are stored in three "hidden" local slots.
        let current_index_slot = self.locals.len();
        self.add_local(b"")?;
        self.add_local(b"")?;
        self.add_local(b"")?;

        // The actual local is in a fourth slot, so that it can be reassigned to.
        self.add_local(name)?;

        // First, all 3 control expressions are evaluated.
        self.parse_expr()?;
        self.expect(TokenType::Comma)?;
        self.parse_expr()?;

        // optional step value
        if self.has_next(TokenType::Comma) {
            self.parse_expr()?;
            self.expect(TokenType::Do)?;
        } else if self.has_next(TokenType::Do) {
            let i = self.find_or_add_number(1.0)?;
            self.push(Instr::PushNum(i));
        } else {
            return Err(ParseError::Expect(TokenType::Comma));
        }

        // The ForPrep command pulls three values off the stack and places them
        // into locals to use in the loop.
        self.push(Instr::ForPrep(current_index_slot));

        // body
        let old_len = self.output.len() as isize;
        self.parse_statements()?;
        self.expect(TokenType::End)?;
        self.push(Instr::ForLoop(
            current_index_slot,
            old_len - self.output.len() as isize - 1,
        ));

        Ok(())
    }

    fn parse_do(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.parse_statements()?;
        self.expect(TokenType::End)?;
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
        self.expect(TokenType::Until)?;
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
        self.expect(TokenType::Do)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;
        old_output.push(Instr::BranchFalse(self.output.len() as isize + 1));
        old_output.append(&mut self.output);
        self.output = old_output;

        self.expect(TokenType::End)?;
        self.push(Instr::Jump(
            condition_start - (self.output.len() as isize + 1),
        ));
        self.level_down();

        Ok(())
    }

    fn parse_if(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.parse_expr()?;

        self.expect(TokenType::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;

        // If there's another arm, add 1 to the branch instruction to skip the closing Jump.
        // If there's no End token, handle it later.
        let body_len = self.output.len() as isize
            + if self.peek_type(TokenType::Else) || self.peek_type(TokenType::ElseIf) {
                1
            } else {
                0
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
        if self.peek_type(TokenType::ElseIf) {
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.parse_elseif()?;
            self.close_else_or_elseif(old_output);
        } else if self.peek_type(TokenType::Else) {
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.parse_else()?;
            self.close_else_or_elseif(old_output);
        } else {
            self.expect(TokenType::End)?;
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
        self.expect(TokenType::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;
        let body_len = self.output.len() as isize
            + if self.peek_type(TokenType::Else) || self.peek_type(TokenType::ElseIf) {
                1
            } else {
                0
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
        self.expect(TokenType::End)?;
        Ok(())
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
        while self.has_next(TokenType::Comma) {
            if output == u8::MAX {
                return Err(ParseError::Complexity);
            }
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
        while self.has_next(TokenType::Or) {
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
        while self.has_next(TokenType::And) {
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

        while let Some(t) = self.next() {
            let i = match t.typ {
                TokenType::Less => Instr::Less,
                TokenType::LessEqual => Instr::LessEqual,
                TokenType::Greater => Instr::Greater,
                TokenType::GreaterEqual => Instr::GreaterEqual,
                TokenType::Equal => Instr::Equal,
                TokenType::NotEqual => Instr::NotEqual,
                _ => {
                    self.input.push(t);
                    break;
                }
            };
            self.parse_concat()?;
            self.push(i);
        }

        Ok(())
    }

    /// Parse a string concatenation expression. Precedence 5.
    ///
    /// `..`
    fn parse_concat(&mut self) -> Result<()> {
        self.parse_addition()?;
        if self.has_next(TokenType::DotDot) {
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
        while let Some(tok) = self.next() {
            let i = match tok.typ {
                TokenType::Plus => Instr::Add,
                TokenType::Minus => Instr::Subtract,
                _ => {
                    self.input.push(tok);
                    break;
                }
            };
            self.parse_multiplication()?;
            self.push(i);
        }

        Ok(())
    }

    /// Parse a multiplication expression. Precedence 3.
    ///
    /// `*`, `/`, `%`
    fn parse_multiplication(&mut self) -> Result<()> {
        self.parse_unary()?;
        while let Some(tok) = self.next() {
            let i = match tok.typ {
                TokenType::Star => Instr::Multiply,
                TokenType::Slash => Instr::Divide,
                TokenType::Mod => Instr::Mod,
                _ => {
                    self.input.push(tok);
                    break;
                }
            };
            self.parse_unary()?;
            self.push(i);
        }

        Ok(())
    }

    /// Parse a unary expression. Precedence 2. Note the `^` operator has a
    /// higher precedence than unary operators.
    ///
    /// `not`, `#`, `-`
    fn parse_unary(&mut self) -> Result<()> {
        if let Some(tok) = self.next() {
            let i = match tok.typ {
                TokenType::Not => Instr::Not,
                TokenType::Hash => Instr::Length,
                TokenType::Minus => Instr::Negate,
                _ => {
                    self.input.push(tok);
                    return self.parse_pow();
                }
            };
            self.parse_unary()?;
            self.push(i);
        }

        Ok(())
    }

    /// Parse an exponentiation expression. Right-associative, Precedence 1.
    ///
    /// `^`
    fn parse_pow(&mut self) -> Result<()> {
        self.parse_primary()?;
        if self.has_next(TokenType::Caret) {
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
    /// * A table constructor
    fn parse_primary(&mut self) -> Result<()> {
        if let Some(tok) = self.next() {
            match tok.typ {
                TokenType::LCurly => self.parse_table()?,
                TokenType::LParen => {
                    self.parse_expr()?;
                    self.expect(TokenType::RParen)?;
                    self.parse_after_prefixexp()?;
                }
                TokenType::Identifier => {
                    let name = &self.text[tok.start..(tok.start + tok.len as usize)];
                    self.parse_identifier(name)?;
                    self.parse_after_prefixexp()?;
                }
                TokenType::LiteralNumber => {
                    let bytes = &self.text[tok.start..(tok.start + tok.len as usize)];
                    let s = str::from_utf8(bytes).unwrap();
                    let n = s.parse::<f64>().unwrap();
                    let i = self.find_or_add_number(n)?;
                    self.push(Instr::PushNum(i));
                }
                TokenType::LiteralString => {
                    let bytes = self.get_string_from_text(tok.start, tok.len);
                    let i = self.find_or_add_string(bytes.as_slice())?;
                    self.push(Instr::PushString(i));
                }
                TokenType::Nil => self.push(Instr::PushNil),
                TokenType::False => self.push(Instr::PushBool(false)),
                TokenType::True => self.push(Instr::PushBool(true)),
                TokenType::DotDotDot | TokenType::Function => {
                    return Err(ParseError::Unsupported);
                }
                _ => {
                    Err::<(), ParseError>(ParseError::Unexpected(tok))?;
                }
            }
            Ok(())
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn parse_table(&mut self) -> Result<()> {
        self.push(Instr::NewTable);
        if !self.has_next(TokenType::RCurly) {
            self.parse_table_entry()?;
            while is_fieldsep(self.peek()) {
                self.next();
                if self.peek_type(TokenType::RCurly) {
                    break;
                } else {
                    self.parse_table_entry()?;
                }
            }
            self.expect(TokenType::RCurly)?;
        }
        Ok(())
    }

    /// Parse a potential table entry
    fn parse_table_entry(&mut self) -> Result<()> {
        if let Some(tok) = self.next() {
            match tok.typ {
                TokenType::Identifier => {
                    let s = &self.text[tok.start..(tok.start + tok.len as usize)];
                    let index = self.find_or_add_string(&s)?;
                    self.expect(TokenType::Assign)?;
                    self.parse_expr()?;
                    self.push(Instr::SetField(index));
                }
                TokenType::LSquare => panic!("Unsupported"),
                _ => panic!("Also unsupported"),
            }
        }
        Ok(())
    }

    /// Parse the operations which can come after a prefix expression: a
    /// function call, a table index, or a method call.
    ///
    /// A prefixexp is a variable, functioncall, or parenthesized expression.
    fn parse_after_prefixexp(&mut self) -> Result<()> {
        if self.has_next(TokenType::LParen) {
            self.parse_call_exp()
        } else if self.has_next(TokenType::Dot) {
            self.parse_field()
        } else {
            Ok(())
        }
    }

    /// Parse a field access, which is
    fn parse_field(&mut self) -> Result<()> {
        let name = self.expect_identifier()?;
        let i = self.find_or_add_string(name.as_slice())?;
        self.push(Instr::GetField(i));
        Ok(())
    }

    fn parse_call_exp(&mut self) -> Result<()> {
        let num_args = if self.peek_type(TokenType::RParen) {
            0
        } else {
            self.parse_explist()?
        };
        self.expect(TokenType::RParen)?;
        self.push(Instr::Call(num_args));
        Ok(())
    }

    fn parse_identifier(&mut self, name: &[u8]) -> Result<()> {
        match find_last_local(&self.locals, name) {
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

    /// Creates a new local slot at the current nest_level.
    /// Fail if we have exceeded the maximum number of locals.
    fn add_local(&mut self, name: &[u8]) -> Result<()> {
        if self.locals.len() == u8::MAX as usize {
            Err(ParseError::TooManyLocals)
        } else {
            self.locals.push((Vec::from(name), self.nest_level));
            if self.locals.len() > self.num_locals as usize {
                self.num_locals += 1;
            }
            Ok(())
        }
    }

    fn find_or_add_string(&mut self, name: &[u8]) -> Result<u8> {
        find_or_add(&mut self.string_literals, Vec::from(name)).ok_or(ParseError::TooManyStrings)
    }

    fn find_or_add_number(&mut self, num: f64) -> Result<u8> {
        find_or_add(&mut self.number_literals, num).ok_or(ParseError::TooManyNumbers)
    }
}

fn find_last_local(locals: &[(Vec<u8>, i32)], name: &[u8]) -> Option<usize> {
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
    T: PartialEq,
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

fn is_fieldsep(token: Option<&Token>) -> bool {
    match token {
        Some(t) => match t.typ {
            TokenType::Comma | TokenType::Semi => true,
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::Instr::{self, *};
    use crate::lexer;
    use crate::token::{
        Token,
        TokenType::{self, *},
    };

    const TOK: fn(TokenType, usize, u32) -> Token = Token::new;

    fn check_it(input: TokenList, output: Chunk) {
        assert_eq!(parse_chunk(input).unwrap(), output);
    }

    #[test]
    fn test1() {
        let text = b"print 5 + 6";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(Plus, 8, 1),
            TOK(LiteralNumber, 10, 1),
        ];
        let tl = TokenList { text, tokens };
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Add, Instr::Print],
            number_literals: vec![5.0, 6.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(tl, out);
    }

    #[test]
    fn test2() {
        let text = b"print -5^2";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(Minus, 6, 1),
            TOK(LiteralNumber, 7, 1),
            TOK(Caret, 8, 1),
            TOK(LiteralNumber, 9, 1),
        ];
        let tl = TokenList { text, tokens };
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Pow, Negate, Instr::Print],
            number_literals: vec![5.0, 2.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(tl, out);
    }

    #[test]
    fn test3() {
        let text = b"print 5 + true .. 'hi'";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(Plus, 8, 1),
            TOK(True, 10, 4),
            TOK(DotDot, 15, 2),
            TOK(LiteralString, 18, 4),
        ];
        let tl = TokenList { text, tokens };
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
            string_literals: vec![b"hi".to_vec()],
            num_locals: 0,
        };
        check_it(tl, out);
    }

    #[test]
    fn test4() {
        let text = b"print 1 .. 2 + 3";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(DotDot, 8, 2),
            TOK(LiteralNumber, 11, 1),
            TOK(Plus, 13, 1),
            TOK(LiteralNumber, 15, 1),
        ];
        let tl = TokenList { text, tokens };
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
        check_it(tl, output);
    }

    #[test]
    fn test5() {
        let text = b"print 2^-3";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(Caret, 7, 1),
            TOK(Minus, 8, 1),
            TOK(LiteralNumber, 9, 1),
        ];
        let tl = TokenList { text, tokens };
        let output = Chunk {
            code: vec![PushNum(0), PushNum(1), Negate, Pow, Instr::Print],
            number_literals: vec![2.0, 3.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(tl, output);
    }

    #[test]
    fn test6() {
        let text = b"print not not 1";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(TokenType::Not, 6, 3),
            TOK(TokenType::Not, 10, 3),
            TOK(LiteralNumber, 14, 1),
        ];
        let tl = TokenList { text, tokens };
        let output = Chunk {
            code: vec![PushNum(0), Instr::Not, Instr::Not, Instr::Print],
            number_literals: vec![1.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(tl, output);
    }

    #[test]
    fn test7() {
        let text = b"a = 5";
        let tokens = vec![
            TOK(Identifier, 0, 1),
            TOK(TokenType::Assign, 2, 1),
            TOK(LiteralNumber, 4, 1),
        ];
        let tl = TokenList { text, tokens };
        let output = Chunk {
            code: vec![PushNum(0), SetGlobal(0)],
            number_literals: vec![5.0],
            string_literals: vec![b"a".to_vec()],
            num_locals: 0,
        };
        check_it(tl, output);
    }

    #[test]
    fn test8() {
        let text = b"print true and false";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(True, 6, 4),
            TOK(And, 11, 3),
            TOK(False, 15, 5),
        ];
        let tl = TokenList { text, tokens };
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
        check_it(tl, output);
    }

    #[test]
    fn test9() {
        let text = b"print 5 or nil and true";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LiteralNumber, 6, 1),
            TOK(Or, 8, 2),
            TOK(Nil, 11, 3),
            TOK(And, 15, 3),
            TOK(True, 19, 4),
        ];
        let tl = TokenList { text, tokens };
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
        check_it(tl, output);
    }

    #[test]
    fn test10() {
        let text = b"if true then a = 5 end";
        let tokens = vec![
            TOK(If, 0, 2),
            TOK(True, 3, 4),
            TOK(Then, 8, 4),
            TOK(Identifier, 13, 1),
            TOK(TokenType::Assign, 15, 1),
            TOK(LiteralNumber, 17, 1),
            TOK(End, 19, 3),
        ];
        let tl = TokenList { text, tokens };
        let code = vec![PushBool(true), BranchFalse(2), PushNum(0), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec![b"a".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test11() {
        let text = b"if true then a = 5 if true then b = 4 end end";
        let tokens = vec![
            TOK(If, 0, 2),
            TOK(True, 3, 4),
            TOK(Then, 8, 4),
            TOK(Identifier, 13, 1),
            TOK(Assign, 15, 1),
            TOK(LiteralNumber, 17, 1),
            TOK(If, 19, 2),
            TOK(True, 22, 4),
            TOK(Then, 27, 4),
            TOK(Identifier, 32, 1),
            TOK(Assign, 34, 1),
            TOK(LiteralNumber, 36, 1),
            TOK(End, 38, 3),
            TOK(End, 42, 3),
        ];
        let tl = TokenList { text, tokens };
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
            string_literals: vec![b"a".to_vec(), b"b".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test12() {
        let text = b"if true then a = 5 else a = 4 end";
        let tokens = vec![
            TOK(If, 0, 2),
            TOK(True, 3, 4),
            TOK(Then, 8, 4),
            TOK(Identifier, 13, 1),
            TOK(Assign, 15, 1),
            TOK(LiteralNumber, 17, 1),
            TOK(Else, 19, 4),
            TOK(Identifier, 24, 1),
            TOK(Assign, 26, 1),
            TOK(LiteralNumber, 28, 1),
            TOK(End, 30, 3),
        ];
        let tl = TokenList { text, tokens };
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
            string_literals: vec![b"a".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test13() {
        let text = b"if true then a = 5 elseif 6 == 7 then a = 3 else a = 4 end";
        let input = lexer::lex(text).unwrap();
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
            string_literals: vec![b"a".to_vec()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test14() {
        let text = b"while a < 10 do a = a + 1 end";
        let input = lexer::lex(text).unwrap();
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
            string_literals: vec![b"a".to_vec()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test15() {
        let text = b"repeat print 5 until a == b print 4";
        let input = lexer::lex(text).unwrap();
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
            string_literals: vec![b"a".to_vec(), b"b".to_vec()],
            num_locals: 0,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test16() {
        let text = b"local i i = 2";
        let input = lexer::lex(text).unwrap();
        let code = vec![PushNil, SetLocal(0), PushNum(0), SetLocal(0)];
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
        let text = b"local i, j print j";
        let input = lexer::lex(text).unwrap();
        let code = vec![
            PushNil,
            PushNil,
            SetLocal(1),
            SetLocal(0),
            GetLocal(1),
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

    #[test]
    fn test18() {
        let text = b"local i do local i print i end print i";
        let input = lexer::lex(text).unwrap();
        let code = vec![
            PushNil,
            SetLocal(0),
            PushNil,
            SetLocal(1),
            GetLocal(1),
            Instr::Print,
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

    #[test]
    fn test19() {
        let text = b"do local i print i end print i";
        let input = lexer::lex(text).unwrap();
        let code = vec![
            PushNil,
            SetLocal(0),
            GetLocal(0),
            Instr::Print,
            GetGlobal(0),
            Instr::Print,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec![b"i".to_vec()],
            num_locals: 1,
        };
        check_it(input, chunk);
    }

    #[test]
    fn test20() {
        let text = b"local i if false then local i else print i end";
        let input = lexer::lex(text).unwrap();
        let code = vec![
            PushNil,
            SetLocal(0),
            PushBool(false),
            BranchFalse(3),
            PushNil,
            SetLocal(1),
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
        let text = b"for i = 1,5 do print i end";
        let input = lexer::lex(text).unwrap();
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
        let text = b"a, b = 1";
        let tokens = vec![
            TOK(Identifier, 0, 1),
            TOK(Comma, 1, 1),
            TOK(Identifier, 3, 1),
            TOK(TokenType::Assign, 5, 1),
            TOK(LiteralNumber, 7, 1),
        ];
        let tl = TokenList { text, tokens };
        let code = vec![PushNum(0), PushNil, SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0],
            string_literals: vec![b"a".to_vec(), b"b".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test23() {
        let text = b"a, b = 1, 2";
        let tokens = vec![
            TOK(Identifier, 0, 1),
            TOK(Comma, 1, 1),
            TOK(Identifier, 3, 1),
            TOK(TokenType::Assign, 5, 1),
            TOK(LiteralNumber, 7, 1),
            TOK(Comma, 8, 1),
            TOK(LiteralNumber, 10, 1),
        ];
        let tl = TokenList { text, tokens };
        let code = vec![PushNum(0), PushNum(1), SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0],
            string_literals: vec![b"a".to_vec(), b"b".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test24() {
        let text = b"a, b = 1, 2, 3";
        let tokens = vec![
            TOK(Identifier, 0, 1),
            TOK(Comma, 1, 1),
            TOK(Identifier, 3, 1),
            TOK(TokenType::Assign, 5, 1),
            TOK(LiteralNumber, 7, 1),
            TOK(Comma, 8, 1),
            TOK(LiteralNumber, 10, 1),
            TOK(Comma, 11, 1),
            TOK(LiteralNumber, 13, 1),
        ];
        let tl = TokenList { text, tokens };
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
            string_literals: vec![b"a".to_vec(), b"b".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test25() {
        let text = b"puts()";
        let tokens = vec![TOK(Identifier, 0, 4), TOK(LParen, 4, 1), TOK(RParen, 5, 1)];
        let tl = TokenList { text, tokens };
        let code = vec![GetGlobal(0), Call(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec![b"puts".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }

    #[test]
    fn test_table_constructor1() {
        let text = b"print {x = 5,}";
        let tokens = vec![
            TOK(TokenType::Print, 0, 5),
            TOK(LCurly, 6, 1),
            TOK(Identifier, 7, 1),
            TOK(TokenType::Assign, 9, 1),
            TOK(LiteralNumber, 11, 1),
            TOK(Comma, 12, 1),
            TOK(RCurly, 13, 1),
        ];
        let tl = TokenList { text, tokens };
        let code = vec![NewTable, PushNum(0), SetField(0), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec![b"x".to_vec()],
            num_locals: 0,
        };
        check_it(tl, chunk);
    }
}
