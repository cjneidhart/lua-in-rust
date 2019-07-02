use std::mem::swap;
use std::str;
use std::u8;

use crate::lexer::TokenStream;
use crate::Error;
use crate::ErrorKind;
use crate::Instr;
use crate::Result;
use crate::Token;
use crate::TokenType;

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<String>,
    pub num_locals: u8,
}

pub fn parse_str(source: &str) -> Result<Chunk> {
    let parser = Parser::new(source);
    parser.parse_chunk()
}

/// Tracks the current state, to make parsing easier.
#[derive(Debug)]
struct Parser<'a> {
    /// The input token stream.
    input: TokenStream<'a>,
    /// The bytecode for the resulting Chunk.
    output: Vec<Instr>,
    /// The raw source code
    text: &'a str,
    string_literals: Vec<String>,
    number_literals: Vec<f64>,
    nest_level: i32,
    locals: Vec<(String, i32)>,
    /// The amount of local slots the resulting Chunk will have.
    num_locals: u8,
}

impl<'a> Parser<'a> {
    /// Basic constructor
    fn new(source: &'a str) -> Self {
        let tokens = TokenStream::new(source);
        Parser::from_token_stream(source, tokens)
    }

    fn from_token_stream(source: &'a str, tokens: TokenStream<'a>) -> Self {
        Parser {
            input: tokens,
            text: source,
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

    /// Pulls a token off the input and checks it against `tok`. If it doesn't
    /// match, it returns an `Err`.
    fn expect(&mut self, expected: TokenType) -> Result<()> {
        match self.input.next()? {
            Some(t) => {
                if t.typ == expected {
                    Ok(())
                } else {
                    Err(self.error(ErrorKind::UnexpectedTok))
                }
            }
            _ => Err(self.error(ErrorKind::UnexpectedTok)),
        }
    }

    /// Expect an identifier token and get the actual identifier from the text.
    fn expect_identifier(&mut self) -> Result<String> {
        match self.input.try_pop(TokenType::Identifier)? {
            Some(token) => {
                let Token { start, len, .. } = token;
                let name = &self.text[start..(start + len as usize)];
                Ok(name.to_string())
            }
            None => Err(self.error(ErrorKind::UnexpectedTok)),
        }
    }

    fn err_unexpected(&mut self) -> Error {
        self.error(ErrorKind::UnexpectedTok)
        // match self.next() {
        //     Some(tok) => ParseError::Unexpected(tok),
        //     None => ParseError::UnexpectedEof,
        // }
    }

    /// Converts a string's offsets into a real String.
    fn get_string_from_text(&self, start: usize, len: u32) -> String {
        self.text[(start + 1)..(start + len as usize - 1)].to_string()
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind)
    }

    // Actual parsing

    /// Entry point for the parser.
    fn parse_chunk(mut self) -> Result<Chunk> {
        self.parse_statements()?;
        if self.input.next()?.is_some() {
            Err(self.err_unexpected())
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
        while let Some(tok) = self.input.next()? {
            use TokenType::*;
            match tok.typ {
                Identifier | LParen => {
                    self.input.push_back(tok);
                    self.parse_assign_or_call()?;
                }
                If => self.parse_if()?,
                Print => self.parse_print()?,
                While => self.parse_while()?,
                Repeat => self.parse_repeat()?,
                Do => self.parse_do()?,
                Local => self.parse_locals()?,
                For => self.parse_for()?,
                _ => {
                    self.input.push_back(tok);
                    break;
                }
            }
            self.input.try_pop(TokenType::Semi)?;
        }
        Ok(())
    }

    fn parse_assign_or_call(&mut self) -> Result<()> {
        let first_set_instr = match self.parse_place()? {
            Some(i) => i,
            None => {
                // we parsed a function call, we're done.
                return Ok(());
            }
        };

        let mut assignment_code = vec![first_set_instr];

        while self.input.try_pop(TokenType::Comma)?.is_some() {
            match self.parse_place()? {
                Some(instr) => assignment_code.push(instr),
                None => return Err(self.err_unexpected()),
            }
        }
        self.expect(TokenType::Assign)?;

        let num_expressions = self.parse_explist()? as isize;
        let diff = assignment_code.len() as isize - num_expressions;
        if diff > 0 {
            for _ in 0..diff {
                self.push(Instr::PushNil);
            }
        } else {
            for _ in diff..0 {
                self.push(Instr::Pop);
            }
        }

        self.output.extend(assignment_code.into_iter().rev());

        Ok(())
    }

    /// Parse a single place expression or function call. If it's a call,
    /// return `None`. If it's a place expression, returns the instruction
    /// to perform the assignment.
    fn parse_place(&mut self) -> Result<Option<Instr>> {
        if self.input.try_pop(TokenType::LParen)?.is_some() {
            self.parse_expr()?;
            self.expect(TokenType::RParen)?;
            match self.parse_place_extension() {
                Ok(None) => Err(self.err_unexpected()),
                x => x,
            }
        } else {
            let name = self.expect_identifier()?;
            if let Some(typ) = self.input.peek_type()? {
                match typ {
                    TokenType::Assign | TokenType::Comma => {
                        let instr = self.parse_set_identifier(&name)?;
                        Ok(Some(instr))
                    }
                    TokenType::Dot
                    | TokenType::LSquare
                    | TokenType::LCurly
                    | TokenType::LParen
                    | TokenType::Colon
                    | TokenType::LiteralString => {
                        self.parse_get_identifier(&name)?;
                        self.parse_place_extension()
                    }
                    _ => Err(self.err_unexpected()),
                }
            } else {
                Err(self.err_unexpected())
            }
        }
    }

    /// Emit the bytecode to retrieve the value of the given `name`, which
    /// may resolve to either a local or a global.
    fn parse_get_identifier(&mut self, name: &str) -> Result<()> {
        let instr = match find_last_local(&self.locals, name) {
            Some(i) => Instr::GetLocal(i as u8),
            None => {
                let i = self.find_or_add_string(name)?;
                Instr::GetGlobal(i)
            }
        };
        self.push(instr);
        Ok(())
    }

    /// Return the instruction to assign to the given identifier.
    ///
    /// Does not alter `self.output`.
    fn parse_set_identifier(&mut self, name: &str) -> Result<Instr> {
        match find_last_local(&self.locals, name) {
            Some(i) => Ok(Instr::SetLocal(i as u8)),
            None => {
                let i = self.find_or_add_string(name)?;
                Ok(Instr::SetGlobal(i))
            }
        }
    }

    /// Any place expression can be followed by an indexing operation or a
    /// function/method call.
    fn parse_place_extension(&mut self) -> Result<Option<Instr>> {
        if let Some(tok) = self.input.next()? {
            match tok.typ {
                TokenType::Dot => self.parse_place_field(),
                TokenType::LSquare => self.parse_place_index(),
                TokenType::LParen => self.parse_place_call(),
                TokenType::Colon => panic!("Method calls unsupported"),
                TokenType::LiteralString | TokenType::LCurly => {
                    panic!("Unparenthesized function calls unsupported")
                }
                _ => {
                    self.input.push_back(tok);
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    /// Parse a field access for a place expression.
    fn parse_place_field(&mut self) -> Result<Option<Instr>> {
        let name = self.expect_identifier()?;
        let i = self.find_or_add_string(&name)?;
        if let Some(next_tok) = self.input.peek()? {
            if let TokenType::Assign | TokenType::Comma = next_tok.typ {
                Ok(Some(Instr::SetField(i)))
            } else {
                self.push(Instr::GetField(i));
                self.parse_place_extension()
            }
        } else {
            Err(self.err_unexpected())
        }
    }

    /// Parse an indexing operation (`[]`) for a place expression.
    fn parse_place_index(&mut self) -> Result<Option<Instr>> {
        self.parse_expr()?;
        self.expect(TokenType::RSquare)?;
        if let Some(next_tok) = self.input.peek()? {
            if let TokenType::Assign | TokenType::Comma = next_tok.typ {
                Ok(Some(Instr::SetTable))
            } else {
                self.push(Instr::GetTable);
                self.parse_place_extension()
            }
        } else {
            Err(self.err_unexpected())
        }
    }

    /// Parse a function call as part of a place expresion.
    fn parse_place_call(&mut self) -> Result<Option<Instr>> {
        let num_args = if self.input.peek_type_is(TokenType::RParen)? {
            0
        } else {
            self.parse_explist()?
        };
        self.expect(TokenType::RParen)?;
        self.push(Instr::Call(num_args));

        self.parse_place_extension()
    }

    /// Parse a `local` declaration.
    fn parse_locals(&mut self) -> Result<()> {
        let start = self.locals.len() as u8;

        let name1 = self.expect_identifier()?;
        self.add_local(&name1)?;
        let mut num_names = 1;

        while self.input.try_pop(TokenType::Comma)?.is_some() {
            let name = self.expect_identifier()?;
            self.add_local(&name)?;
            num_names += 1;
        }

        if self.input.try_pop(TokenType::Assign)?.is_some() {
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

    /// Parse a for loop, before we know whether it's generic (`for i in t do`) or
    /// numeric (`for i = 1,5 do`).
    fn parse_for(&mut self) -> Result<()> {
        let name = self.expect_identifier()?;
        self.nest_level += 1;
        self.expect(TokenType::Assign)?;
        self.parse_numeric_for(&name)?;
        self.level_down();

        Ok(())
    }

    /// Parse a numeric for, starting with the first expression after the `=`.
    fn parse_numeric_for(&mut self, name: &str) -> Result<()> {
        // The start(current), stop and step are stored in three "hidden" local slots.
        let current_index_slot = self.locals.len() as u8;
        self.add_local("")?;
        self.add_local("")?;
        self.add_local("")?;

        // The actual local is in a fourth slot, so that it can be reassigned to.
        self.add_local(&name)?;

        // First, all 3 control expressions are evaluated.
        self.parse_expr()?;
        self.expect(TokenType::Comma)?;
        self.parse_expr()?;

        // optional step value
        if self.input.try_pop(TokenType::Comma)?.is_some() {
            self.parse_expr()?;
            self.expect(TokenType::Do)?;
        } else if self.input.try_pop(TokenType::Do)?.is_some() {
            let i = self.find_or_add_number(1.0)?;
            self.push(Instr::PushNum(i));
        } else {
            return Err(self.err_unexpected());
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
        dbg!(&self);
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;

        // If there's another arm, add 1 to the branch instruction to skip the closing Jump.
        // If there's no End token, handle it later.
        let body_len = self.output.len() as isize
            + if self.next_tok_is_else_or_elseif()? {
                1
            } else {
                0
            };
        old_output.push(Instr::BranchFalse(body_len));
        old_output.append(&mut self.output);
        self.output = old_output;

        self.parse_if_end()
    }

    fn next_tok_is_else_or_elseif(&mut self) -> Result<bool> {
        Ok(self.input.peek_type_is(TokenType::Else)?
            || self.input.peek_type_is(TokenType::ElseIf)?)
    }

    /// Parse either:
    /// - An 'elseif' condition and body, and any subsequent arms.
    /// - An 'else' body.
    /// - An 'end' token.
    ///
    /// Then handle the logic of closing those.
    fn parse_if_end(&mut self) -> Result<()> {
        self.level_down();
        if self.input.peek_type_is(TokenType::ElseIf)? {
            let mut old_output = Vec::new();
            swap(&mut self.output, &mut old_output);
            self.parse_elseif()?;
            self.close_else_or_elseif(old_output);
        } else if self.input.peek_type_is(TokenType::Else)? {
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
        self.input.next()?;
        self.parse_expr()?;
        self.expect(TokenType::Then)?;
        let mut old_output = Vec::new();
        swap(&mut self.output, &mut old_output);
        self.parse_statements()?;
        let body_len = self.output.len() as isize
            + if self.next_tok_is_else_or_elseif()? {
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
        self.input.next()?;
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
        while self.input.try_pop(TokenType::Comma)?.is_some() {
            if output == u8::MAX {
                return Err(Error::new(ErrorKind::Complexity));
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
        while self.input.try_pop(TokenType::Or)?.is_some() {
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
        while self.input.try_pop(TokenType::And)?.is_some() {
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

        while let Some(t) = self.input.next()? {
            let i = match t.typ {
                TokenType::Less => Instr::Less,
                TokenType::LessEqual => Instr::LessEqual,
                TokenType::Greater => Instr::Greater,
                TokenType::GreaterEqual => Instr::GreaterEqual,
                TokenType::Equal => Instr::Equal,
                TokenType::NotEqual => Instr::NotEqual,
                _ => {
                    self.input.push_back(t);
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
        if self.input.try_pop(TokenType::DotDot)?.is_some() {
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
        while let Some(tok) = self.input.next()? {
            let i = match tok.typ {
                TokenType::Plus => Instr::Add,
                TokenType::Minus => Instr::Subtract,
                _ => {
                    self.input.push_back(tok);
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
        while let Some(tok) = self.input.next()? {
            let i = match tok.typ {
                TokenType::Star => Instr::Multiply,
                TokenType::Slash => Instr::Divide,
                TokenType::Mod => Instr::Mod,
                _ => {
                    self.input.push_back(tok);
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
        if let Some(tok) = self.input.next()? {
            let i = match tok.typ {
                TokenType::Not => Instr::Not,
                TokenType::Hash => Instr::Length,
                TokenType::Minus => Instr::Negate,
                _ => {
                    self.input.push_back(tok);
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
        if self.input.try_pop(TokenType::Caret)?.is_some() {
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
        if let Some(tok) = self.input.next()? {
            match tok.typ {
                TokenType::LCurly => self.parse_table()?,
                TokenType::LParen => {
                    self.parse_expr()?;
                    self.expect(TokenType::RParen)?;
                    self.parse_after_prefixexp()?;
                }
                TokenType::Identifier => {
                    let name = &self.text[tok.start..(tok.start + tok.len as usize)];
                    self.parse_get_identifier(name)?;
                    self.parse_after_prefixexp()?;
                }
                TokenType::LiteralNumber => {
                    let s = &self.text[tok.start..(tok.start + tok.len as usize)];
                    let n = s.parse::<f64>().unwrap();
                    let i = self.find_or_add_number(n)?;
                    self.push(Instr::PushNum(i));
                }
                TokenType::LiteralHexNumber => {
                    let s = &self.text[(tok.start + 2)..(tok.start + tok.len as usize)];
                    let n = u128::from_str_radix(s, 16).unwrap() as f64;
                    let i = self.find_or_add_number(n)?;
                    self.push(Instr::PushNum(i));
                }
                TokenType::LiteralString => {
                    let s = self.get_string_from_text(tok.start, tok.len);
                    let i = self.find_or_add_string(&s)?;
                    self.push(Instr::PushString(i));
                }
                TokenType::Nil => self.push(Instr::PushNil),
                TokenType::False => self.push(Instr::PushBool(false)),
                TokenType::True => self.push(Instr::PushBool(true)),
                TokenType::DotDotDot | TokenType::Function => {
                    return Err(Error::new(ErrorKind::UnsupportedFeature));
                }
                _ => {
                    return Err(self.err_unexpected());
                }
            }
            Ok(())
        } else {
            Err(Error::new(ErrorKind::UnexpectedEof))
        }
    }

    fn parse_table(&mut self) -> Result<()> {
        self.push(Instr::NewTable);
        if self.input.try_pop(TokenType::RCurly)?.is_none() {
            self.parse_table_entry()?;
            while is_fieldsep(self.input.peek()?) {
                self.input.next()?;
                if self.input.peek_type_is(TokenType::RCurly)? {
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
        if let Some(tok) = self.input.next()? {
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
        if self.input.try_pop(TokenType::LParen)?.is_some() {
            self.parse_call_exp()
        } else if self.input.try_pop(TokenType::Dot)?.is_some() {
            self.parse_field()
        } else if self.input.try_pop(TokenType::LSquare)?.is_some() {
            self.parse_expr()?;
            self.expect(TokenType::RSquare)?;
            self.push(Instr::GetTable);
            self.parse_after_prefixexp()
        } else {
            Ok(())
        }
    }

    /// Parse a field access
    fn parse_field(&mut self) -> Result<()> {
        let name = self.expect_identifier()?;
        let i = self.find_or_add_string(&name)?;
        self.push(Instr::GetField(i));
        self.parse_after_prefixexp()
    }

    fn parse_call_exp(&mut self) -> Result<()> {
        let num_args = if self.input.peek_type_is(TokenType::RParen)? {
            0
        } else {
            self.parse_explist()?
        };
        self.expect(TokenType::RParen)?;
        self.push(Instr::Call(num_args));
        Ok(())
    }

    /// Creates a new local slot at the current nest_level.
    /// Fail if we have exceeded the maximum number of locals.
    fn add_local(&mut self, name: &str) -> Result<()> {
        if self.locals.len() == u8::MAX as usize {
            Err(Error::new(ErrorKind::TooManyLocals))
        } else {
            self.locals.push((name.to_string(), self.nest_level));
            if self.locals.len() > self.num_locals as usize {
                self.num_locals += 1;
            }
            Ok(())
        }
    }

    fn find_or_add_string(&mut self, name: &str) -> Result<u8> {
        find_or_add(&mut self.string_literals, name.to_string())
            .ok_or_else(|| Error::new(ErrorKind::TooManyStrings))
    }

    fn find_or_add_number(&mut self, num: f64) -> Result<u8> {
        find_or_add(&mut self.number_literals, num)
            .ok_or_else(|| Error::new(ErrorKind::TooManyStrings))
    }
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

    fn check_it(input: &str, output: Chunk) {
        assert_eq!(parse_str(input).unwrap(), output);
    }

    #[test]
    fn test1() {
        let text = "print 5 + 6";
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Add, Instr::Print],
            number_literals: vec![5.0, 6.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(text, out);
    }

    #[test]
    fn test2() {
        let text = "print -5^2";
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Pow, Negate, Instr::Print],
            number_literals: vec![5.0, 2.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(text, out);
    }

    #[test]
    fn test3() {
        let text = "print 5 + true .. 'hi'";
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
        check_it(text, out);
    }

    #[test]
    fn test4() {
        let text = "print 1 .. 2 + 3";
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
        check_it(text, output);
    }

    #[test]
    fn test5() {
        let text = "print 2^-3";
        let output = Chunk {
            code: vec![PushNum(0), PushNum(1), Negate, Pow, Instr::Print],
            number_literals: vec![2.0, 3.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(text, output);
    }

    #[test]
    fn test6() {
        let text = "print not not 1";
        let output = Chunk {
            code: vec![PushNum(0), Instr::Not, Instr::Not, Instr::Print],
            number_literals: vec![1.0],
            string_literals: vec![],
            num_locals: 0,
        };
        check_it(text, output);
    }

    #[test]
    fn test7() {
        let text = "a = 5";
        let output = Chunk {
            code: vec![PushNum(0), SetGlobal(0)],
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        check_it(text, output);
    }

    #[test]
    fn test8() {
        let text = "print true and false";
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
        check_it(text, output);
    }

    #[test]
    fn test9() {
        let text = "print 5 or nil and true";
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
        check_it(text, output);
    }

    #[test]
    fn test10() {
        let text = "if true then a = 5 end";
        let code = vec![PushBool(true), BranchFalse(2), PushNum(0), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test11() {
        let text = "if true then a = 5 if true then b = 4 end end";
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
        check_it(text, chunk);
    }

    #[test]
    fn test12() {
        let text = "if true then a = 5 else a = 4 end";
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
        check_it(text, chunk);
    }

    #[test]
    fn test13() {
        let text = "if true then a = 5 elseif 6 == 7 then a = 3 else a = 4 end";
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
        check_it(text, chunk);
    }

    #[test]
    fn test14() {
        let text = "while a < 10 do a = a + 1 end";
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
        check_it(text, chunk);
    }

    #[test]
    fn test15() {
        let text = "repeat print 5 until a == b print 4";
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
        check_it(text, chunk);
    }

    #[test]
    fn test16() {
        let text = "local i i = 2";
        let code = vec![PushNil, SetLocal(0), PushNum(0), SetLocal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            string_literals: vec![],
            num_locals: 1,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test17() {
        let text = "local i, j print j";
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
        check_it(text, chunk);
    }

    #[test]
    fn test18() {
        let text = "local i do local i print i end print i";
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
        check_it(text, chunk);
    }

    #[test]
    fn test19() {
        let text = "do local i print i end print i";
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
            string_literals: vec!["i".to_string()],
            num_locals: 1,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test20() {
        let text = "local i if false then local i else print i end";
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
        check_it(text, chunk);
    }

    // for i = 1,5 do print i end
    #[test]
    fn test21() {
        let text = "for i = 1,5 do print i end";
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
        check_it(text, chunk);
    }

    #[test]
    fn test22() {
        let text = "a, b = 1";
        let code = vec![PushNum(0), PushNil, SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test23() {
        let text = "a, b = 1, 2";
        let code = vec![PushNum(0), PushNum(1), SetGlobal(1), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test24() {
        let text = "a, b = 1, 2, 3";
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
        check_it(text, chunk);
    }

    #[test]
    fn test25() {
        let text = "puts()";
        let code = vec![GetGlobal(0), Call(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec!["puts".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test26() {
        let text = "print {x = 5,}";
        let code = vec![NewTable, PushNum(0), SetField(0), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["x".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }

    #[test]
    fn test27() {
        let text = "print t.x.y";
        let code = vec![GetGlobal(0), GetField(1), GetField(2), Instr::Print];
        let chunk = Chunk {
            code,
            number_literals: vec![],
            string_literals: vec!["t".to_string(), "x".to_string(), "y".to_string()],
            num_locals: 0,
        };
        check_it(text, chunk);
    }
}
