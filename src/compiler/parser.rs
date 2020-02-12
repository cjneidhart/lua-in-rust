use super::error::Error;
use super::error::ErrorKind;
use super::error::SyntaxError;
use super::lexer::TokenStream;
use super::Chunk;
use super::Instr;
use super::Result;
use super::Token;
use super::TokenType;

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::mem::swap;
use std::str;
use std::u8;

/// Tracks the current state, to make parsing easier.
#[derive(Debug)]
struct Parser<'a> {
    /// The input token stream.
    input: TokenStream<'a>,
    chunk: Chunk,
    nest_level: i32,
    locals: Vec<(String, i32)>,
}

/// This represents an expression which can appear on the left-hand side of an assignment.
/// Also called an "lvalue" in other languages.
#[derive(Clone, Debug)]
enum PlaceExp {
    /// A local variable, and its index in the list of locals
    Local(u8),
    /// A global variable, and its index in the list of string literals
    Global(u8),
    /// A table index, with `[` and `]`
    TableIndex,
    /// A field access, and the index of the field's identifier in the list of
    /// string literals
    FieldAccess(u8),
}

/// A "prefix expression" is an expression which could be followed by certain
/// extensions and still be a valid expression.
#[derive(Clone, Debug)]
enum PrefixExp {
    /// One of the variants of `PlaceExp`
    Place(PlaceExp),
    /// A function call, and the number of arguments
    FunctionCall(u8),
    /// An expression wrapped in parentheses
    Parenthesized,
}

#[derive(Debug)]
enum ExpDesc {
    Prefix(PrefixExp),
    Other,
}

/// Parses Lua source code into a `Chunk`.
pub(super) fn parse_str(source: &str) -> Result<Chunk> {
    let parser = Parser {
        input: TokenStream::new(source),
        chunk: Chunk::default(),
        nest_level: 0,
        locals: Vec::new(),
    };
    parser.parse_all()
}

impl<'a> Parser<'a> {
    // Helper functions

    /// Creates a new local slot at the current nest_level.
    /// Fails if we have exceeded the maximum number of locals.
    fn add_local(&mut self, name: &str) -> Result<()> {
        if self.locals.len() == u8::MAX as usize {
            Err(self.error(SyntaxError::TooManyLocals))
        } else {
            self.locals.push((name.to_string(), self.nest_level));
            if self.locals.len() > self.chunk.num_locals as usize {
                self.chunk.num_locals += 1;
            }
            Ok(())
        }
    }

    /// Constructs an error of the given kind at the current position.
    // TODO: rename to error_here
    fn error(&self, kind: impl Into<ErrorKind>) -> Error {
        let pos = self.input.pos();
        self.error_at(kind, pos)
    }

    /// Constructs an error of the given kind and position.
    fn error_at(&self, kind: impl Into<ErrorKind>, pos: usize) -> Error {
        let (line, column) = self.input.line_and_column(pos);
        Error::new(kind, line, column)
    }

    /// Constructs an error for when a specific `TokenType` was expected but not found.
    fn err_unexpected(&self, token: Token, _expected: TokenType) -> Error {
        let error_kind = if token.typ == TokenType::EndOfFile {
            SyntaxError::UnexpectedEof
        } else {
            SyntaxError::UnexpectedTok
        };
        self.error_at(error_kind, token.start)
    }

    /// Pulls a token off the input and checks it against `expected`.
    /// Returns the token if it matches, `Err` otherwise.
    fn expect(&mut self, expected: TokenType) -> Result<Token> {
        let token = self.input.next()?;
        if token.typ == expected {
            Ok(token)
        } else {
            Err(self.err_unexpected(token, expected))
        }
    }

    /// Expects an identifier token and returns the identifier as a string.
    fn expect_identifier(&mut self) -> Result<&'a str> {
        let token = self.expect(TokenType::Identifier)?;
        let name = self.get_text(token);
        Ok(name)
    }

    /// Expects an identifier and returns the id of its string literal.
    fn expect_identifier_id(&mut self) -> Result<u8> {
        let name = self.expect_identifier()?;
        self.find_or_add_string(name)
    }

    /// Stores a literal string and returns its index.
    fn find_or_add_string(&mut self, string: &str) -> Result<u8> {
        find_or_add(&mut self.chunk.string_literals, string)
            .ok_or_else(|| self.error(SyntaxError::TooManyStrings))
    }

    /// Stores a literal number and returns its index.
    fn find_or_add_number(&mut self, num: f64) -> Result<u8> {
        find_or_add(&mut self.chunk.number_literals, &num)
            .ok_or_else(|| self.error(SyntaxError::TooManyNumbers))
    }

    /// Converts a literal string's offsets into a real String.
    fn get_literal_string_contents(&self, tok: Token) -> &'a str {
        // Chop off the quotes
        let Token { start, len, typ } = tok;
        assert_eq!(typ, TokenType::LiteralString);
        assert!(len >= 2);
        let range = (start + 1)..(start + len as usize - 1);
        self.input.from_src(range)
    }

    /// Gets the original source code contained by a token.
    fn get_text(&self, token: Token) -> &'a str {
        self.input.from_src(token.range())
    }

    /// Lowers the nesting level by one, discarding any locals from that block.
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

    /// Adds an instruction to the output.
    fn push(&mut self, instr: Instr) {
        self.chunk.code.push(instr);
    }

    // Actual parsing

    /// The main entry point for the parser. This parses the entire input.
    fn parse_all(mut self) -> Result<Chunk> {
        let c = self.parse_chunk(&[])?;
        let token = self.input.next()?;
        if let TokenType::EndOfFile = token.typ {
            Ok(c)
        } else {
            Err(self.err_unexpected(token, TokenType::EndOfFile))
        }
    }

    /// Parses a `Chunk`.
    fn parse_chunk(&mut self, params: &[&str]) -> Result<Chunk> {
        let mut tmp_chunk = Chunk::default();
        swap(&mut tmp_chunk, &mut self.chunk);

        self.chunk.num_params = params.len() as u8;
        for &param in params {
            self.locals.push((param.into(), self.nest_level));
        }

        self.parse_statements()?;
        self.push(Instr::Return(0));

        swap(&mut tmp_chunk, &mut self.chunk);

        if option_env!("LUA_DEBUG_PARSER").is_some() {
            println!("Compiled chunk: {:#?}", &tmp_chunk);
        }

        Ok(tmp_chunk)
    }

    /// Parses 0 or more statements, possibly separated by semicolons.
    fn parse_statements(&mut self) -> Result<()> {
        loop {
            match self.input.peek_type()? {
                TokenType::Identifier | TokenType::LParen => self.parse_assign_or_call()?,
                TokenType::If => self.parse_if()?,
                TokenType::While => self.parse_while()?,
                TokenType::Repeat => self.parse_repeat()?,
                TokenType::Do => self.parse_do()?,
                TokenType::Local => self.parse_locals()?,
                TokenType::For => self.parse_for()?,
                TokenType::Function => self.parse_fndecl()?,
                TokenType::Semi => {
                    self.input.next()?;
                }
                TokenType::Return => break self.parse_return(),
                _ => break Ok(()),
            }
        }
    }

    /// Parses a function declaration, which is any statement that starts with
    /// the keyword `function`.
    fn parse_fndecl(&mut self) -> Result<()> {
        self.input.next()?; // 'function' keyword
        let name = self.expect_identifier()?;
        match self.input.peek_type()? {
            TokenType::Dot => self.parse_fndecl_table(name),
            _ => self.parse_fndecl_basic(name),
        }
    }

    /// Parses a basic function declaration, which just assigns the function to
    /// a local or global variable.
    fn parse_fndecl_basic(&mut self, name: &'a str) -> Result<()> {
        let place_exp = self.parse_prefix_identifier(name)?;
        let instr = match place_exp {
            PlaceExp::Local(i) => Instr::SetLocal(i),
            PlaceExp::Global(i) => Instr::SetGlobal(i),
            _ => unreachable!("place expression was not a local or global variable"),
        };
        self.parse_fndef()?;
        self.push(instr);
        Ok(())
    }

    fn parse_fndecl_table(&mut self, table_name: &'a str) -> Result<()> {
        // Push the table onto the stack.
        let table_instr = match self.parse_prefix_identifier(table_name)? {
            PlaceExp::Local(i) => Instr::GetLocal(i),
            PlaceExp::Global(i) => Instr::GetGlobal(i),
            _ => unreachable!("place expression was not a local or global variable"),
        };
        self.push(table_instr);

        // Parse all the fields. There must be at least one.
        self.expect(TokenType::Dot)?;
        let mut last_field_id = self.expect_identifier_id()?;
        while self.input.try_pop(TokenType::Dot)?.is_some() {
            self.push(Instr::GetField(last_field_id));
            last_field_id = self.expect_identifier_id()?;
        }

        // Parse the function params and body.
        self.parse_fndef()?;
        self.push(Instr::SetField(0, last_field_id));
        Ok(())
    }

    /// Parses a return statement. Return statements must always come last in a
    /// block.
    fn parse_return(&mut self) -> Result<()> {
        self.input.next()?; // 'return' keyword
        let (n, _) = self.parse_explist()?;
        self.push(Instr::Return(n));
        self.input.try_pop(TokenType::Semi)?;
        Ok(())
    }

    /// Parses a statement which could be a variable assignment or a function call.
    fn parse_assign_or_call(&mut self) -> Result<()> {
        match self.parse_prefix_exp()? {
            PrefixExp::Parenthesized => {
                let tok = self.input.next()?;
                Err(self.err_unexpected(tok, TokenType::Assign))
            }
            PrefixExp::FunctionCall(num_args) => {
                self.push(Instr::Call(num_args, 0));
                Ok(())
            }
            PrefixExp::Place(first_place) => self.parse_assign(first_place),
        }
    }

    /// Parses a variable assignment.
    fn parse_assign(&mut self, first_exp: PlaceExp) -> Result<()> {
        let mut places = vec![first_exp];
        while self.input.try_pop(TokenType::Comma)?.is_some() {
            places.push(self.parse_place_exp()?);
        }

        self.expect(TokenType::Assign)?;
        let num_lvals = places.len() as isize;
        let (num_rvals, last_exp) = self.parse_explist()?;
        let num_rvals = num_rvals as isize;
        let diff = num_lvals - num_rvals;
        if diff > 0 {
            if let ExpDesc::Prefix(PrefixExp::FunctionCall(_)) = last_exp {
                let num_args = match self.chunk.code.pop() {
                    Some(Instr::Call(args, _)) => args,
                    i => unreachable!("PrefixExp::FunctionCall but last instruction was {:?}", i),
                };
                self.push(Instr::Call(num_args, 1 + diff as u8));
            } else {
                for _ in 0..diff {
                    self.push(Instr::PushNil);
                }
            }
        } else {
            // discard excess rvals
            for _ in diff..0 {
                self.push(Instr::Pop);
            }
        }

        places.reverse();
        for (i, place_exp) in places.into_iter().enumerate() {
            let instr = match place_exp {
                PlaceExp::Local(i) => Instr::SetLocal(i),
                PlaceExp::Global(i) => Instr::SetGlobal(i),
                PlaceExp::FieldAccess(literal_id) => {
                    let stack_offset = num_lvals as u8 - i as u8 - 1;
                    Instr::SetField(stack_offset, literal_id)
                }
                PlaceExp::TableIndex => {
                    let stack_offset = num_lvals as u8 - i as u8 - 1;
                    Instr::SetTable(stack_offset)
                }
            };
            self.push(instr);
        }

        Ok(())
    }

    /// Parses an expression which can appear on the left side of an assignment.
    fn parse_place_exp(&mut self) -> Result<PlaceExp> {
        match self.parse_prefix_exp()? {
            PrefixExp::Parenthesized | PrefixExp::FunctionCall(_) => {
                let tok = self.input.next()?;
                Err(self.err_unexpected(tok, TokenType::Assign))
            }
            PrefixExp::Place(place) => Ok(place),
        }
    }

    /// Emits code to evaluate the prefix expression as a normal expression.
    fn eval_prefix_exp(&mut self, exp: PrefixExp) {
        match exp {
            PrefixExp::FunctionCall(num_args) => {
                self.push(Instr::Call(num_args, 1));
            }
            PrefixExp::Parenthesized => (),
            PrefixExp::Place(place) => {
                let instr = match place {
                    PlaceExp::Local(i) => Instr::GetLocal(i),
                    PlaceExp::Global(i) => Instr::GetGlobal(i),
                    PlaceExp::FieldAccess(i) => Instr::GetField(i),
                    PlaceExp::TableIndex => Instr::GetTable,
                };
                self.push(instr);
            }
        }
    }

    /// Parses a variable's name. This should only ever return `Local` or `Global`.
    fn parse_prefix_identifier(&mut self, name: &str) -> Result<PlaceExp> {
        match find_last_local(&self.locals, name) {
            Some(i) => Ok(PlaceExp::Local(i as u8)),
            None => {
                let i = self.find_or_add_string(name)?;
                Ok(PlaceExp::Global(i))
            }
        }
    }

    /// Parses a `local` declaration.
    fn parse_locals(&mut self) -> Result<()> {
        self.input.next().unwrap(); // `local` keyword
        let old_local_count = self.locals.len() as u8;

        let mut names = Vec::new();
        // There has to be at least one name
        names.push(self.expect_identifier()?);

        while self.input.try_pop(TokenType::Comma)?.is_some() {
            names.push(self.expect_identifier()?);
        }

        let num_names = names.len() as u8;
        if self.input.try_pop(TokenType::Assign)?.is_some() {
            // Also perform the assignment
            let (num_rvalues, last_exp) = self.parse_explist()?;
            match num_names.cmp(&num_rvalues) {
                Ordering::Less => {
                    for _ in num_names..num_rvalues {
                        self.push(Instr::Pop);
                    }
                }
                Ordering::Greater => {
                    if let ExpDesc::Prefix(PrefixExp::FunctionCall(num_args)) = last_exp {
                        self.chunk.code.pop(); // Pop the old 'Call' instruction
                        self.push(Instr::Call(num_args, 1 + num_names - num_rvalues));
                    } else {
                        for _ in num_rvalues..num_names {
                            self.push(Instr::PushNil);
                        }
                    }
                }
                Ordering::Equal => (),
            }
        } else {
            // They've only been declared, just set them all nil
            for _ in &names {
                self.push(Instr::PushNil);
            }
        }

        // Actually perform the assignment
        for i in (0..num_names).rev() {
            self.push(Instr::SetLocal(i + old_local_count));
        }

        // Bring the new variables into scope. It is important they are not
        // in scope until after this statement.
        for name in names {
            self.add_local(name)?;
        }

        Ok(())
    }

    /// Parses a `for` loop, before we know whether it's generic (`for i in t do`) or
    /// numeric (`for i = 1,5 do`).
    fn parse_for(&mut self) -> Result<()> {
        self.input.next()?; // `for` keyword
        let name = self.expect_identifier()?;
        self.nest_level += 1;
        self.expect(TokenType::Assign)?;
        self.parse_numeric_for(&name)?;
        self.level_down();

        Ok(())
    }

    /// Parses a numeric `for` loop, starting with the first expression after the `=`.
    fn parse_numeric_for(&mut self, name: &str) -> Result<()> {
        // The start(current), stop and step are stored in three "hidden" local slots.
        let current_local_slot = self.locals.len() as u8;
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
        self.parse_numeric_for_step()?;

        // The ForPrep command pulls three values off the stack and places them
        // into locals to use in the loop.
        let loop_start_instr_index = self.chunk.code.len();
        self.push(Instr::ForPrep(current_local_slot, -1));

        // body
        self.parse_statements()?;
        self.expect(TokenType::End)?;
        let body_length = (self.chunk.code.len() - loop_start_instr_index) as isize;
        self.push(Instr::ForLoop(current_local_slot, -(body_length)));

        // Correct the ForPrep instruction.
        self.chunk.code[loop_start_instr_index] = Instr::ForPrep(current_local_slot, body_length);

        Ok(())
    }

    /// Parses the optional step value of a numeric `for` loop.
    fn parse_numeric_for_step(&mut self) -> Result<()> {
        let next_token = self.input.next()?;
        match next_token.typ {
            TokenType::Comma => {
                self.parse_expr()?;
                self.expect(TokenType::Do)?;
                Ok(())
            }
            TokenType::Do => {
                let i = self.find_or_add_number(1.0)?;
                self.push(Instr::PushNum(i));
                Ok(())
            }
            _ => Err(self.err_unexpected(next_token, TokenType::Do)),
        }
    }

    /// Parses a `do ... end` statement.
    fn parse_do(&mut self) -> Result<()> {
        self.input.next()?; // `do` keyword
        self.nest_level += 1;
        self.parse_statements()?;
        self.expect(TokenType::End)?;
        self.level_down();
        Ok(())
    }

    /// Parses a `repeat ... until` statement.
    fn parse_repeat(&mut self) -> Result<()> {
        self.input.next()?; // `repeat` keyword
        self.nest_level += 1;
        let body_start = self.chunk.code.len() as isize;
        self.parse_statements()?;
        self.expect(TokenType::Until)?;
        self.parse_expr()?;
        let expr_end = self.chunk.code.len() as isize;
        self.push(Instr::BranchFalse(body_start - (expr_end + 1)));
        self.level_down();
        Ok(())
    }

    /// Parses a `while ... do ... end` statement.
    fn parse_while(&mut self) -> Result<()> {
        self.input.next()?; // `while` keyword
        self.nest_level += 1;
        let condition_start = self.chunk.code.len() as isize;
        self.parse_expr()?;
        self.expect(TokenType::Do)?;
        let mut old_output = Vec::new();
        swap(&mut self.chunk.code, &mut old_output);
        self.parse_statements()?;
        old_output.push(Instr::BranchFalse(self.chunk.code.len() as isize + 1));
        old_output.append(&mut self.chunk.code);
        self.chunk.code = old_output;

        self.expect(TokenType::End)?;
        self.push(Instr::Jump(
            condition_start - (self.chunk.code.len() as isize + 1),
        ));
        self.level_down();

        Ok(())
    }

    /// Parses an if-then statement, including any attached `else` or `elseif` branches.
    fn parse_if(&mut self) -> Result<()> {
        self.parse_if_arm()
    }

    /// Parses an `if` or `elseif` block and any subsequent `elseif` or `else`
    /// blocks in the same chain.
    fn parse_if_arm(&mut self) -> Result<()> {
        self.input.next()?; // `if` or `elseif` keyword
        self.parse_expr()?;
        self.expect(TokenType::Then)?;
        self.nest_level += 1;

        let branch_instr_index = self.chunk.code.len();
        self.push(Instr::BranchFalse(0));

        self.parse_statements()?;
        let mut branch_target = self.chunk.code.len();

        self.close_if_arm()?;
        if self.chunk.code.len() > branch_target {
            // If the size has changed, the first instruction added was a
            // Jump, so we need to skip it.
            branch_target += 1;
        }

        let branch_offset = (branch_target - branch_instr_index - 1) as isize;
        self.chunk.code[branch_instr_index] = Instr::BranchFalse(branch_offset);
        Ok(())
    }

    /// Parses the closing keyword of an `if` or `elseif` arms, and any arms
    /// that may follow.
    fn close_if_arm(&mut self) -> Result<()> {
        self.level_down();
        match self.input.peek_type()? {
            TokenType::ElseIf => self.parse_else_or_elseif(true),
            TokenType::Else => self.parse_else_or_elseif(false),
            _ => {
                self.expect(TokenType::End)?;
                Ok(())
            }
        }
    }

    /// Parses an `elseif` or `else` block, and handles the `Jump` instruction
    /// for the end of the preceding block.
    fn parse_else_or_elseif(&mut self, elseif: bool) -> Result<()> {
        let jump_instr_index = self.chunk.code.len();
        self.push(Instr::Jump(0));
        if elseif {
            self.parse_if_arm()?;
        } else {
            self.parse_else()?;
        }
        let new_len = self.chunk.code.len();
        let jump_len = new_len - jump_instr_index - 1;
        self.chunk.code[jump_instr_index] = Instr::Jump(jump_len as isize);
        Ok(())
    }

    /// Parses an `else` block.
    fn parse_else(&mut self) -> Result<()> {
        self.nest_level += 1;
        self.input.next()?; // `else` keyword
        self.parse_statements()?;
        self.expect(TokenType::End)?;
        self.level_down();
        Ok(())
    }

    /// Parses a comma-separated list of expressions. Trailing and leading
    /// commas are not allowed. Returns how many expressions were parsed.
    /// `nvalues` is the expected number of values.
    fn parse_explist(&mut self) -> Result<(u8, ExpDesc)> {
        // An explist has to have at least one expression.
        let mut last_exp_desc = self.parse_expr()?;
        let mut num_expressions = 1;
        while let Some(token) = self.input.try_pop(TokenType::Comma)? {
            if num_expressions == u8::MAX {
                return Err(self.error_at(SyntaxError::Complexity, token.start));
            }
            last_exp_desc = self.parse_expr()?;
            num_expressions += 1;
        }

        Ok((num_expressions, last_exp_desc))
    }

    /// Parses a single expression.
    fn parse_expr(&mut self) -> Result<ExpDesc> {
        self.parse_or()
    }

    /// Parses an `or` expression. Precedence 8.
    fn parse_or(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_and()?;

        while self.input.try_pop(TokenType::Or)?.is_some() {
            exp_desc = ExpDesc::Other;
            let branch_instr_index = self.chunk.code.len();
            self.push(Instr::BranchTrueKeep(0));
            // If we don't short-circuit, pop the left-hand expression
            self.push(Instr::Pop);
            self.parse_and()?;
            let branch_offset = (self.chunk.code.len() - branch_instr_index - 1) as isize;
            self.chunk.code[branch_instr_index] = Instr::BranchTrueKeep(branch_offset);
        }

        Ok(exp_desc)
    }

    /// Parses `and` expression. Precedence 7.
    fn parse_and(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_comparison()?;

        while self.input.try_pop(TokenType::And)?.is_some() {
            exp_desc = ExpDesc::Other;
            let branch_instr_index = self.chunk.code.len();
            self.push(Instr::BranchFalseKeep(0));
            // If we don't short-circuit, pop the left-hand expression
            self.push(Instr::Pop);
            self.parse_comparison()?;
            let branch_offset = (self.chunk.code.len() - branch_instr_index - 1) as isize;
            self.chunk.code[branch_instr_index] = Instr::BranchFalseKeep(branch_offset);
        }

        Ok(exp_desc)
    }

    /// Parses a comparison expression. Precedence 6.
    ///
    /// `==`, `~=`, `<`, `<=`, `>`, `>=`
    fn parse_comparison(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_concat()?;
        loop {
            let instr = match self.input.peek_type()? {
                TokenType::Less => Instr::Less,
                TokenType::LessEqual => Instr::LessEqual,
                TokenType::Greater => Instr::Greater,
                TokenType::GreaterEqual => Instr::GreaterEqual,
                TokenType::Equal => Instr::Equal,
                TokenType::NotEqual => Instr::NotEqual,
                _ => break,
            };
            exp_desc = ExpDesc::Other;
            self.input.next()?;
            self.parse_concat()?;
            self.push(instr);
        }
        Ok(exp_desc)
    }

    /// Parses a string concatenation expression (`..`). Precedence 5.
    fn parse_concat(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_addition()?;
        if self.input.try_pop(TokenType::DotDot)?.is_some() {
            exp_desc = ExpDesc::Other;
            self.parse_concat()?;
            self.push(Instr::Concat);
        }

        Ok(exp_desc)
    }

    /// Parses an addition expression (`+`, `-`). Precedence 4.
    fn parse_addition(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_multiplication()?;
        loop {
            let instr = match self.input.peek_type()? {
                TokenType::Plus => Instr::Add,
                TokenType::Minus => Instr::Subtract,
                _ => break,
            };
            exp_desc = ExpDesc::Other;
            self.input.next()?;
            self.parse_multiplication()?;
            self.push(instr);
        }
        Ok(exp_desc)
    }

    /// Parses a multiplication expression (`*`, `/`, `%`). Precedence 3.
    fn parse_multiplication(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_unary()?;
        loop {
            let instr = match self.input.peek_type()? {
                TokenType::Star => Instr::Multiply,
                TokenType::Slash => Instr::Divide,
                TokenType::Mod => Instr::Mod,
                _ => break,
            };
            exp_desc = ExpDesc::Other;
            self.input.next()?;
            self.parse_unary()?;
            self.push(instr);
        }
        Ok(exp_desc)
    }

    /// Parses a unary expression (`not`, `#`, `-`). Precedence 2.
    fn parse_unary(&mut self) -> Result<ExpDesc> {
        let instr = match self.input.peek_type()? {
            TokenType::Not => Instr::Not,
            TokenType::Hash => Instr::Length,
            TokenType::Minus => Instr::Negate,
            _ => {
                return self.parse_pow();
            }
        };
        self.input.next()?;
        self.parse_unary()?;
        self.push(instr);

        Ok(ExpDesc::Other)
    }

    /// Parse an exponentiation expression (`^`). Right-associative, Precedence 1.
    fn parse_pow(&mut self) -> Result<ExpDesc> {
        let mut exp_desc = self.parse_primary()?;
        if self.input.try_pop(TokenType::Caret)?.is_some() {
            exp_desc = ExpDesc::Other;
            self.parse_unary()?;
            self.push(Instr::Pow);
        }

        Ok(exp_desc)
    }

    /// Parses a 'primary' expression. See `parse_prefix_exp` and `parse_expr_base` for details.
    fn parse_primary(&mut self) -> Result<ExpDesc> {
        match self.input.peek_type()? {
            TokenType::Identifier | TokenType::LParen => {
                let prefix = self.parse_prefix_exp()?;
                self.eval_prefix_exp(prefix.clone());
                Ok(ExpDesc::Prefix(prefix))
            }
            _ => self.parse_expr_base(),
        }
    }

    /// Parses a `prefix expression`. Prefix expressions are the expressions
    /// which can appear on the left side of a function call, table index, or
    /// field access.
    fn parse_prefix_exp(&mut self) -> Result<PrefixExp> {
        let tok = self.input.next()?;
        let prefix = match tok.typ {
            TokenType::Identifier => {
                let text = self.get_text(tok);
                let place = self.parse_prefix_identifier(text)?;
                PrefixExp::Place(place)
            }
            TokenType::LParen => {
                self.parse_expr()?;
                self.expect(TokenType::RParen)?;
                PrefixExp::Parenthesized
            }
            _ => {
                return Err(self.err_unexpected(tok, TokenType::Identifier));
            }
        };
        self.parse_prefix_extension(prefix)
    }

    /// Attempts to parse an extension to a prefix expression: a field access,
    /// table index, or function/method call.
    fn parse_prefix_extension(&mut self, base_expr: PrefixExp) -> Result<PrefixExp> {
        match self.input.peek_type()? {
            TokenType::Dot => {
                self.eval_prefix_exp(base_expr);
                self.input.next()?;
                let name = self.expect_identifier()?;
                let i = self.find_or_add_string(&name)?;
                let prefix = PrefixExp::Place(PlaceExp::FieldAccess(i));
                self.parse_prefix_extension(prefix)
            }
            TokenType::LSquare => {
                self.eval_prefix_exp(base_expr);
                self.input.next()?;
                self.parse_expr()?;
                self.expect(TokenType::RSquare)?;
                let prefix = PrefixExp::Place(PlaceExp::TableIndex);
                self.parse_prefix_extension(prefix)
            }
            TokenType::LParen => {
                self.eval_prefix_exp(base_expr);
                self.input.next()?;
                let (num_args, _) = self.parse_call()?;
                let prefix = PrefixExp::FunctionCall(num_args);
                self.parse_prefix_extension(prefix)
            }
            TokenType::Colon => panic!("Method calls unsupported"),
            TokenType::LiteralString | TokenType::LCurly => {
                panic!("Unparenthesized function calls unsupported")
            }
            _ => Ok(base_expr),
        }
    }

    /// Parses a 'base' expression, after eliminating any operators. This can be:
    /// * A literal number
    /// * A literal string
    /// * A function definition
    /// * One of the keywords `nil`, `false` or `true
    /// * A table constructor
    fn parse_expr_base(&mut self) -> Result<ExpDesc> {
        let tok = self.input.next()?;
        match tok.typ {
            TokenType::LCurly => self.parse_table()?,
            TokenType::LiteralNumber => {
                let text = self.get_text(tok);
                let number = text.parse::<f64>().unwrap();
                let idx = self.find_or_add_number(number)?;
                self.push(Instr::PushNum(idx));
            }
            TokenType::LiteralHexNumber => {
                // Cut off the "0x"
                let text = &self.get_text(tok)[2..];
                let number = u128::from_str_radix(text, 16).unwrap() as f64;
                let idx = self.find_or_add_number(number)?;
                self.push(Instr::PushNum(idx));
            }
            TokenType::LiteralString => {
                let text = self.get_literal_string_contents(tok);
                let idx = self.find_or_add_string(text)?;
                self.push(Instr::PushString(idx));
            }
            TokenType::Function => {
                self.parse_fndef()?;
            }
            TokenType::Nil => self.push(Instr::PushNil),
            TokenType::False => self.push(Instr::PushBool(false)),
            TokenType::True => self.push(Instr::PushBool(true)),
            TokenType::DotDotDot => {
                return Err(self.error(ErrorKind::UnsupportedFeature));
            }
            _ => {
                return Err(self.err_unexpected(tok, TokenType::Nil));
            }
        }
        Ok(ExpDesc::Other)
    }

    /// Parses the parameters in a function definition.
    fn parse_params(&mut self) -> Result<Vec<&'a str>> {
        self.expect(TokenType::LParen)?;
        let mut args = Vec::new();
        if self.input.try_pop(TokenType::RParen)?.is_some() {
            return Ok(args);
        }
        args.push(self.expect_identifier()?);
        while self.input.try_pop(TokenType::Comma)?.is_some() {
            args.push(self.expect_identifier()?);
        }
        self.expect(TokenType::RParen)?;
        Ok(args)
    }

    /// Parses the parameters and body of a function definition.
    fn parse_fndef(&mut self) -> Result<()> {
        let params = self.parse_params()?;
        if self.chunk.nested.len() >= u8::MAX as usize {
            return Err(self.error(SyntaxError::Complexity));
        }

        self.nest_level += 1;
        let new_chunk = self.parse_chunk(&params)?;
        self.level_down();

        self.chunk.nested.push(new_chunk);
        self.push(Instr::Closure(self.chunk.nested.len() as u8 - 1));
        self.expect(TokenType::End)?;
        Ok(())
    }

    /// Parses a table constructor.
    fn parse_table(&mut self) -> Result<()> {
        self.push(Instr::NewTable);
        if self.input.try_pop(TokenType::RCurly)?.is_none() {
            // i is the number of array-style entries.
            let mut i = 0;
            i = self.parse_table_entry(i)?;
            while let TokenType::Comma | TokenType::Semi = self.input.peek_type()? {
                self.input.next()?;
                if self.input.check_type(TokenType::RCurly)? {
                    break;
                } else {
                    i = self.parse_table_entry(i)?;
                }
            }
            self.expect(TokenType::RCurly)?;

            if i > 0 {
                self.push(Instr::SetList(i));
            }
        }
        Ok(())
    }

    /// Parses a table entry.
    fn parse_table_entry(&mut self, counter: u8) -> Result<u8> {
        match self.input.peek_type()? {
            TokenType::Identifier => {
                let index = self.expect_identifier_id().unwrap();
                self.expect(TokenType::Assign)?;
                self.parse_expr()?;
                self.push(Instr::InitField(counter, index));
                Ok(counter)
            }
            TokenType::LSquare => {
                self.input.next().unwrap();
                self.parse_expr()?;
                self.expect(TokenType::RSquare)?;
                self.expect(TokenType::Assign)?;
                self.parse_expr()?;
                self.push(Instr::InitIndex(counter));
                Ok(counter)
            }
            _ => {
                if counter == u8::MAX {
                    return Err(self.error(SyntaxError::Complexity));
                }
                self.parse_expr()?;
                Ok(counter + 1)
            }
        }
    }

    /// Parses a function call. Returns the number of arguments.
    fn parse_call(&mut self) -> Result<(u8, ExpDesc)> {
        let tup = if self.input.check_type(TokenType::RParen)? {
            (0, ExpDesc::Other)
        } else {
            self.parse_explist()?
        };
        self.expect(TokenType::RParen)?;
        Ok(tup)
    }
}

/// Finds the index of the last local entry which matches `name`.
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
fn find_or_add<T, E>(queue: &mut Vec<T>, x: &E) -> Option<u8>
where
    T: Borrow<E> + PartialEq<E>,
    E: PartialEq<T> + ToOwned<Owned = T> + ?Sized,
{
    match queue.iter().position(|y| y == x) {
        Some(i) => Some(i as u8),
        None => {
            let i = queue.len();
            if i == u8::MAX as usize {
                None
            } else {
                queue.push(x.to_owned());
                Some(i as u8)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::parse_str;
    use super::Chunk;
    use super::Instr::{self, *};

    fn check_it(input: &str, output: Chunk) {
        assert_eq!(parse_str(input).unwrap(), output);
    }

    #[test]
    fn test01() {
        let text = "x = 5 + 6";
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Add, SetGlobal(0), Return(0)],
            number_literals: vec![5.0, 6.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, out);
    }

    #[test]
    fn test02() {
        let text = "x = -5^2";
        let out = Chunk {
            code: vec![PushNum(0), PushNum(1), Pow, Negate, SetGlobal(0), Return(0)],
            number_literals: vec![5.0, 2.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, out);
    }

    #[test]
    fn test03() {
        let text = "x = 5 + true .. 'hi'";
        let out = Chunk {
            code: vec![
                PushNum(0),
                PushBool(true),
                Add,
                PushString(1),
                Concat,
                SetGlobal(0),
                Return(0),
            ],
            number_literals: vec![5.0],
            string_literals: vec!["x".into(), "hi".into()],
            ..Chunk::default()
        };
        check_it(text, out);
    }

    #[test]
    fn test04() {
        let text = "x = 1 .. 2 + 3";
        let output = Chunk {
            code: vec![
                PushNum(0),
                PushNum(1),
                PushNum(2),
                Add,
                Concat,
                SetGlobal(0),
                Return(0),
            ],
            number_literals: vec![1.0, 2.0, 3.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test05() {
        let text = "x = 2^-3";
        let output = Chunk {
            code: vec![PushNum(0), PushNum(1), Negate, Pow, SetGlobal(0), Return(0)],
            number_literals: vec![2.0, 3.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test06() {
        let text = "x=  not not 1";
        let output = Chunk {
            code: vec![PushNum(0), Instr::Not, Instr::Not, SetGlobal(0), Return(0)],
            number_literals: vec![1.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test07() {
        let text = "a = 5";
        let output = Chunk {
            code: vec![PushNum(0), SetGlobal(0), Return(0)],
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test08() {
        let text = "x = true and false";
        let output = Chunk {
            code: vec![
                PushBool(true),
                BranchFalseKeep(2),
                Pop,
                PushBool(false),
                SetGlobal(0),
                Return(0),
            ],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test09() {
        let text = "x =  5 or nil and true";
        let code = vec![
            PushNum(0),
            BranchTrueKeep(5),
            Pop,
            PushNil,
            BranchFalseKeep(2),
            Pop,
            PushBool(true),
            SetGlobal(0),
            Return(0),
        ];
        let output = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["x".into()],
            ..Chunk::default()
        };
        check_it(text, output);
    }

    #[test]
    fn test10() {
        let text = "if true then a = 5 end";
        let code = vec![
            PushBool(true),
            BranchFalse(2),
            PushNum(0),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
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
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            ..Chunk::default()
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
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
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
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 6.0, 7.0, 3.0, 4.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
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
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![10.0, 1.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test15() {
        let text = "repeat local x = 5 until a == b y = 4";
        let code = vec![
            PushNum(0),
            SetLocal(0),
            GetGlobal(0),
            GetGlobal(1),
            Instr::Equal,
            BranchFalse(-6),
            PushNum(1),
            SetGlobal(2),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0, 4.0],
            string_literals: vec!["a".into(), "b".into(), "y".into()],
            num_locals: 1,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test16() {
        let text = "local i i = 2";
        let code = vec![PushNil, SetLocal(0), PushNum(0), SetLocal(0), Return(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            num_locals: 1,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test17() {
        let text = "local i, j print(j)";
        let code = vec![
            PushNil,
            PushNil,
            SetLocal(1),
            SetLocal(0),
            GetGlobal(0),
            GetLocal(1),
            Call(1, 0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            string_literals: vec!["print".into()],
            num_locals: 2,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test18() {
        let text = "local i do local i x = i end x = i";
        let code = vec![
            PushNil,
            SetLocal(0),
            PushNil,
            SetLocal(1),
            GetLocal(1),
            SetGlobal(0),
            GetLocal(0),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            string_literals: vec!["x".into()],
            num_locals: 2,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test19() {
        let text = "do local i x = i end x = i";
        let code = vec![
            PushNil,
            SetLocal(0),
            GetLocal(0),
            SetGlobal(0),
            GetGlobal(1),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            string_literals: vec!["x".into(), "i".into()],
            num_locals: 1,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test20() {
        let text = "local i if false then local i else x = i end";
        let code = vec![
            PushNil,
            SetLocal(0),
            PushBool(false),
            BranchFalse(3),
            PushNil,
            SetLocal(1),
            Jump(2),
            GetLocal(0),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            string_literals: vec!["x".into()],
            num_locals: 2,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test21() {
        let text = "for i = 1,5 do x = i end";
        let code = vec![
            PushNum(0),
            PushNum(1),
            PushNum(0),
            ForPrep(0, 3),
            GetLocal(3),
            SetGlobal(0),
            ForLoop(0, -3),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 5.0],
            string_literals: vec!["x".into()],
            num_locals: 4,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test22() {
        let text = "a, b = 1";
        let code = vec![PushNum(0), PushNil, SetGlobal(1), SetGlobal(0), Return(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test23() {
        let text = "a, b = 1, 2";
        let code = vec![
            PushNum(0),
            PushNum(1),
            SetGlobal(1),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            ..Chunk::default()
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
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 2.0, 3.0],
            string_literals: vec!["a".to_string(), "b".to_string()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test25() {
        let text = "puts()";
        let code = vec![GetGlobal(0), Call(0, 0), Return(0)];
        let chunk = Chunk {
            code,
            string_literals: vec!["puts".to_string()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test26() {
        let text = "y = {x = 5,}";
        let code = vec![
            NewTable,
            PushNum(0),
            InitField(0, 1),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["y".into(), "x".into()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test27() {
        let text = "local x = t.x.y";
        let code = vec![
            GetGlobal(0),
            GetField(1),
            GetField(2),
            SetLocal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            string_literals: vec!["t".to_string(), "x".to_string(), "y".to_string()],
            num_locals: 1,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test28() {
        let text = "x = function () end";
        let code = vec![Closure(0), SetGlobal(0), Return(0)];
        let string_literals = vec!["x".into()];
        let nested = vec![Chunk {
            code: vec![Return(0)],
            ..Chunk::default()
        }];
        let chunk = Chunk {
            code,
            string_literals,
            nested,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test29() {
        let default = Chunk::default();
        let text = "x = function () local y = 7 end";
        let inner_chunk = Chunk {
            code: vec![PushNum(0), SetLocal(0), Return(0)],
            number_literals: vec![7.0],
            num_locals: 1,
            ..default
        };
        let outer_chunk = Chunk {
            code: vec![Closure(0), SetGlobal(0), Return(0)],
            string_literals: vec!["x".into()],
            nested: vec![inner_chunk],
            ..default
        };
        check_it(text, outer_chunk);
    }

    #[test]
    fn test30() {
        let text = "
        z = function () local z = 21 end
        x = function ()
            local y = function () end
            print(y)
        end";
        let z = Chunk {
            code: vec![PushNum(0), SetLocal(0), Return(0)],
            number_literals: vec![21.0],
            num_locals: 1,
            ..Chunk::default()
        };
        let y = Chunk {
            code: vec![Return(0)],
            ..Chunk::default()
        };
        let x = Chunk {
            code: vec![
                Closure(0),
                SetLocal(0),
                GetGlobal(0),
                GetLocal(0),
                Call(1, 0),
                Return(0),
            ],
            string_literals: vec!["print".into()],
            nested: vec![y],
            num_locals: 1,
            ..Chunk::default()
        };
        let outer_chunk = Chunk {
            code: vec![
                Closure(0),
                SetGlobal(0),
                Closure(1),
                SetGlobal(1),
                Return(0),
            ],
            nested: vec![z, x],
            string_literals: vec!["z".into(), "x".into()],
            ..Chunk::default()
        };
        check_it(text, outer_chunk);
    }

    #[test]
    fn test31() {
        let text = "local s = type(4)";
        let code = vec![GetGlobal(0), PushNum(0), Call(1, 1), SetLocal(0), Return(0)];
        let chunk = Chunk {
            code,
            num_locals: 1,
            number_literals: vec![4.0],
            string_literals: vec!["type".into()],
            ..Chunk::default()
        };
        check_it(text, chunk);
    }

    #[test]
    fn test32() {
        let text = "local type, print print(type(nil))";
        let code = vec![
            PushNil,
            PushNil,
            SetLocal(1),
            SetLocal(0),
            GetLocal(1),
            GetLocal(0),
            PushNil,
            Call(1, 1),
            Call(1, 0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            num_locals: 2,
            ..Chunk::default()
        };
        check_it(text, chunk);
    }
}
