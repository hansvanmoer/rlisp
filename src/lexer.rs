//
// This file is part of rlisp.
//
// rlisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// rlisp is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with rlisp.  If not, see <https://www.gnu.org/licenses/>.
//


///
/// Contains the lexer and all associated types
///

use std::str::Chars;
use std::iter::Peekable;

use crate::pos::Pos;

///
/// All errors related to the lexer
///
#[derive(PartialEq)]
#[derive(Debug)]
pub enum Error{
    /// Unexpected character
    BadChar,
    /// Unparseable numeric literal
    BadNumber,
    ///
    /// Unterminated string literal
    ///
    UntermString,
    ///
    /// Empty escape
    ///
    EmptyEscape,
    ///
    /// Bad escape
    ///
    BadEscape
}

///
/// All tokens returned by the lexer
///
#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token{
    /// 
    /// Signifies the end of the stream
    ///
    End,
    ///
    /// Signifies the start of an s-expression
    ///
    ExprStart,
    ///
    /// Signifies the end of an s-expression
    ///
    ExprEnd,
    ///
    /// Signifies an identifier
    ///
    Ident(String),
    ///
    /// Number
    ///
    Number(f64),
    /// 
    /// String literal
    ///
    StringLit(String),
    ///
    /// Boolean literal
    ///
    Boolean(bool),
    ///
    /// Quote
    ///
    Quote
}


/// 
/// Start expression character
///
const EXPR_START: char = '(';

///
/// End expression character
///
const EXPR_END: char = ')';

///
/// Newline character
///
const NEWLINE: char = '\n';

///
/// Carriage return character
///
const CARRIAGE_RETURN: char = '\r';

///
/// Space character
///
const SPACE: char =  ' ';

///
/// Tab character
///
const TAB: char = '\t';

///
/// Dot character
///
const DOT: char = '.';

///
/// Plus character
///
const PLUS: char = '+';

///
/// Minus character
///
const MINUS: char = '-';

///
/// Lowercase exponent
///
const EXP_LC: char = 'e';

///
/// Uppercase exponent
///
const EXP_UC: char = 'E';

///
/// String delimiter
///
const STRING_DELIM: char = '\"';

///
/// String escape
///
const STRING_ESCAPE: char = '\\';

///
/// Comment start
///
const COMMENT_START: char = ';';

///
/// Boolean true character
///
const TRUE_LITERAL: char = 't';

///
/// Boolean false character
///
const FALSE_LITERAL: char = 'f';

///
/// Quote
///
const QUOTE: char = '\'';

///
/// The lexer
///
pub struct Lexer<'a>{
    input: Peekable<Chars<'a>>,
    line: i32,
    col: i32,
    last_line: i32,
    last_col: i32
}

impl<'a> Lexer<'a>{

    ///
    /// Creates a new lexer from the supplies string slice
    ///
    pub fn new(data: &'a str) -> Lexer<'a>{
        Lexer{input: data.chars().peekable(), line: 0, col: 0, last_line: 0, last_col: 0}
    }

    ///
    /// Returns the current position of the lexer as a (line, column) tuple
    /// Note that lines and columns start at zero.
    ///
    pub fn pos(& self) -> Pos{
        Pos::new(self.line, self.col)
    }

    ///
    /// Returns the position of the lexer at the start of the last attempted token as a (line, column) tuple
    /// Note that lines and columns start at zero.
    ///
    pub fn token_pos(& self) -> Pos {
        Pos::new(self.last_line, self.last_col)
    }
    
    ///
    /// Returns the next character in the stream
    ///
    fn next(&mut self) -> Option<char>{
        match self.input.next() {
            Some(c) => {
                match c {
                    NEWLINE | CARRIAGE_RETURN => {
                        self.line = self.line + 1;
                        self.col = 0;
                        Some(c)
                    },
                    _ => {
                        self.col = self.col + 1;
                        Some(c)
                    }
                }
            },
            None => None
        }
    }

    ///
    /// Skips the next character and assume it is not a line break
    ///
    fn skip_nb(&mut self){
        self.input.next();
        self.col = self.col + 1;
    }
    
    ///
    /// Skips whitespace
    /// returns true if further non-whitespace characters remain on the stream, false otherwise
    /// 
    fn skip_whitespace(&mut self) -> bool{
        loop {
            match self.input.peek() {
                Some(c) => {
                    match c {
                        &NEWLINE | &CARRIAGE_RETURN => {
                            self.line = self.line + 1;
                            self.col = 0;
                        },
                        &SPACE | &TAB => {
                            self.col = self.col + 1;
                        },
                        _ => {
                            return true;
                        }
                    }
                },
                None => {
                    return false;
                }
            }
            self.input.next();
        }
    }

    ///
    /// Tests whether the character is an identifier delimiter
    ///
    fn is_identifier_delim(c : &char) -> bool{
        match c {
            &EXPR_START | &EXPR_END | &NEWLINE | &TAB | &SPACE | &CARRIAGE_RETURN | &COMMENT_START=> {
                true
            },
            _ => false
        }
    }

    ///
    /// Tests whether the character is a numeric symbol
    ///
    fn is_numeric(c: &char) -> bool{
        match c {
            &'0' | &'1' | &'2' | &'3' | &'4' | &'5' | &'6' | &'7' | &'8' | &'9' | &DOT | &PLUS | &MINUS | &EXP_LC | &EXP_UC => true,
            _ => false
        }
    }

    ///
    /// Lexes an expression start token
    ///
    fn lex_expr_start(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        Ok(Token::ExprStart)
    }

    ///
    /// Lexes an expression end token
    ///
    fn lex_expr_end(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        Ok(Token::ExprEnd)
    }

    ///
    /// Attempts to lex a number
    ///
    fn lex_number(&mut self) -> Result<f64, Error> {
        let mut s = String::new();
        loop{
            match self.input.peek() {
                Some(c) => {
                    if Lexer::is_numeric(c) {
                        s.push(*c);
                        self.skip_nb();
                    }else if Lexer::is_identifier_delim(c) {
                        break;
                    }else {
                        return Err(Error::BadChar);
                    }
                },
                None => {
                    break;
                }
            }
        }
        match s.parse::<f64>() {
            Ok(n) => Ok(n),
            Err(_e) => Err(Error::BadNumber)
        }
    }

    ///
    /// Attempts to lex either the '+' identifier or a number
    ///
    fn lex_plus(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        match self.input.peek() {
            Some(n) => {
                if Lexer::is_identifier_delim(n) {
                    Ok(Token::Ident(String::from("+")))
                }else if Lexer::is_numeric(n) {
                    Ok(Token::Number(self.lex_number()?))
                }else{
                    Err(Error::BadChar)
                }
            },
            None => {
                Ok(Token::Ident(String::from("+")))
            }
                
        }
    }

    ///
    /// Attempts to lex either the '-' identifier or a number
    ///
    fn lex_minus(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        match self.input.peek() {
            Some(n) => {
                if Lexer::is_identifier_delim(n) {
                    Ok(Token::Ident(String::from("-")))
                }else if Lexer::is_numeric(n) {
                    Ok(Token::Number( - self.lex_number()?))
                }else{
                    Err(Error::BadChar)
                }
            },
            None => {
                Ok(Token::Ident(String::from("-")))
            }
        }
    }

    ///
    /// Attempts to lex an escape
    ///
    fn lex_escape(&mut self) -> Result<char, Error> {
        match self.input.peek() {
            Some(n) => {
                match n {
                    &STRING_DELIM | &STRING_ESCAPE => {
                        let r = Ok(*n);
                        self.skip_nb();
                        r
                    },
                    &'n' => {
                        self.skip_nb();
                        Ok('\n')
                    },
                    &'r' => {
                        self.skip_nb();
                        Ok('\r')
                    }
                    &'t' => {
                        self.skip_nb();
                        Ok('\t')
                    }
                    _ => {
                        Err(Error::BadEscape)
                    }
                }
            },
            None => {
                Err(Error::EmptyEscape) 
            }
        }
    }

    ///
    /// Attempts to lex a string literal
    ///
    fn lex_string(&mut self) -> Result<Token, Error>{
        self.skip_nb();
        let mut s = String::new();
        loop {
            match self.next() {
                Some(c) => {
                    match c {
                        STRING_ESCAPE => {
                            match self.lex_escape() {
                                Ok(e) => {
                                    s.push(e);
                                },
                                Err(e) => {
                                    break Err(e)
                                }
                            }
                        },
                        STRING_DELIM => {
                            break Ok(Token::StringLit(s))
                        },
                        _ => {
                            s.push(c);
                        }
                    }
                },
                None => {
                    break Err(Error::UntermString)
                }
            }
        }
    }

    ///
    /// Attempts to lex an identifierr
    ///
    fn lex_identifier(&mut self) -> Result<Token, Error> {
        self.lex_identifier_with(String::new())
    }

    ///
    /// Attempts to lex an identifier using the supplied buffer 
    ///
    fn lex_identifier_with(&mut self, mut s: String) -> Result<Token, Error> {
        loop{
            match self.input.peek() {
                Some(c) => {
                    if Lexer::is_identifier_delim(c) {
                        break Ok(Token::Ident(s));
                    }else {
                        s.push(*c);
                        self.skip_nb();
                    }
                },
                None => {
                    break Ok(Token::Ident(s));
                }
            }
        }
    }

    ///
    /// Lexes a comment and ignores it
    ///
    fn lex_comment(&mut self){
        self.skip_nb();
        loop{
            match self.input.peek() {
                Some(c) => {
                    match c {
                        &CARRIAGE_RETURN | &NEWLINE => {
                            self.skip_nb();
                            break;
                        },
                        _ => {}
                    }
                },
                None => {
                    break;
                }
            }
            self.skip_nb();
        }
    }

    ///
    /// Lexes either a boolean true literal or an identifier
    ///
    fn lex_true(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        match self.input.peek() {
            Some(c) => {
                if Lexer::is_identifier_delim(c) {
                    Ok(Token::Boolean(true))
                }else{
                    self.lex_identifier_with(String::from("t"))
                }
            },
            None => Ok(Token::Boolean(true))
        }
    }

    ///
    /// Lexes either a boolean false literal or an identifier
    ///
    fn lex_false(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        match self.input.peek() {
            Some(c) => {
                if Lexer::is_identifier_delim(c) {
                    Ok(Token::Boolean(false))
                }else{
                    self.lex_identifier_with(String::from("f"))
                }
            },
            None => Ok(Token::Boolean(false))
        }
    }

    ///
    /// Lexes a quote
    ///
    fn lex_quote(&mut self) -> Result<Token, Error> {
        self.skip_nb();
        Ok(Token::Quote)
    }
        
    ///
    /// Produces the next token or an error
    /// If the stream has reached the end, the lexer will continue to produce 'End' tokens
    ///
    pub fn lex(&mut self) -> Result<Token, Error>{
        loop{
            if self.skip_whitespace() {
                self.last_line = self.line;
                self.last_col = self.col;
                match self.input.peek() {
                    Some(c)  => {
                        match c {
                            &EXPR_START => {
                                return self.lex_expr_start();
                            },
                            &EXPR_END => {
                                return self.lex_expr_end();
                            },
                            &PLUS => {
                                return self.lex_plus();
                            },
                            &MINUS => {
                                return self.lex_minus();
                            },
                            &'0' | &'1' | &'2' | &'3' | &'4' | &'5' | &'6' | &'7' | &'8' | &'9' | &DOT => {
                                return Ok(Token::Number(self.lex_number()?));
                            },
                            &STRING_DELIM => {
                                return self.lex_string();
                            },
                            &COMMENT_START => {
                                self.lex_comment();
                            },
                            &TRUE_LITERAL => {
                                return self.lex_true();
                            },
                            &FALSE_LITERAL => {
                                return self.lex_false();
                            },
                            &QUOTE => {
                                return self.lex_quote();
                            },
                            _ => {
                                return self.lex_identifier();
                            }
                        }
                    },
                    None => {
                        return Ok(Token::End)
                    }
                }
            }else{
                return Ok(Token::End);
            }
        }
    }
    
}

#[cfg(test)]
mod tests{

    use super::*;
    
    #[test]
    fn empty(){
        let mut lexer = Lexer::new("");
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap()); // lexer should continue to return 'End'
        assert_eq!(Pos::new(0,0), lexer.pos());
    }

    #[test]
    fn nested_empty_expressions(){
        let mut lexer = Lexer::new("(())");
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Token::ExprEnd, lexer.lex().unwrap());
        assert_eq!(Token::ExprEnd, lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Pos::new(0,4), lexer.pos());
    }

    #[test]
    fn whitespace_skipper(){
        let mut lexer = Lexer::new(" \t(\n   )\r");
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Pos::new(0, 3), lexer.pos());
        assert_eq!(Token::ExprEnd, lexer.lex().unwrap());
        assert_eq!(Pos::new(1, 4), lexer.pos());
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Pos::new(2, 0), lexer.pos());
    }

    #[test]
    fn plus(){
        let mut lexer = Lexer::new("+)");
        match lexer.lex().unwrap() {
            Token::Ident(s) => {
                assert_eq!(String::from("+"), s);
            },
            _ => {
                panic!("expected '+' token");
            }
        }
        assert_eq!(Token::ExprEnd, lexer.lex().unwrap());
    }

    #[test]
    fn terminal_plus(){
        let mut lexer = Lexer::new("+");
        match lexer.lex().unwrap() {
            Token::Ident(s) => {
                assert_eq!(String::from("+"), s);
            },
            _ => {
                panic!("expected '+' token");
            }
        }
        assert_eq!(Token::End, lexer.lex().unwrap());
    }
    
    #[test]
    fn bad_plus(){
        let mut lexer = Lexer::new("+a)");
        assert_eq!(Error::BadChar, lexer.lex().unwrap_err());
    }

    #[test]
    fn plus_number(){
        let mut lexer = Lexer::new("+12.3e10");
        assert_eq!(Token::Number(12.3e10), lexer.lex().unwrap());
    }
    
    #[test]
    fn minus(){
        let mut lexer = Lexer::new("-)");
        match lexer.lex().unwrap() {
            Token::Ident(s) => {
                assert_eq!(String::from("-"), s);
            },
            _ => {
                panic!("expected '-' token");
            }
        }
        assert_eq!(Token::ExprEnd, lexer.lex().unwrap());    
    }

    #[test]
    fn terminal_minus(){
        let mut lexer = Lexer::new("-");
        match lexer.lex().unwrap() {
            Token::Ident(s) => {
                assert_eq!(String::from("-"), s);
            },
            _ => {
                panic!("expected '+' token");
            }
        }
        assert_eq!(Token::End, lexer.lex().unwrap());
    }
    
    #[test]
    fn bad_minus(){
        let mut lexer = Lexer::new("-a)");
        assert_eq!(Error::BadChar, lexer.lex().unwrap_err());
    }

    #[test]
    fn minus_number(){
        let mut lexer = Lexer::new("-12.3e10");
        assert_eq!(Token::Number(-12.3e10), lexer.lex().unwrap());
    }

    #[test]
    fn number(){
        let mut lexer = Lexer::new("12.3e10");
        assert_eq!(Token::Number(12.3e10), lexer.lex().unwrap());
    }

    #[test]
    fn dot_number(){
        let mut lexer = Lexer::new(".13e10");
        assert_eq!(Token::Number(0.13e10), lexer.lex().unwrap());
    }

    #[test]
    fn invalid_number(){
        let mut lexer = Lexer::new(".1.3e10");
        assert_eq!(Error::BadNumber, lexer.lex().unwrap_err());
    }

    #[test]
    fn unterminated_number(){
        let mut lexer = Lexer::new(".1.3e10ab");
        assert_eq!(Error::BadChar, lexer.lex().unwrap_err());
    }

    #[test]
    fn string(){
        let mut lexer = Lexer::new("\"test test\"");
        assert_eq!(Token::StringLit(String::from("test test")), lexer.lex().unwrap());
    }

    #[test]
    fn string_with_escapes(){
        let mut lexer = Lexer::new("\"\\ntest\\r\\t \\\"test\\\\\"");
        assert_eq!(Token::StringLit(String::from("\ntest\r\t \"test\\")), lexer.lex().unwrap());
    }

    #[test]
    fn unterminated_string(){
        let mut lexer = Lexer::new("\"test test");
        assert_eq!(Error::UntermString, lexer.lex().unwrap_err());
    }

    #[test]
    fn bad_string_escape(){
        let mut lexer = Lexer::new("\"test \\atest\"");
        assert_eq!(Error::BadEscape, lexer.lex().unwrap_err());
    }

    #[test]
    fn empty_string_escape(){
        let mut lexer = Lexer::new("\"test \\");
        assert_eq!(Error::EmptyEscape, lexer.lex().unwrap_err());
    }

    #[test]
    fn identifier(){
        let mut lexer = Lexer::new("test");
        assert_eq!(Token::Ident(String::from("test")), lexer.lex().unwrap());
    }

    #[test]
    fn bad_plus_identifier(){
        let mut lexer = Lexer::new("+test");
        assert_eq!(Error::BadChar, lexer.lex().unwrap_err());
    }

    #[test]
    fn bad_minus_identifier(){
        let mut lexer = Lexer::new("-test");
        assert_eq!(Error::BadChar, lexer.lex().unwrap_err());
    }

    #[test]
    fn true_literal(){
        let mut lexer = Lexer::new("t t( tx t");
        assert_eq!(Token::Boolean(true), lexer.lex().unwrap());
        assert_eq!(Token::Boolean(true), lexer.lex().unwrap()); 
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Token::Ident(String::from("tx")), lexer.lex().unwrap());
        assert_eq!(Token::Boolean(true), lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap());
    }

    #[test]
    fn false_literal(){
        let mut lexer = Lexer::new("f f( fx f");
        assert_eq!(Token::Boolean(false), lexer.lex().unwrap());
        assert_eq!(Token::Boolean(false), lexer.lex().unwrap()); 
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Token::Ident(String::from("fx")), lexer.lex().unwrap());
        assert_eq!(Token::Boolean(false), lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap());
    }
    
    #[test]
    fn comments(){
        let mut lexer = Lexer::new(";comment \n(;;second comment\n test;third comment;;\n3.12;last comment\r");
        assert_eq!(Pos::new(0,0), lexer.token_pos());
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Token::Ident(String::from("test")), lexer.lex().unwrap());
        assert_eq!(Token::Number(3.12), lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Token::End, lexer.lex().unwrap());
        
    }

    #[test]
    fn quote(){
       let mut lexer = Lexer::new("'abc");
        assert_eq!(Token::Quote, lexer.lex().unwrap());
        assert_eq!(Token::Ident(String::from("abc")), lexer.lex().unwrap());
    }
    
    #[test]
    fn token_pos(){
        let mut lexer = Lexer::new("( test\n3.12");
        assert_eq!(Pos::new(0,0), lexer.token_pos());
        assert_eq!(Token::ExprStart, lexer.lex().unwrap());
        assert_eq!(Pos::new(0,0), lexer.token_pos());
        assert_eq!(Token::Ident(String::from("test")), lexer.lex().unwrap());
        assert_eq!(Pos::new(0,2), lexer.token_pos());
        assert_eq!(Token::Number(3.12), lexer.lex().unwrap());
        assert_eq!(Pos::new(1,0), lexer.token_pos());
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Pos::new(1,0), lexer.token_pos());
        assert_eq!(Token::End, lexer.lex().unwrap());
        assert_eq!(Pos::new(1,0), lexer.token_pos());
    }
    
}
