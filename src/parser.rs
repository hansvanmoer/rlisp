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

use crate::lexer::Error as InternalLexerError;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::pos::Pos;

///
/// Specifies the parser and associated types
///

///
/// All errors associated to the parser
///
#[derive(Debug, PartialEq)]
pub enum Error{

    ///
    /// Wraps an error from the lexer
    ///
    LexerError(InternalLexerError)
    
}

///
/// Specifies all possible literal values
///
#[derive(Debug, PartialEq)]
pub enum LiteralValue{
    Boolean(bool),
    Number(f64),
    String(String)
}

///
/// A representation of a list
///
#[derive(Debug, PartialEq)]
pub struct CombValue{
    elements: Box<Vec<Node>>
}

impl CombValue {

    ///
    /// Creates a new combination with the specified elements
    ///
    pub fn new(elements: Vec<Node>) -> CombValue {
        CombValue{elements: Box::new(elements)}
    }
    
}

///
/// A representation of a quoted value
///
#[derive(Debug, PartialEq)]
pub struct QuoteValue{
    value: Box<Node>
}

impl QuoteValue{

    ///
    /// Creates a new quoted value
    ///
    pub fn new(value: Node) -> QuoteValue {
        QuoteValue{value: Box::new(value)}
    }
    
}

///
/// a representation of a define form
///
#[derive(Debug, PartialEq)] 
pub struct DefineValue{
    name: String,
    value: Box<Node>
}

impl DefineValue {

    ///
    /// Creates a new define form
    ///
    pub fn new(name: String, value: Node) -> DefineValue{
        DefineValue{name, value: Box::new(value)}
    }
}

/// 
/// All possible types of syntax nodes
///
#[derive(Debug, PartialEq)]
pub enum Node{
    ///
    /// A literal
    ///
    Literal(LiteralValue),

    ///
    /// An identifier
    ///
    Identifier(String),
    
    ///
    /// A combination
    ///
    Comb(CombValue),

    ///
    /// A quote
    ///
    Quote(QuoteValue),

    ///
    /// Define form
    ///
    Define(DefineValue)
}

///
/// The parser
///
pub struct Parser<'a>{
    lexer: Lexer<'a>,
    pos: Pos
}

impl<'a> Parser<'a> {
    
    ///
    /// Creates a new parser with the specified slice
    ///
    pub fn new(data: &'a str) -> Parser<'a> {
        Parser{lexer: Lexer::new(data), pos: Pos::new(0,0)}
    }
    
}
