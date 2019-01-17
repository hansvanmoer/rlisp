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
/// define identifier
///
const DEFINE_IDENT: &str = "define";

///
/// All errors associated to the parser
///
#[derive(Debug, PartialEq)]
pub enum Error{

    ///
    /// Wraps an error from the lexer
    ///
    LexerError(InternalLexerError),
    ///
    /// Unexpected end of stream
    ///
    BadEnd,
    ///
    /// Unexpected token
    ///
    BadToken,
    ///
    /// Expected identifier, got something else
    ///
    BadIdent,
    ///
    /// Expected expression, got something else
    ///
    BadExpr
}

///
/// Specifies all possible literal values
///
#[derive(Debug, PartialEq)]
pub enum LiteralValue{
    ///
    /// Boolean
    ///
    Boolean(bool),
    ///
    /// Number
    ///
    Number(f64),
    ///
    /// String
    ///
    String(String),
    ///
    /// Nil
    ///
    Nil
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
    Ident(String),
    
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
/// Helper enum for the parse_node_or_end function
///
enum ParsedExpr{
    ///
    /// End token encountered
    ///
    End,
    ///
    /// Normal node encountered
    ///
    Expr(Node),
    ///
    /// Expression end token encountered
    ///
    ExprEnd
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
        Parser{lexer: Lexer::new(data), pos : Pos::new(0,0)}
    }

    ///
    /// Parses a single node
    ///
    pub fn parse(& mut self) -> Result<Node, Error> {
        match self.parse_node_or_end()? {
            ParsedExpr::End => Ok(Node::Literal(LiteralValue::Nil)),
            ParsedExpr::Expr(node) => Ok(node),
            ParsedExpr::ExprEnd => Err(Error::BadEnd)
        }
    }

    ///
    /// Returns the position at the beginnning of the last attempted parse
    ///
    pub fn pos(& self) -> Pos {
        self.pos
    }

    ///
    /// Returns a node, an end or an expression end value
    ///
    fn parse_node_or_end(& mut self) -> Result<ParsedExpr, Error> {
        self.pos = self.lexer.token_pos();
        match self.lexer.lex() {
            Ok(token) => {
                match token {
                    Token::End => Ok(ParsedExpr::End),
                    Token::ExprStart => Ok(ParsedExpr::Expr(self.parse_comb()?)),
                    Token::ExprEnd => Ok(ParsedExpr::ExprEnd),
                    Token::Ident(s) => Ok(ParsedExpr::Expr(Node::Ident(s))),
                    Token::Number(n) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::Number(n)))),
                    Token::Boolean(b) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::Boolean(b)))),
                    Token::StringLit(s) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::String(s)))),
                    Token::Quote => Ok(ParsedExpr::Expr(self.parse_quote()?))
                }
            },
            Err(e) => Err(Error::LexerError(e))
         }
    }

    ///
    /// Parses and returns a parsed node
    ///
    fn parse_node(& mut self) -> Result<Node, Error> {
        match self.parse_node_or_end()? {
            ParsedExpr::End => Err(Error::BadEnd),
            ParsedExpr::Expr(node) => Ok(node),
            ParsedExpr::ExprEnd => Err(Error::BadToken)
        }
    }

    ///
    /// Parses and returns an identifier
    ///
    fn parse_ident(& mut self) -> Result<String, Error> {
        let node = self.parse_node()?;
        match node {
            Node::Ident(i) => Ok(i),
            _ => Err(Error::BadIdent)
        }
    }

    ///
    /// Parses and returns an expression
    ///
    fn parse_expr(& mut self) -> Result<Node, Error> {
        let node = self.parse_node()?;
        match node {
            Node::Define(_) => Err(Error::BadExpr),
            _ => Ok(node)
        }
    }

    ///
    /// Parses and returns a combination or a define
    ///
    fn parse_comb(& mut self) -> Result<Node, Error> {
        let mut elements = Vec::new();
        match self.parse_node_or_end()? {
            ParsedExpr::End => {
                return Err(Error::BadEnd);
            },
            ParsedExpr::Expr(node) => {
                match & node {
                    & Node::Ident(ref i) => {
                        if i == DEFINE_IDENT {
                            return self.parse_define();
                        } else {
                            elements.push(node);
                        }
                    },
                    _ => {
                        elements.push(node);
                    }
                }
            },
            ParsedExpr::ExprEnd => {
                return Ok(Node::Literal(LiteralValue::Nil))
            }
        }

        loop {
            match self.parse_node_or_end()? {
                ParsedExpr::End => {
                    return Err(Error::BadEnd);
                },
                ParsedExpr::Expr(node) => {
                    elements.push(node);
                },
                ParsedExpr::ExprEnd => {
                    return Ok(Node::Comb(CombValue::new(elements)))
                }
            }
        }
    }

    ///
    /// parses a quote
    ///
    fn parse_quote(& mut self) -> Result<Node, Error> {
        Ok(Node::Quote(QuoteValue::new(self.parse_node()?)))
    }

    ///
    /// parses a define
    ///
    fn parse_define(& mut self) -> Result<Node, Error> {
        Ok(Node::Define(DefineValue::new(self.parse_ident()?, self.parse_expr()?)))
    }
}
