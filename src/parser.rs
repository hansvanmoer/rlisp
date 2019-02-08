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
/// lamda identifier
///
const LAMBDA_IDENT: &str = "lambda";

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
    BadExpr,
    ///
    /// Expected argument list, got somthing else
    ///
    BadArgList,
    ///
    /// Expected procedure body, got something else
    ///
    BadBody
}

///
/// A representation of a procedure define expression
///
#[derive(Debug, PartialEq)] 
pub struct ProcExpr{
    pub params: Vec<String>, 
    pub body: Box<Node>
}

impl ProcExpr{

    fn new(params: Vec<String>, body: Node) -> ProcExpr{
        ProcExpr{params, body: Box::new(body)}
    }
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
    /// Procedure
    ///
    Proc(ProcExpr),
    
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
    pub elements: Box<Vec<Node>>
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
/// A representation of a value define expression
///
#[derive(Debug, PartialEq)] 
pub struct ValueExpr{
    pub expr: Box<Node>
}

///
/// a representation of a define expression
///
#[derive(Debug, PartialEq)] 
enum DefineExpr{
    Simple(ValueExpr),
    Proc(ProcExpr)
}

///
/// a representation of a define form
///
#[derive(Debug, PartialEq)] 
pub struct DefineValue{
    name: String,
    expr: DefineExpr
}

impl DefineValue {

    ///
    /// Creates a new simple define form
    ///
    pub fn new_value(name: String, expr: Node) -> DefineValue{
        DefineValue{name, expr: DefineExpr::Simple(ValueExpr{expr: Box::new(expr)})}
    }

    ///
    /// Creates a new procedure define form
    ///
    pub fn new_proc(name: String, params: Vec<String>, expr: Node) -> DefineValue {
        DefineValue{name, expr: DefineExpr::Proc(ProcExpr::new(params, expr))}
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
    Literal(LiteralValue, Pos),

    ///
    /// An identifier
    ///
    Ident(String, Pos),
    
    ///
    /// A combination
    ///
    Comb(CombValue, Pos),

    ///
    /// A quote
    ///
    Quote(QuoteValue, Pos),

    ///
    /// Define form
    ///
    Define(DefineValue, Pos)
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
            ParsedExpr::End => Ok(Node::Literal(LiteralValue::Nil, self.pos)),
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

    fn lex(& mut self) -> Result<Token, InternalLexerError> {
        let token = self.lexer.lex();
        self.pos = self.lexer.token_pos();
        token
    }
    
    ///
    /// Returns a node, an end or an expression end value
    ///
    fn parse_node_or_end(& mut self) -> Result<ParsedExpr, Error> {
        match self.lex() {
            Ok(token) => {
                match token {
                    Token::End => Ok(ParsedExpr::End),
                    Token::ExprStart => Ok(ParsedExpr::Expr(self.parse_comb()?)),
                    Token::ExprEnd => Ok(ParsedExpr::ExprEnd),
                    Token::Ident(s) => Ok(ParsedExpr::Expr(Node::Ident(s, self.pos))),
                    Token::Number(n) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::Number(n), self.pos))),
                    Token::Boolean(b) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::Boolean(b), self.pos))),
                    Token::StringLit(s) => Ok(ParsedExpr::Expr(Node::Literal(LiteralValue::String(s), self.pos))),
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
    /// Parses and returns an expression
    ///
    fn parse_expr(& mut self) -> Result<Node, Error> {
        let node = self.parse_node()?;
        match node {
            Node::Define(_, _) => Err(Error::BadExpr),
            _ => Ok(node)
        }
    }

    fn parse_comb_tail(& mut self, node: Node, pos: Pos) -> Result<Node, Error> {
        let mut elems = vec!(node);
        loop {
            match self.parse_node_or_end()? {
                ParsedExpr::End => {
                    return Err(Error::BadEnd);
                },
                ParsedExpr::ExprEnd => {
                    return Ok(Node::Comb(CombValue::new(elems), pos));
                }
                ParsedExpr::Expr(node) => {
                    elems.push(node);
                }
            };
        }
    }
    
    ///
    /// Parses and returns a combination or a define
    ///
    fn parse_comb(& mut self) -> Result<Node, Error> {
        let pos = self.pos;
        match self.parse_node_or_end()? {
            ParsedExpr::End => {
                return Err(Error::BadEnd);
            },
            ParsedExpr::ExprEnd => {
                return Ok(Node::Literal(LiteralValue::Nil, pos))
            }
            ParsedExpr::Expr(node) => {
                match node {
                    Node::Ident(i, ipos) => {
                        if i == DEFINE_IDENT {
                            self.parse_define(pos)
                        }else if i == LAMBDA_IDENT {
                            self.parse_lambda(pos)
                        }else{
                            self.parse_comb_tail(Node::Ident(i, ipos), pos)
                        }
                    },
                    _ => self.parse_comb_tail(node, pos)
                }
            }
        }
    }

    ///
    /// parses a quote
    ///
    fn parse_quote(& mut self) -> Result<Node, Error> {
        let pos = self.pos;
        Ok(Node::Quote(QuoteValue::new(self.parse_node()?), pos))
    }
    
    ///
    /// parses a define
    ///
    fn parse_define(& mut self, pos: Pos) -> Result<Node, Error> {
        match self.parse_node()? {
            Node::Ident(ident, _) => Ok(Node::Define(DefineValue::new_value(ident, self.parse_expr()?), pos)),
            Node::Comb(mut comb, _) => {
                let mut param_elems = comb.elements.drain(0..);
                let ident = match param_elems.next() {
                    Some(n) => {
                        match n {
                            Node::Ident(i, _) => i,
                            _ => {
                                return Err(Error::BadIdent);
                            }
                        }
                    },    
                    None => {
                        return Err(Error::BadIdent);
                    }    
                };
                let mut params = Vec::new();
                for param in param_elems {
                    params.push( match param {
                        Node::Ident(i, _) => i,
                        _ => {
                            return Err(Error::BadIdent)
                        }
                    });
                }
                Ok(Node::Define(DefineValue::new_proc(ident, params, self.parse_expr()?), pos))
            }
            _=> Err(Error::BadIdent)
        }
    }

    fn parse_lambda_body(& mut self, params: Vec<String>, pos: Pos) -> Result<Node, Error>{
        match self.parse_node()? {
            Node::Comb(comb, body_pos) => {
                Ok(Node::Literal(LiteralValue::Proc(ProcExpr::new(params, Node::Comb(comb, body_pos))),pos))
            },
            _ => {
                return Err(Error::BadBody);
            }
        }
    }
    
    ///
    /// parses lambda
    ///
    fn parse_lambda(& mut self,  pos: Pos) -> Result<Node, Error> {
        match self.parse_node()? {
            Node::Comb(mut comb, _) => {
                let elems = comb.elements.drain(0..);
                let mut params = Vec::new();
                for elem in elems {
                    params.push(match elem {
                        Node::Ident(i, _) => i,
                        _ => {
                            return Err(Error::BadIdent);
                        }
                    });
                }
                self.parse_lambda_body(params, pos)
            },
            Node::Literal(LiteralValue::Nil, _) => {
                self.parse_lambda_body(Vec::new(), pos)
            }
            _ => Err(Error::BadArgList)
        }
    }

}

#[cfg(test)]
mod test{

    use super::*;
    
    #[test]
    fn boolean_literal(){
        let mut p = Parser::new("t");
        assert_eq!(Node::Literal(LiteralValue::Boolean(true), Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn numeric_literal(){
        let mut p = Parser::new("1.0");
        assert_eq!(Node::Literal(LiteralValue::Number(1.0), Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn string_literal(){
        let mut p = Parser::new("\"test\"");
        assert_eq!(Node::Literal(LiteralValue::String(String::from("test")), Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn identifier(){
        let mut p = Parser::new("test");
        assert_eq!(Node::Ident(String::from("test"), Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn nil(){
        let mut p = Parser::new("()");
        assert_eq!(Node::Literal(LiteralValue::Nil, Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn comb(){
        let mut p = Parser::new("(test 12.0 t)");
        assert_eq!(Node::Comb(CombValue::new(
            vec![
                Node::Ident(String::from("test"), Pos::new(0,1)),
                Node::Literal(LiteralValue::Number(12.0), Pos::new(0,6)),
                Node::Literal(LiteralValue::Boolean(true), Pos::new(0,11))
            ]
        ), Pos::new(0,0)), p.parse().unwrap());
    }

    #[test]
    fn define(){
        let mut p = Parser::new("(define abc t)");
        assert_eq!(
            Node::Define(DefineValue::new_value(String::from("abc"),Node::Literal(LiteralValue::Boolean(true), Pos::new(0,12))), Pos::new(0,0)),
            p.parse().unwrap()
        );
    }

    #[test]
    fn define_bad_ident(){
        let mut p = Parser::new("(define 12 t)");
        assert_eq!(Error::BadIdent, p.parse().unwrap_err());
    }

    #[test]
    fn define_bad_expr(){
        let mut p = Parser::new("(define abc (define tst t))");
        assert_eq!(Error::BadExpr, p.parse().unwrap_err());
    }

    #[test]
    fn lambda(){
        let mut p = Parser::new("(lambda (a b c) (13 t))");
        assert_eq!(
            Node::Literal(
                LiteralValue::Proc(
                    ProcExpr::new(
                        vec![String::from("a"), String::from("b"), String::from("c")],
                        Node::Comb(
                            CombValue::new(
                                vec![Node::Literal(LiteralValue::Number(13.0), Pos::new(0,17)), Node::Literal(LiteralValue::Boolean(true), Pos::new(0,20))],
                            ),
                            Pos::new(0,16)
                        )
                    )
                ),Pos::new(0,0)
            ),
            p.parse().unwrap()
        );
    }

    #[test]
    fn lambda_no_params(){
        let mut p = Parser::new("(lambda () (13 t))");
        assert_eq!(
            Node::Literal(
                LiteralValue::Proc(
                    ProcExpr::new(
                        Vec::new(),
                        Node::Comb(
                            CombValue::new(
                                vec![Node::Literal(LiteralValue::Number(13.0), Pos::new(0,12)), Node::Literal(LiteralValue::Boolean(true), Pos::new(0,15))],
                            ),
                            Pos::new(0,11)
                        )
                    )
                ),Pos::new(0,0)
            ),
            p.parse().unwrap()
        );
    }
    
    #[test]
    fn lambda_bad_arg_list(){
        let mut p = Parser::new("(lambda 12 (13 t))");
        assert_eq!(Error::BadArgList, p.parse().unwrap_err());
    }

    #[test]
    fn lambda_bad_ident(){
        let mut p = Parser::new("(lambda (12) (13 t))");
        assert_eq!(Error::BadIdent, p.parse().unwrap_err());
    }

    #[test]
    fn lambda_bad_body(){
        let mut p = Parser::new("(lambda (a) 12)");
        assert_eq!(Error::BadBody, p.parse().unwrap_err());
    }

}
