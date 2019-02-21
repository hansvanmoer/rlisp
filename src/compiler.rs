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
/// Defines the compiler
///

use std::collections::HashMap;

use crate::parser::DefineExpr;
use crate::parser::DefineValue;
use crate::parser::LiteralValue;
use crate::parser::Node;
use crate::pos::Pos;
use crate::value::Value;

///
/// Compiler error
///
#[derive(Debug, PartialEq)]
enum Error{
    UnknownSymbol(String),
    NotImpl
}

///
/// Represents an instruction
///
#[derive(Debug, PartialEq)]
enum Ins{
    ///
    /// Creates a value
    /// (destination register, value)
    ///
    Create(usize, Value),

    ///
    /// Stores a value as a global
    /// (dest data address, src register)
    ///
    StoreGlobal(usize, usize),

    ///
    /// Stores a value as a global
    /// (dest, src data address)
    ///
    LoadGlobal(usize, usize),
    
    ///
    /// stores the contents of a register into the result register
    /// 
    ///
    StoreResult(usize)
}

///
/// A compilation unit
///
#[derive(Debug, PartialEq)]
struct Unit{
    symbols: HashMap<String, usize>,
    ins: Vec<Ins>,
    start: usize
}

impl Unit{

    fn new() -> Unit{
        Unit{symbols: HashMap::new(), ins: Vec::new(), start: 0}
    }
    
}

struct Compiler{
    unit: Unit,
    gpr: Vec<bool>,
    next_data: usize
}

impl Compiler{

    fn new() -> Compiler{
        Compiler{unit: Unit::new(), gpr: Vec::new(), next_data: 0}
    }

    fn acquire_gpr(& mut self) -> usize{
        for i in 0..self.gpr.len() {
            if !self.gpr[i] {
                self.gpr[i] = true;
                return i;
            }
        }
        let i = self.gpr.len();
        self.gpr.push(true);
        i
    }

    fn release_gpr(& mut self, i: usize){
        self.gpr[i] = false;
    }

    fn add(& mut self, ins: Ins) {
        self.unit.ins.push(ins);
    }

    fn compile_literal(&  mut self, dest: usize, literal: & LiteralValue) -> Option<Error> {
        self.add(Ins::Create(dest, match literal {
            LiteralValue::Boolean(b) => Value::Boolean(*b),
            LiteralValue::Number(n) => Value::Number(*n),
            LiteralValue::String(s) => Value::String(s.clone()),
            _ => {
                return Some(Error::NotImpl);
            }
        }));
        None
    } 

    fn compile_define(& mut self, def: & DefineValue) -> Option<Error> {
        let addr = self.next_data;
        self.next_data = self.next_data + 1;
        let dest = self.acquire_gpr();
        match & def.expr {
            DefineExpr::Simple(expr) => {
                match self.compile_expr(dest, &expr.expr) {
                    Some(e) => Some(e),
                    None => {
                        self.unit.symbols.insert(def.name.clone(), dest);
                        self.add(Ins::StoreGlobal(addr, dest));
                        self.release_gpr(dest);
                        None
                    }
                }
            },
            DefineExpr::Proc(_) => Some(Error::NotImpl)
        }
    }

    fn compile_ident(& mut self, dest: usize, ident: & String) -> Option<Error> {
        match self.unit.symbols.get(ident) {
            None => {
                Some(Error::UnknownSymbol(ident.clone()))
            },
            Some(addr) => {
                self.add(Ins::LoadGlobal(dest, *addr));
                None
            }
        }
    }
    
    fn compile_expr(& mut self, dest: usize, node: & Node) -> Option<Error> {
        match node {
            Node::Literal(l, _) => {
                self.compile_literal(dest, l)
            },
            _ => Some(Error::NotImpl)
        }
    }

    fn compile_result(& mut self, dest: usize, result: Option<Error>) -> Option<Error>{
        match result {
            Some(e) => Some(e),
            None => {
                self.add(Ins::StoreResult(dest));
                self.release_gpr(dest);
                None
            }
        }
    }
    
    pub fn compile(& mut self, node: & Node) -> Option<Error> {
        match node {
            Node::Literal(l, _) => {
                let dest = self.acquire_gpr();
                let result = self.compile_literal(dest, l);
                self.compile_result(dest, result)
            },
            Node::Define(def, _) => {
                self.compile_define(def)
            },
            Node::Ident(i, _) => {
                let dest = self.acquire_gpr();
                let result = self.compile_ident(dest, i);
                self.compile_result(dest, result)
            }
            _ => Some(Error::NotImpl)
        }
    }

    fn create_unit(& mut self) -> Unit{
        let mut result = Unit::new();
        std::mem::swap(& mut self.unit, & mut result);
        result
    }

}

#[cfg(test)]
mod tests{

    use super::*;
    
    #[test]
    fn literal_boolean(){
        let node = Node::Literal(LiteralValue::Boolean(true), Pos::new(0,0));
        let mut c = Compiler::new();
        c.compile(& node);
        assert_eq!(c.create_unit(), Unit{
            ins: vec![
                Ins::Create(0, Value::Boolean(true)),
                Ins::StoreResult(0)
            ],
            symbols: HashMap::new(),
            start: 0
        });
    }
    
    #[test]
    fn literal_string(){
        let node = Node::Literal(LiteralValue::String(String::from("abc")), Pos::new(0,0));
        let mut c = Compiler::new();
        c.compile(& node);
        assert_eq!(c.create_unit(), Unit{
            ins: vec![
                Ins::Create(0, Value::String(String::from("abc"))),
                Ins::StoreResult(0)
            ],
            symbols: HashMap::new(),
            start: 0
        });
    }
    
    #[test]
    fn literal_number(){
        let node = Node::Literal(LiteralValue::Number(1.0), Pos::new(0,0));
        let mut c = Compiler::new();
        c.compile(& node);
        assert_eq!(c.create_unit(), Unit{
            ins: vec![
                Ins::Create(0, Value::Number(1.0)),
                Ins::StoreResult(0)
            ],
            symbols: HashMap::new(),
            start: 0
        });
    }

    #[test]
    fn define_value(){
        let node = Node::Define(
            DefineValue::new_value(
                String::from("a"),
                Node::Literal(LiteralValue::Number(1.0), Pos::new(0,1))
            ),
            Pos::new(0,0)
        );

        let mut s = HashMap::new();
        s.insert(String::from("a"), 0);
        
        let mut c = Compiler::new();
        c.compile(& node);
        assert_eq!(c.create_unit(), Unit{
            ins: vec![
                Ins::Create(0, Value::Number(1.0)),
                Ins::StoreGlobal(0, 0)
            ],
            symbols: s,
            start: 0
        });
    }

    #[test]
    fn load_defined_value(){
        let node1 = Node::Define(
            DefineValue::new_value(
                String::from("a"),
                Node::Literal(LiteralValue::Number(1.0), Pos::new(0,1))
            ),
            Pos::new(0,0)
        );

        let node2 = Node::Ident(String::from("a"), Pos::new(1,0));

        let mut s = HashMap::new();
        s.insert(String::from("a"), 0);
        
        let mut c = Compiler::new();
        c.compile(& node1);
        c.compile(& node2);
        assert_eq!(c.create_unit(), Unit{
            ins: vec![
                Ins::Create(0, Value::Number(1.0)),
                Ins::StoreGlobal(0, 0),
                Ins::LoadGlobal(0, 0),
                Ins::StoreResult(0)
            ],
            symbols: s,
            start: 0
        });
    }
    
}
