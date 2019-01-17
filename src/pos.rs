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


#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Pos{
    pub line: i32,
    pub col: i32
}

impl Pos{

    pub fn new(line: i32, col: i32) -> Pos {
        Pos{line,col}
    }
    
}

impl std::fmt::Display for Pos{

    fn fmt(& self, f: & mut std::fmt::Formatter) -> std::fmt::Result{
        write!(f, "line {}, column {}", self.line, self.col)
    }
}
