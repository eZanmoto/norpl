// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

#[derive(Clone,Debug)]
pub enum Prog {
    Body{stmts: Vec<Stmt>},
}

#[derive(Clone,Debug)]
pub enum Stmt {
    Expr{expr: Expr},
}

#[derive(Clone,Debug)]
pub enum Expr {
    Int{n: i64},
    Call{func: String, args: Vec<Expr>},
}
