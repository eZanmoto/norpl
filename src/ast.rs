// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

#[derive(Clone,Debug)]
pub enum Prog {
    Body{stmts: Vec<Stmt>},
}

#[derive(Clone,Debug)]
pub enum Stmt {
    Assign{name: String, rhs: Expr},
    OpAssign{name: String, op: Op, rhs: Expr},

    If{cond: Expr, if_stmts: Vec<Stmt>, else_stmts: Option<Vec<Stmt>>},
    While{cond: Expr, stmts: Vec<Stmt>},

    Expr{expr: Expr},
}

#[derive(Clone,Debug)]
pub enum Expr {
    Int{n: i64},
    Str{s: String},

    Var{name: String},

    Op{lhs: Box<Expr>, rhs: Box<Expr>},

    List{xs: Vec<ListItem>},
    Call{func: String, args: Vec<Expr>},
}

#[derive(Clone,Debug)]
pub struct ListItem {
    pub expr: Expr,
    pub is_spread: bool,
}

#[derive(Clone,Debug)]
pub enum Op {
    LT,
    Plus,
}
