// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

#[derive(Clone,Debug)]
pub enum Prog {
    Body{stmts: Vec<Stmt>},
}

#[derive(Clone,Debug)]
pub enum Stmt {
    Declare{lhs: Expr, rhs: Expr},
    Assign{lhs: Expr, rhs: Expr},
    OpAssign{name: String, op: Op, rhs: Expr},

    If{cond: Expr, if_stmts: Vec<Stmt>, else_stmts: Option<Vec<Stmt>>},
    While{cond: Expr, stmts: Vec<Stmt>},
    For{lhs: Expr, iter: Expr, stmts: Vec<Stmt>},

    Func{name: String, args: Vec<String>, stmts: Vec<Stmt>},
    Return{expr: Expr},

    Expr{expr: Expr},
}

#[derive(Clone,Debug)]
pub enum Expr {
    Int{n: i64},
    Str{s: String},

    Var{name: String},

    Op{op: Op, lhs: Box<Expr>, rhs: Box<Expr>},

    List{xs: Vec<ListItem>},
    Range{start: Box<Expr>, end: Box<Expr>},
    Index{expr: Box<Expr>, n: Box<Expr>},

    Object{props: Vec<Prop>},

    Call{func: String, args: Vec<Expr>},
}

#[derive(Clone,Debug)]
pub struct ListItem {
    pub expr: Expr,
    pub is_spread: bool,
    pub is_unspread: bool,
}

#[derive(Clone,Debug)]
pub struct Prop {
    pub name: String,
    pub value: Expr,
}

#[derive(Clone,Debug)]
pub enum Op {
    LT,

    Sum,

    Div,
}
