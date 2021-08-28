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
    Bool{b: bool},
    Int{n: i64},
    Str{s: String},

    Var{name: String},

    Op{op: Op, lhs: Box<Expr>, rhs: Box<Expr>},

    List{xs: Vec<ListItem>},
    Range{start: Box<Expr>, end: Box<Expr>},
    Index{expr: Box<Expr>, location: Box<Expr>},

    Object{props: Vec<PropItem>},

    Func{args: Vec<String>, stmts: Vec<Stmt>},
    Call{func: String, args: Vec<Expr>},
}

#[derive(Clone,Debug)]
pub struct ListItem {
    pub expr: Expr,
    pub is_spread: bool,
    pub is_unspread: bool,
}

#[derive(Clone,Debug)]
pub enum PropItem {
    Pair{name: String, value: Expr, name_is_str: bool},
    Spread{expr: Expr},
}

#[derive(Clone,Debug)]
pub enum Op {
    EQ,
    NE,
    GT,
    LT,

    Sum,
    Sub,
    Mul,
    Div,
    Mod,

    And,
    Or,
}
