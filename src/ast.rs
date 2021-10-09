// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

#[derive(Clone,Debug)]
pub enum Prog {
    Body{stmts: Vec<Stmt>},
}

#[derive(Clone,Debug)]
pub enum Stmt {
    Declare{lhs: Expr, rhs: Expr, dt: DeclarationType},
    Assign{lhs: Expr, rhs: Expr},
    OpAssign{name: String, op: BinaryOp, rhs: Expr},

    If{cond: Expr, if_stmts: Vec<Stmt>, else_stmts: Option<Vec<Stmt>>},
    While{cond: Expr, stmts: Vec<Stmt>},
    For{lhs: Expr, iter: Expr, stmts: Vec<Stmt>},

    Func{name: String, args: Vec<Expr>, stmts: Vec<Stmt>},
    Return{expr: Expr},

    Expr{expr: Expr},
}

#[derive(Clone,Debug)]
pub enum DeclarationType {
    Const,
    Var,
}

#[derive(Clone,Debug)]
pub enum Expr {
    Null,

    Bool{b: bool},
    Int{n: i64},
    Str{s: String},

    Var{name: String},

    UnaryOp{op: UnaryOp, expr: Box<Expr>},
    BinaryOp{op: BinaryOp, lhs: Box<Expr>, rhs: Box<Expr>},

    List{xs: Vec<ListItem>},
    Range{start: Box<Expr>, end: Box<Expr>},
    Index{expr: Box<Expr>, location: Box<Expr>},
    Prop{expr: Box<Expr>, name: String},

    Object{props: Vec<PropItem>},

    Func{args: Vec<Expr>, stmts: Vec<Stmt>},
    Call{expr: Box<Expr>, args: Vec<Expr>},
}

#[derive(Clone,Debug)]
pub struct ListItem {
    pub expr: Expr,
    pub is_spread: bool,
    pub is_unspread: bool,
}

#[derive(Clone,Debug)]
pub enum PropItem {
    Pair{name: Expr, value: Expr},
    Single{expr: Expr, is_spread: bool, is_unspread: bool},
}

#[derive(Clone,Debug)]
pub enum BinaryOp {
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

#[derive(Clone,Debug)]
pub enum UnaryOp {
    Not,
}
