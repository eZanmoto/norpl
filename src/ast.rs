// Copyright 2021-2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

#[derive(Clone,Debug)]
pub enum Prog {
    Body{stmts: Block},
}

pub type Block = Vec<Stmt>;

#[derive(Clone,Debug)]
pub enum Stmt {
    Import{name: String},

    Declare{lhs: Expr, rhs: Expr, dt: DeclarationType},
    Assign{lhs: Expr, rhs: Expr},
    OpAssign{lhs: Expr, op: BinaryOp, rhs: Expr},

    If{branches: Vec<Branch>, else_stmts: Option<Block>},
    While{cond: Expr, stmts: Block},
    For{lhs: Expr, iter: Expr, stmts: Block},

    Break,
    Continue,

    Func{name: String, args: Vec<Expr>, stmts: Block},
    Return{expr: Expr},

    Expr{expr: Expr},
}

#[derive(Clone,Debug)]
pub struct Branch {
    pub cond: Expr,
    pub stmts: Block,
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
    InterpolatedStr{s: String},

    Var{name: String},

    UnaryOp{op: UnaryOp, expr: Box<Expr>},
    BinaryOp{op: BinaryOp, lhs: Box<Expr>, rhs: Box<Expr>},

    List{xs: Vec<ListItem>, is_mutable: bool},
    Range{start: Box<Expr>, end: Box<Expr>},
    Index{expr: Box<Expr>, location: Box<Expr>},
    IndexRange{expr: Box<Expr>, start: Option<Box<Expr>>, end: Option<Box<Expr>>},
    Prop{expr: Box<Expr>, name: String, prototype: bool},
    Subcommand{expr: Box<Expr>, name: String},
    CatchAsBool{expr: Box<Expr>},
    CatchAsError{expr: Box<Expr>},

    Object{props: Vec<PropItem>, is_mutable: bool},

    Func{args: Vec<Expr>, stmts: Block},
    Call{expr: Box<Expr>, args: Vec<Expr>},
    Spawn{expr: Box<Expr>, args: Vec<Expr>},
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
    GTE,
    LT,
    LTE,

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
