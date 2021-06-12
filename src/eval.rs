// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;

use ast::*;

pub fn eval_prog(env: &mut HashMap<String, Value>, Prog::Body{stmts}: &Prog)
    -> Result<(),String>
{
    eval_block(env, &stmts)
}

pub fn eval_block(env: &mut HashMap<String, Value>, stmts: &Vec<Stmt>)
    -> Result<(),String>
{
    for stmt in stmts {
        if let Err(e) = eval_stmt(env, &stmt) {
            return Err(e);
        }
    }

    Ok(())
}

fn eval_stmt(env: &mut HashMap<String, Value>, stmt: &Stmt) ->
    Result<(),String>
{
    match stmt {
        Stmt::Expr{expr} => {
            if let Err(e) = eval_expr(env, &expr) {
                return Err(e);
            }
        },

        Stmt::Assign{name, rhs} => {
            match eval_expr(env, &rhs) {
                Ok(v) => {
                    // TODO Investigate whether `clone()` can be avoided here.
                    env.insert(name.clone(), v);
                },
                Err(e) => return Err(e),
            }
        },

        Stmt::OpAssign{name, op, rhs} => {
            let lhs =
                match env.get(name) {
                    Some(v) => v.clone(),
                    None => return Err(format!("'{}' isn't defined", name)),
                };

            let rhs =
                match eval_expr(env, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let v_ =
                match operate(&op, &lhs, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            env.insert(name.clone(), v_);
        },

        Stmt::If{cond, if_stmts, else_stmts} => {
            let cond =
                match eval_expr(env, &cond) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let b =
                match cond {
                    Value::Bool{b} => b,
                    _ => return Err(format!("condition must be a `bool`")),
                };

            if b {
                return eval_block(env, &if_stmts);
            } else if let Some(stmts) = else_stmts {
                return eval_block(env, &stmts);
            }
        },

        Stmt::While{cond, stmts} => {
            loop {
                let cond =
                    match eval_expr(env, &cond) {
                        Ok(v) => v,
                        Err(e) => return Err(e),
                    };

                let b =
                    match cond {
                        Value::Bool{b} => b,
                        _ => return Err(format!("condition must be a `bool`")),
                    };

                if !b {
                    break;
                }

                if let Err(e) = eval_block(env, &stmts) {
                    return Err(e);
                }
            }
        },

        // _ => return Err(format!("unhandled statement: {:?}", stmt)),
    }

    Ok(())
}

fn eval_expr(env: &mut HashMap<String, Value>, expr: &Expr)
    -> Result<Value,String>
{
    match expr {
        Expr::Int{n} => Ok(Value::Int{n: n.clone()}),

        Expr::Str{s} => Ok(Value::Str{s: s.clone()}),

        Expr::Op{lhs, rhs} => {
            let exprs = vec![lhs, rhs];

            let mut vals = vec![];
            for expr in exprs {
                match eval_expr(env, &*expr) {
                    Ok(v) => vals.push(v),
                    Err(e) => return Err(e),
                }
            }

            if let [lhs, rhs] = vals.as_slice() {
                operate(&Op::LT, lhs, rhs)
            } else {
                Err(format!("dev error: unexpected slice size"))
            }
        },

        Expr::Var{name} => {
            match env.get(name) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("'{}' isn't defined", name)),
            }
        },

        Expr::Call{func, args} => {
            let mut vals = vec![];

            for arg in args {
                match eval_expr(env, &arg) {
                    Ok(v) => vals.push(v),
                    Err(e) => return Err(e),
                }
            }

            let v =
                match env.get(func) {
                    Some(v) => v,
                    None => return Err(format!("'{}' isn't defined", &func)),
                };

            if let Value::Func{f} = v {
                f(vals)
            } else {
                Err(format!("'{}' isn't a function", func))
            }
        },

        // _ => Err(format!("unhandled expression: {:?}", expr)),
    }
}

fn operate(op: &Op, lhs: &Value, rhs: &Value)
    -> Result<Value,String>
{
    match (lhs, rhs) {
        (Value::Int{n: lhs}, Value::Int{n: rhs}) => {
            match op {
                Op::LT => Ok(Value::Bool{b: lhs < rhs}),
                Op::Plus => Ok(Value::Int{n: lhs + rhs}),
            }
        },
        _ => Err(format!("invalid types: {:?}", (lhs, rhs))),
    }
}

#[derive(Clone,Debug)]
pub enum Value {
    Null,

    Bool{b: bool},
    Int{n: i64},
    Str{s: String},

    Func{f: fn(Vec<Value>) -> Result<Value, String>},
}
