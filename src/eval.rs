// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;

use ast::Expr;
use ast::Prog;
use ast::Stmt;

pub fn eval_prog(env: &mut HashMap<String, Value>, Prog::Body{stmts}: Prog)
    -> Result<(),String>
{
    for stmt in stmts {
        if let Err(e) = eval_stmt(env, stmt) {
            return Err(e);
        }
    }

    Ok(())
}

fn eval_stmt(env: &mut HashMap<String, Value>, stmt: Stmt) ->
    Result<(),String>
{
    match stmt {
        Stmt::Expr{expr} => {
            if let Err(e) = eval_expr(env, expr) {
                return Err(e);
            }
        },
    }

    Ok(())
}

fn eval_expr(env: &mut HashMap<String, Value>, expr: Expr)
    -> Result<Value,String>
{
    match expr {
        Expr::Int{n} => Ok(Value::Int{n}),

        Expr::Call{func, args} => {
            let mut vals = vec![];

            for arg in args {
                match eval_expr(env, arg) {
                    Ok(v) => vals.push(v),
                    Err(e) => return Err(e),
                }
            }

            let maybe_func =
                match env.get(&func) {
                    Some(v) => v,
                    None => return Err(format!("'{}' isn't defined", &func)),
                };

            if let Value::Func{f} = maybe_func {
                f(vals)
            } else {
                Err(format!("'{}' isn't a function", func))
            }
        },
    }
}

#[derive(Clone,Debug)]
pub enum Value {
    Null,

    Int{n: i64},

    Func{f: fn(Vec<Value>) -> Result<Value, String>},
}
