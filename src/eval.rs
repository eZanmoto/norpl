// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;

use ast::*;

pub fn eval_prog(env: &mut HashMap<String, Value>, Prog::Body{stmts}: &Prog)
    -> Result<(),String>
{
    eval_stmts(env, &stmts)
}

pub fn eval_stmts(env: &mut HashMap<String, Value>, stmts: &Vec<Stmt>)
    -> Result<(),String>
{
    for stmt in stmts {
        if let Err(e) = eval_stmt(env, &stmt) {
            return Err(e);
        }
    }

    Ok(())
}

fn eval_stmt(env: &mut HashMap<String, Value>, stmt: &Stmt)
    -> Result<(),String>
{
    match stmt {
        Stmt::Expr{expr} => {
            if let Err(e) = eval_expr(env, &expr) {
                return Err(e);
            }
        },

        Stmt::Assign{lhs, rhs} => {
            let v =
                match eval_expr(env, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            // TODO Consider whether `clone()` can be avoided here.
            if let Err(e) = assign(env, lhs.clone(), v) {
                return Err(e);
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
                return eval_stmts(env, &if_stmts);
            } else if let Some(stmts) = else_stmts {
                return eval_stmts(env, &stmts);
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

                if let Err(e) = eval_stmts(env, &stmts) {
                    return Err(e);
                }
            }
        },

        Stmt::For{name, iter, stmts} => {
            let iter_ =
                match eval_expr(env, &iter) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let mut vals =
                match iter_ {
                    Value::List{xs} => xs,
                    _ => return Err(format!("iterator must be a `list`")),
                };

            while vals.len() > 0 {
                if let Err(e) = assign_var(env, name.clone(), vals.remove(0)) {
                    return Err(e);
                }

                if let Err(e) = eval_stmts(env, &stmts) {
                    return Err(e);
                }
            }
        },

        // _ => return Err(format!("unhandled statement: {:?}", stmt)),
    }

    Ok(())
}

// TODO `assign` doesn't check for uniqueness among variable names for now,
// meaning that multiple instances of the same variable may be overwritten in
// the same operation. For example, `[x, x] = [1, 2]` will result in `x` having
// the value 2, instead of reporting the fact that the first instance of the
// assigment is effectively redundant.
fn assign(env: &mut HashMap<String, Value>, lhs: Expr, rhs: Value)
    -> Result<(),String>
{
    match lhs {
        Expr::Var{name} => {
            assign_var(env, name, rhs)
        }

        Expr::List{xs} => {
            let ys =
                match rhs {
                    Value::List{xs} => xs,
                    _ => return Err(format!("can't destructure non-list into list")),
                };

            assign_list(env, xs, ys)
        },

        Expr::Int{..} => return Err(format!("cannot assign to an integer literal")),
        Expr::Str{..} => return Err(format!("cannot assign to a string literal")),
        Expr::Op{..} => return Err(format!("cannot assign to an operation")),
        Expr::Call{..} => return Err(format!("cannot assign to a function call")),
    }
}

fn assign_var(env: &mut HashMap<String, Value>, name: String, rhs: Value)
    -> Result<(),String>
{
    if name != "_" {
        env.insert(name, rhs);
    }

    Ok(())
}

fn assign_list(env: &mut HashMap<String, Value>, lhs: Vec<ListItem>, rhs: Vec<Value>)
    -> Result<(),String>
{
    if lhs.len() == 0 {
        if lhs.len() != rhs.len() {
            return Err(format!("cannot assign {} item(s) to {} item(s)", rhs.len(), lhs.len()));
        }
        return Ok(())
    }

    let ListItem{is_unspread, ..} = lhs[lhs.len()-1];
    if is_unspread {
        return assign_unspread_list(env, lhs, rhs);
    }
    return assign_exact_list(env, lhs, rhs);
}

fn assign_unspread_list(env: &mut HashMap<String, Value>, mut lhs: Vec<ListItem>, mut rhs: Vec<Value>)
    -> Result<(),String>
{
    if lhs.len() > rhs.len() {
        return Err(format!("cannot unspread {} item(s) to {} item(s)", rhs.len(), lhs.len()));
    }

    let ListItem{expr: unspread_expr, ..} =
        match lhs.pop() {
            Some(v) => v,
            None => return Err(format!("dev error: `lhs` shoudn't be empty")),
        };

    let rhs_rest = rhs.split_off(lhs.len());

    for (ListItem{expr, is_spread, ..}, rhs) in lhs.into_iter().zip(rhs.into_iter()) {
        if is_spread {
            return Err(format!("can't use spread operator in list assigment"));
        }

        if let Err(e) = assign(env, expr, rhs) {
            return Err(e);
        }
    }

    match unspread_expr {
        Expr::Var{name} => {
            assign_var(env, name, Value::List{xs: rhs_rest})
        }
        _ => {
            Err(format!("can only unspread to a variable"))
        }
    }
}

fn assign_exact_list(env: &mut HashMap<String, Value>, lhs: Vec<ListItem>, rhs: Vec<Value>)
    -> Result<(),String>
{
    if lhs.len() != rhs.len() {
        return Err(format!("cannot assign {} item(s) to {} item(s)", rhs.len(), lhs.len()));
    }

    for (ListItem{expr, is_spread, ..}, rhs) in lhs.into_iter().zip(rhs.into_iter()) {
        if is_spread {
            return Err(format!("can't use spread operator in list assigment"));
        }

        if let Err(e) = assign(env, expr, rhs) {
            return Err(e);
        }
    }

    Ok(())
}

fn eval_expr(env: &mut HashMap<String, Value>, expr: &Expr)
    -> Result<Value,String>
{
    match expr {
        Expr::Int{n} => Ok(Value::Int{n: n.clone()}),

        Expr::Str{s} => Ok(Value::Str{s: s.clone()}),

        Expr::List{xs} => {
            let mut vals = vec![];

            for item in xs {
                let v =
                    match eval_expr(env, &item.expr) {
                        Ok(v) => v,
                        Err(e) => return Err(e),
                    };

                if !item.is_spread {
                    vals.push(v);
                    continue;
                }

                match v {
                    Value::List{mut xs} => vals.append(&mut xs),
                    _ => return Err(format!("only lists can be spread")),
                }
            }

            Ok(Value::List{xs: vals})
        },

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
            let vals =
                match eval_exprs(env, &args) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

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

pub fn eval_exprs(env: &mut HashMap<String, Value>, exprs: &Vec<Expr>)
    -> Result<Vec<Value>,String>
{
    let mut vals = vec![];

    for expr in exprs {
        match eval_expr(env, &expr) {
            Ok(v) => vals.push(v),
            Err(e) => return Err(e),
        }
    }

    Ok(vals)
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
    List{xs: Vec<Value>},

    Func{f: fn(Vec<Value>) -> Result<Value, String>},
}
