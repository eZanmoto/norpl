// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use ast::*;

pub fn eval_prog(scopes: &mut ScopeStack, global_scope: Scope, Prog::Body{stmts}: &Prog)
    -> Result<(),String>
{
    let ret_val =
        match eval_stmts(scopes, global_scope, stmts) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

    if let Some(_) = ret_val {
        return Err("`return` outside function".to_string());
    }

    Ok(())
}

// See `eval_stmts` for more information on the values
// `eval_stmts_in_new_scope` returns.
pub fn eval_stmts_in_new_scope(
    outer_scopes: &mut ScopeStack,
    stmts: &Vec<Stmt>,
)
    -> Result<Option<Value>,String>
{
    eval_stmts(outer_scopes, HashMap::<String, Value>::new(), stmts)
}

// `eval_stmts` returns `Some(v)` if one of the statements is evaluates is a a
// `return` statement, otherwise it returns `None`.
pub fn eval_stmts(scopes: &mut ScopeStack, inner_scope: Scope, stmts: &Vec<Stmt>)
    -> Result<Option<Value>,String>
{
    let mut inner_scopes = scopes.new_from_push(inner_scope);

    for stmt in stmts {
        match eval_stmt(&mut inner_scopes, &stmt) {
            Ok(Some(v)) => return Ok(Some(v)),
            Err(e) => return Err(e),
            _ => {},
        }
    }

    Ok(None)
}

// `eval_stmt` returns `Some(v)` if a `return` value is evaluated, otherwise it
// returns `None`.
fn eval_stmt(scopes: &mut ScopeStack, stmt: &Stmt)
    -> Result<Option<Value>,String>
{
    match stmt {
        Stmt::Expr{expr} => {
            if let Err(e) = eval_expr(scopes, &expr) {
                return Err(e);
            }
        },

        Stmt::Declare{lhs, rhs} => {
            let v =
                match eval_expr(scopes, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            // TODO Consider whether `clone()` can be avoided here.
            if let Err(e) = bind(scopes, lhs.clone(), v, BindType::Declaration) {
                return Err(e);
            }
        },

        Stmt::Assign{lhs, rhs} => {
            let v =
                match eval_expr(scopes, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            // TODO Consider whether `clone()` can be avoided here.
            if let Err(e) = bind(scopes, lhs.clone(), v, BindType::Assignment) {
                return Err(e);
            }
        },

        Stmt::OpAssign{name, op, rhs} => {
            let lhs =
                match scopes.get(name) {
                    Some(v) => v.clone(),
                    None => return Err(format!("'{}' isn't defined", name)),
                };

            let rhs =
                match eval_expr(scopes, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let v_ =
                match operate(&op, &lhs, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let r = bind_name(scopes, name.to_string(), v_, BindType::Assignment);
            if let Err(e) = r {
                return Err(e);
            }
        },

        Stmt::If{cond, if_stmts, else_stmts} => {
            let cond =
                match eval_expr(scopes, &cond) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let b =
                match cond {
                    Value::Bool{b} => b,
                    _ => return Err(format!("condition must be a `bool`")),
                };

            if b {
                return eval_stmts_in_new_scope(scopes, &if_stmts);
            } else if let Some(stmts) = else_stmts {
                return eval_stmts_in_new_scope(scopes, &stmts);
            }
        },

        Stmt::While{cond, stmts} => {
            loop {
                let cond =
                    match eval_expr(scopes, &cond) {
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

                if let Err(e) = eval_stmts_in_new_scope(scopes, &stmts) {
                    return Err(e);
                }
            }
        },

        Stmt::For{lhs, iter, stmts} => {
            let iter_ =
                match eval_expr(scopes, &iter) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let mut vals =
                match iter_ {
                    Value::List{xs} => xs,
                    _ => return Err(format!("iterator must be a `list`")),
                };

            while vals.len() > 0 {
                // TODO `lhs.clone()` is being used here because
                // `bind_unspread_list` is destructive; this can be updated to
                // a reference if this function is updated to be
                // non-destructive.
                let r = bind(scopes, lhs.clone(), vals.remove(0), BindType::Declaration);
                if let Err(e) = r {
                    return Err(e);
                }

                if let Err(e) = eval_stmts_in_new_scope(scopes, &stmts) {
                    return Err(e);
                }
            }
        },

        Stmt::Func{name, args, stmts} => {
            let v = Value::Func{
                args: args.clone(),
                stmts: stmts.clone(),
                closure: scopes.clone(),
            };

            let r = bind_name(scopes, name.clone(), v, BindType::Declaration);
            if let Err(e) = r {
                return Err(e);
            }
        },

        Stmt::Return{expr} => {
            let v =
                match eval_expr(scopes, expr) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            return Ok(Some(v));
        },

        // _ => return Err(format!("unhandled statement: {:?}", stmt)),
    }

    Ok(None)
}

// TODO `bind` doesn't check for uniqueness among variable names for now,
// meaning that multiple instances of the same variable may be overwritten in
// the same operation. For example, `[x, x] = [1, 2]` will result in `x` having
// the value 2, instead of reporting the fact that the first instance of the
// assigment is effectively redundant.
fn bind(scopes: &mut ScopeStack, lhs: Expr, rhs: Value, bt: BindType)
    -> Result<(),String>
{
    match lhs {
        Expr::Var{name} => {
            bind_name(scopes, name, rhs, bt)
        }

        Expr::List{xs} => {
            let ys =
                match rhs {
                    Value::List{xs} => xs,
                    _ => return Err(format!("can't destructure non-list into list")),
                };

            bind_list(scopes, xs, ys, bt)
        },

        Expr::Int{..} => return Err(format!("cannot bind to an integer literal")),
        Expr::Str{..} => return Err(format!("cannot bind to a string literal")),
        Expr::Op{..} => return Err(format!("cannot bind to an operation")),
        Expr::Call{..} => return Err(format!("cannot bind to a function call")),
    }
}

#[derive(Clone,Copy)]
enum BindType {
    Assignment,
    Declaration,
}

fn bind_name(scopes: &mut ScopeStack, name: String, rhs: Value, bind_type: BindType)
    -> Result<(),String>
{
    if name == "_" {
        return Ok(())
    }

    match bind_type {
        BindType::Assignment => {
            if !scopes.assign(name.clone(), rhs) {
                return Err(format!("'{}' isn't defined", name));
            }
        },
        BindType::Declaration => {
            scopes.declare(name, rhs)
        },
    }

    Ok(())
}

fn bind_list(scopes: &mut ScopeStack, lhs: Vec<ListItem>, rhs: Vec<Value>, bt: BindType)
    -> Result<(),String>
{
    if lhs.len() == 0 {
        if lhs.len() != rhs.len() {
            return Err(format!("cannot bind {} item(s) to {} item(s)", rhs.len(), lhs.len()));
        }
        return Ok(())
    }

    let ListItem{is_unspread, ..} = lhs[lhs.len()-1];
    if is_unspread {
        return bind_unspread_list(scopes, lhs, rhs, bt);
    }
    return bind_exact_list(scopes, lhs, rhs, bt);
}

fn bind_unspread_list(scopes: &mut ScopeStack, mut lhs: Vec<ListItem>, mut rhs: Vec<Value>, bt: BindType)
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

        if let Err(e) = bind(scopes, expr, rhs, bt) {
            return Err(e);
        }
    }

    match unspread_expr {
        Expr::Var{name} => {
            bind_name(scopes, name, Value::List{xs: rhs_rest}, bt)
        }
        _ => {
            Err(format!("can only unspread to a variable"))
        }
    }
}

fn bind_exact_list(scopes: &mut ScopeStack, lhs: Vec<ListItem>, rhs: Vec<Value>, bt: BindType)
    -> Result<(),String>
{
    if lhs.len() != rhs.len() {
        return Err(format!("cannot bind {} item(s) to {} item(s)", rhs.len(), lhs.len()));
    }

    for (ListItem{expr, is_spread, ..}, rhs) in lhs.into_iter().zip(rhs.into_iter()) {
        if is_spread {
            return Err(format!("can't use spread operator in list assigment"));
        }

        if let Err(e) = bind(scopes, expr, rhs, bt) {
            return Err(e);
        }
    }

    Ok(())
}

fn eval_expr(scopes: &mut ScopeStack, expr: &Expr) -> Result<Value,String> {
    match expr {
        Expr::Int{n} => Ok(Value::Int{n: n.clone()}),

        Expr::Str{s} => Ok(Value::Str{s: s.clone()}),

        Expr::List{xs} => {
            let mut vals = vec![];

            for item in xs {
                let v =
                    match eval_expr(scopes, &item.expr) {
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

        Expr::Op{op, lhs, rhs} => {
            let exprs = vec![lhs, rhs];

            let mut vals = vec![];
            for expr in exprs {
                match eval_expr(scopes, &*expr) {
                    Ok(v) => vals.push(v),
                    Err(e) => return Err(e),
                }
            }

            if let [lhs, rhs] = vals.as_slice() {
                operate(op, lhs, rhs)
            } else {
                Err(format!("dev error: unexpected slice size"))
            }
        },

        Expr::Var{name} => {
            match scopes.get(name) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("'{}' isn't defined", name)),
            }
        },

        Expr::Call{func, args} => {
            let vals =
                match eval_exprs(scopes, &args) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let v =
                match scopes.get(func) {
                    Some(v) => v,
                    None => return Err(format!("'{}' isn't defined", &func)),
                };

            if let Value::BuiltInFunc{f} = v {
                f(vals)
            } else if let Value::Func{args: arg_names, stmts, closure} = v {
                let inner_scope: HashMap<String, Value> =
                    arg_names.clone().into_iter().zip(vals).collect();

                let r = eval_stmts(&mut closure.clone(), inner_scope, &stmts);

                match r {
                    Ok(ret_val) => {
                        match ret_val {
                            Some(v) => return Ok(v),
                            None => return Ok(Value::Null),
                        }
                    },
                    Err(e) => {
                        return Err(e);
                    },
                }
            } else {
                Err(format!("'{}' isn't a function", func))
            }
        },

        // _ => Err(format!("unhandled expression: {:?}", expr)),
    }
}

pub fn eval_exprs(scopes: &mut ScopeStack, exprs: &Vec<Expr>)
    -> Result<Vec<Value>,String>
{
    let mut vals = vec![];

    for expr in exprs {
        match eval_expr(scopes, &expr) {
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
                Op::Sum => Ok(Value::Int{n: lhs + rhs}),
                Op::Div => Ok(Value::Int{n: lhs / rhs}),
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

    BuiltInFunc{f: fn(Vec<Value>) -> Result<Value, String>},
    Func{args: Vec<String>, stmts: Vec<Stmt>, closure: ScopeStack},
}

#[derive(Clone,Debug)]
pub struct ScopeStack(Vec<Arc<Mutex<Scope>>>);

pub type Scope = HashMap<String, Value>;

impl ScopeStack {
    pub fn new(scopes: Vec<Arc<Mutex<Scope>>>) -> ScopeStack {
        ScopeStack(scopes)
    }

    fn new_from_push(&self, scope: Scope) -> ScopeStack {
        let mut scopes = self.0.clone();
        scopes.push(Arc::new(Mutex::new(scope)));

        ScopeStack::new(scopes)
    }

    fn declare(&mut self, name: String, v: Value) {
        self.0.last()
            .expect("`ScopeStack` stack shouldn't be empty")
            .lock()
            .unwrap()
            .insert(name, v);
    }

    // `assign` replaces `name` in the topmost scope of this `ScopeStack` and
    // returns `true`, or else it returns `false` if `name` wasn't found in
    // this `ScopeStack`.
    fn assign(&mut self, name: String, v: Value) -> bool {
        for scope in self.0.iter().rev() {
            let mut unlocked_scope = scope.lock().unwrap();
            if unlocked_scope.contains_key(&name) {
                unlocked_scope.insert(name, v);
                return true;
            }
        }

        false
    }

    fn get(&self, name: &String) -> Option<Value> {
        for scope in self.0.iter().rev() {
            let unlocked_scope = scope.lock().unwrap();
            if let Some(v) = unlocked_scope.get(name) {
                // TODO Remove `clone()`.
                return Some(v.clone());
            }
        }

        None
    }
}
