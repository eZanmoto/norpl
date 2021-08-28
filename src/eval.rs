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
        return Err(format!("`return` outside function"));
    }

    Ok(())
}

// See `eval_stmts` for more information on the values
// `eval_stmts_in_new_scope` returns.
pub fn eval_stmts_in_new_scope(
    outer_scopes: &mut ScopeStack,
    stmts: &Vec<Stmt>,
)
    -> Result<Option<ValRef>,String>
{
    eval_stmts(outer_scopes, HashMap::<String, ValRef>::new(), stmts)
}

// `eval_stmts` returns `Some(v)` if one of the statements is evaluates is a a
// `return` statement, otherwise it returns `None`.
pub fn eval_stmts(scopes: &mut ScopeStack, inner_scope: Scope, stmts: &Vec<Stmt>)
    -> Result<Option<ValRef>,String>
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
    -> Result<Option<ValRef>,String>
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
                match apply_binary_operation(&op, &lhs.lock().unwrap(), &rhs.lock().unwrap()) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let r = bind_name(
                scopes,
                name.to_string(),
                new_val_ref(v_),
                BindType::Assignment,
            );
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
                match *cond.lock().unwrap() {
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
                    match *cond.lock().unwrap() {
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

            match &mut *iter_.lock().unwrap() {
                Value::List{xs} => {
                    while xs.len() > 0 {
                        // TODO `lhs.clone()` is being used here because
                        // `bind_unspread_list` is destructive; this can be updated to
                        // a reference if this function is updated to be
                        // non-destructive.
                        let r = bind(scopes, lhs.clone(), xs.remove(0), BindType::Declaration);
                        if let Err(e) = r {
                            return Err(e);
                        }

                        if let Err(e) = eval_stmts_in_new_scope(scopes, &stmts) {
                            return Err(e);
                        }
                    }
                },
                _ => {
                    return Err(format!("iterator must be a `list`"));
                },
            };
        },

        Stmt::Func{name, args, stmts} => {
            let func = Value::Func{
                args: args.clone(),
                stmts: stmts.clone(),
                closure: scopes.clone(),
            };

            let r = bind_name(
                scopes,
                name.clone(),
                new_val_ref(func),
                BindType::Declaration,
            );
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
fn bind(scopes: &mut ScopeStack, lhs: Expr, rhs: ValRef, bt: BindType)
    -> Result<(),String>
{
    match lhs {
        Expr::Var{name} => {
            bind_name(scopes, name, rhs, bt)
        }

        Expr::List{xs} => {
            match &*rhs.lock().unwrap() {
                Value::List{xs: ys} => {
                    // TODO Investigate removing the call to `to_vec()`.
                    bind_list(scopes, xs, ys.to_vec(), bt)
                },
                _ => {
                    Err(format!("can't destructure non-list into list"))
                },
            }
        },

        Expr::Index{expr, location} => {
            match eval_expr(scopes, &expr) {
                Ok(value) => {
                    match &mut *value.lock().unwrap() {
                        Value::List{xs} => {
                            match eval_expr(scopes, &location) {
                                Ok(index) => {
                                    match &*index.lock().unwrap() {
                                        Value::Int{n} => {
                                            // TODO Handle out-of-bounds
                                            // assignment.
                                            xs[*n as usize] = rhs;
                                        },
                                        _ => return Err(format!("index must be an integer")),
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                },
                            };
                        },

                        Value::Object{props} => {
                            match eval_expr(scopes, &location) {
                                Ok(index) => {
                                    match &*index.lock().unwrap() {
                                        Value::Str{s} => {
                                            props.insert(s.to_string(), rhs);
                                        },
                                        _ => return Err(format!("index must be an integer")),
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                },
                            };
                        },

                        _ => return Err(format!("can only assign to indices of lists and objects")),
                    }
                },
                Err(e) => return Err(e),
            };

            Ok(())
        },

        Expr::Object{props: lhs_props} => {
            match &*rhs.lock().unwrap() {
                Value::Object{props: rhs_props} => {
                    bind_object(scopes, lhs_props, rhs_props.clone(), bt)
                },
                _ => {
                    Err(format!("can't destructure non-object into object"))
                },
            }
        },

        Expr::Range{..} => return Err(format!("cannot bind to a range")),
        Expr::Null => return Err(format!("cannot bind to `null`")),
        Expr::Bool{..} => return Err(format!("cannot bind to a boolean literal")),
        Expr::Int{..} => return Err(format!("cannot bind to an integer literal")),
        Expr::Str{..} => return Err(format!("cannot bind to a string literal")),
        Expr::UnaryOp{..} => return Err(format!("cannot bind to a unary operation")),
        Expr::BinaryOp{..} => return Err(format!("cannot bind to a binary operation")),
        Expr::Func{..} => return Err(format!("cannot bind to a function literal")),
        Expr::Call{..} => return Err(format!("cannot bind to a function call")),
    }
}

#[derive(Clone,Copy)]
enum BindType {
    Assignment,
    Declaration,
}

fn bind_name(scopes: &mut ScopeStack, name: String, rhs: ValRef, bind_type: BindType)
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

fn bind_list(scopes: &mut ScopeStack, lhs: Vec<ListItem>, rhs: Vec<ValRef>, bt: BindType)
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

fn bind_unspread_list(scopes: &mut ScopeStack, mut lhs: Vec<ListItem>, mut rhs: Vec<ValRef>, bt: BindType)
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
            bind_name(scopes, name, new_val_ref(Value::List{xs: rhs_rest}), bt)
        }
        _ => {
            Err(format!("can only unspread to a variable"))
        }
    }
}

fn bind_exact_list(scopes: &mut ScopeStack, lhs: Vec<ListItem>, rhs: Vec<ValRef>, bt: BindType)
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

fn bind_object(
    scopes: &mut ScopeStack,
    lhs: Vec<PropItem>,
    rhs: HashMap<String,ValRef>,
    bt: BindType,
)
    -> Result<(),String>
{
    // In contrast with lists, we don't explicitly require that the number of
    // elements in the source object is equal to the number of elements in the
    // target object.

    for prop_item in lhs {
        match prop_item {
            PropItem::Spread{..} => {
                return Err(format!("can't use spread operator in object assigment"));
            },
            PropItem::Pair{name, value: new_lhs, name_is_str} => {
                if name_is_str {
                    return Err(format!("keys in object destructuring can't be strings (must identifier shorthand)"));
                }

                let new_rhs =
                    match rhs.get(&name) {
                        Some(v) => v.clone(),
                        None => return Err(format!("property '{}' not found in source object", name)),
                    };

                if let Err(e) = bind(scopes, new_lhs, new_rhs, bt) {
                    return Err(format!("couldn't bind '{}': {}", name, e));
                }
            },
        }
    }

    Ok(())
}

fn eval_expr(scopes: &mut ScopeStack, expr: &Expr) -> Result<ValRef,String> {
    match expr {
        Expr::Null => Ok(new_val_ref(Value::Null)),

        Expr::Bool{b} => Ok(new_val_ref(Value::Bool{b: b.clone()})),

        Expr::Int{n} => Ok(new_val_ref(Value::Int{n: n.clone()})),

        Expr::Str{s} => Ok(new_val_ref(Value::Str{s: s.clone()})),

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

                match &*v.lock().unwrap() {
                    Value::List{xs} => {
                        for x in xs {
                            vals.push(x.clone());
                        }
                    },
                    _ => {
                        return Err(format!("only lists can be spread"));
                    },
                };
            }

            Ok(new_val_ref(Value::List{xs: vals}))
        },

        Expr::Range{start, end} => {
            let start =
                match eval_expr(scopes, start) {
                    Ok(v) => {
                        match *v.lock().unwrap() {
                            Value::Int{n} => n,
                            _ => return Err(format!("range end must be an integer")),
                        }
                    },
                    Err(e) => {
                        return Err(e);
                    },
                };

            let end =
                match eval_expr(scopes, end) {
                    Ok(v) => {
                        match *v.lock().unwrap() {
                            Value::Int{n} => n,
                            _ => return Err(format!("range end must be an integer")),
                        }
                    },
                    Err(e) => {
                        return Err(e);
                    },
                };

            let range =
                (start..end)
                    .map(|n| new_val_ref(Value::Int{n}))
                    .collect();

            Ok(new_val_ref(Value::List{xs: range}))
        },

        Expr::Index{expr, location} => {
            match eval_expr(scopes, expr) {
                Ok(v) => {
                    match &*v.lock().unwrap() {
                        Value::List{xs} => {
                            match eval_expr(scopes, location) {
                                Ok(v) => {
                                    match &*v.lock().unwrap() {
                                        Value::Int{n} => {
                                            match xs.get(*n as usize) {
                                                Some(v) => return Ok(v.clone()),
                                                None => return Err(format!("index out of bounds")),
                                            };
                                        },
                                        _ => return Err(format!("index must be an integer")),
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                },
                            }
                        },

                        Value::Object{props} => {
                            match eval_expr(scopes, location) {
                                Ok(v) => {
                                    match &*v.lock().unwrap() {
                                        Value::Str{s: name} => {
                                            match props.get(name) {
                                                Some(v) => return Ok(v.clone()),
                                                None => return Err(format!("property name not found")),
                                            };
                                        },
                                        _ => return Err(format!("property name must be a string")),
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                },
                            }
                        },

                        _ => return Err(format!("can only index lists and objects")),
                    }
                },
                Err(e) => {
                    return Err(e);
                },
            };
        },

        Expr::Object{props} => {
            let mut vals = HashMap::<String, ValRef>::new();

            for prop in props {
                match prop {
                    PropItem::Pair{name, value, name_is_str} => {
                        if !name_is_str {
                            return Err(format!("keys in object literals must be strings (can't use identifier shorthand)"));
                        }

                        let v =
                            match eval_expr(scopes, &value) {
                                Ok(v) => v,
                                Err(e) => return Err(e),
                            };

                        vals.insert(name.clone(), v);
                    },
                    PropItem::Spread{expr} => {
                        let v =
                            match eval_expr(scopes, &expr) {
                                Ok(v) => v,
                                Err(e) => return Err(e),
                            };

                        match &*v.lock().unwrap() {
                            Value::Object{props} => {
                                for (name, value) in props.iter() {
                                    vals.insert(name.to_string(), value.clone());
                                }
                            },

                            _ => {
                                return Err(format!("can only spread objects in objects"));
                            },
                        };
                    },
                }
            }

            Ok(new_val_ref(Value::Object{props: vals}))
        },

        Expr::UnaryOp{op, expr} => {
            let v =
                match eval_expr(scopes, &*expr) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let result =
                match apply_unary_operation(op, &v.lock().unwrap()) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            Ok(new_val_ref(result))
        },

        Expr::BinaryOp{op, lhs, rhs} => {
            let exprs = vec![lhs, rhs];

            let mut vals = vec![];
            for expr in exprs {
                match eval_expr(scopes, &*expr) {
                    Ok(v) => vals.push(v),
                    Err(e) => return Err(e),
                }
            }

            let v =
                if let [lhs, rhs] = vals.as_slice() {
                    match apply_binary_operation(op, &lhs.lock().unwrap(), &rhs.lock().unwrap()) {
                        Ok(v) => v,
                        Err(e) => return Err(e),
                    }
                } else {
                    return Err(format!("dev error: unexpected slice size"));
                };

            Ok(new_val_ref(v))
        },

        Expr::Var{name} => {
            let v =
                match scopes.get(name) {
                    Some(v) => v.clone(),
                    None => return Err(format!("'{}' isn't defined", name)),
                };

            Ok(v)
        },

        Expr::Call{func, args} => {
            let vals =
                match eval_exprs(scopes, &args) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let func_ =
                match scopes.get(func) {
                    Some(v) => v,
                    None => return Err(format!("'{}' isn't defined", &func)),
                };

            let v =
                match &*func_.lock().unwrap() {
                    Value::BuiltInFunc{f} => {
                        match f(vals) {
                            Ok(v) => v,
                            Err(e) => return Err(e),
                        }
                    },
                    Value::Func{args: arg_names, stmts, closure} => {
                        let inner_scope: HashMap<String, ValRef> =
                            arg_names.clone().into_iter().zip(vals).collect();

                        let r = eval_stmts(&mut closure.clone(), inner_scope, &stmts);

                        match r {
                            Ok(ret_val) => {
                                match ret_val {
                                    Some(v) => v,
                                    None => new_val_ref(Value::Null),
                                }
                            },
                            Err(e) => {
                                return Err(e);
                            },
                        }
                    },
                    _ => {
                        return Err(format!("'{}' isn't a function", func));
                    },
                };

            Ok(v)
        },

        Expr::Func{args, stmts} => {
            let f = Value::Func{
                args: args.clone(),
                stmts: stmts.clone(),
                closure: scopes.clone(),
            };
            Ok(new_val_ref(f))
        },

        // _ => Err(format!("unhandled expression: {:?}", expr)),
    }
}

pub fn eval_exprs(scopes: &mut ScopeStack, exprs: &Vec<Expr>)
    -> Result<Vec<ValRef>,String>
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

fn apply_unary_operation(op: &UnaryOp, v: &Value) -> Result<Value,String> {
    match v {
        Value::Bool{b} => {
            match op {
                UnaryOp::Not => Ok(Value::Bool{b: !b}),
            }
        },
        _ => Err(format!("invalid type: {:?}", v)),
    }
}

fn apply_binary_operation(op: &BinaryOp, lhs: &Value, rhs: &Value)
    -> Result<Value,String>
{
    match (lhs, rhs) {
        (Value::Int{n: lhs}, Value::Int{n: rhs}) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool{b: lhs == rhs}),
                BinaryOp::NE => Ok(Value::Bool{b: lhs != rhs}),
                BinaryOp::GT => Ok(Value::Bool{b: lhs > rhs}),
                BinaryOp::LT => Ok(Value::Bool{b: lhs < rhs}),

                BinaryOp::Sum => Ok(Value::Int{n: lhs + rhs}),
                BinaryOp::Sub => Ok(Value::Int{n: lhs - rhs}),
                BinaryOp::Mul => Ok(Value::Int{n: lhs * rhs}),
                BinaryOp::Div => Ok(Value::Int{n: lhs / rhs}),
                BinaryOp::Mod => Ok(Value::Int{n: lhs % rhs}),

                _ => Err(format!("unsupported operation for integers ({:?})", op))
            }
        },
        (Value::Bool{b: lhs}, Value::Bool{b: rhs}) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool{b: lhs == rhs}),
                BinaryOp::NE => Ok(Value::Bool{b: lhs != rhs}),
                BinaryOp::And => Ok(Value::Bool{b: *lhs && *rhs}),
                BinaryOp::Or => Ok(Value::Bool{b: *lhs || *rhs}),

                _ => Err(format!("unsupported operation for booleans ({:?})", op))
            }
        },
        (Value::Str{s: lhs}, Value::Str{s: rhs}) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool{b: lhs == rhs}),
                BinaryOp::Sum => Ok(Value::Str{s: lhs.to_owned() + rhs}),

                _ => Err(format!("unsupported operation for strings ({:?})", op))
            }
        },
        (Value::List{xs: lhs}, Value::List{xs: rhs}) => {
            match op {
                BinaryOp::Sum => {
                    let mut xs = vec![];
                    for v in lhs {
                        xs.push(v.clone());
                    }
                    for v in rhs {
                        xs.push(v.clone());
                    }
                    Ok(Value::List{xs})
                },

                _ => Err(format!("unsupported operation for lists ({:?})", op))
            }
        },
        _ => Err(format!("invalid types: {:?}", (lhs, rhs))),
    }
}

pub fn new_val_ref(v: Value) -> Arc<Mutex<Value>> {
    return Arc::new(Mutex::new(v));
}

pub type ValRef = Arc<Mutex<Value>>;

#[derive(Clone,Debug)]
pub enum Value {
    Null,

    Bool{b: bool},
    Int{n: i64},
    Str{s: String},
    List{xs: Vec<ValRef>},
    Object{props: HashMap<String,ValRef>},

    BuiltInFunc{f: fn(Vec<ValRef>) -> Result<ValRef, String>},
    Func{args: Vec<String>, stmts: Vec<Stmt>, closure: ScopeStack},
}

#[derive(Clone,Debug)]
pub struct ScopeStack(Vec<Arc<Mutex<Scope>>>);

pub type Scope = HashMap<String, ValRef>;

impl ScopeStack {
    pub fn new(scopes: Vec<Arc<Mutex<Scope>>>) -> ScopeStack {
        ScopeStack(scopes)
    }

    fn new_from_push(&self, scope: Scope) -> ScopeStack {
        let mut scopes = self.0.clone();
        scopes.push(Arc::new(Mutex::new(scope)));

        ScopeStack::new(scopes)
    }

    fn declare(&mut self, name: String, v: ValRef) {
        self.0.last()
            .expect("`ScopeStack` stack shouldn't be empty")
            .lock()
            .unwrap()
            .insert(name, v);
    }

    // `assign` replaces `name` in the topmost scope of this `ScopeStack` and
    // returns `true`, or else it returns `false` if `name` wasn't found in
    // this `ScopeStack`.
    fn assign(&mut self, name: String, v: ValRef) -> bool {
        for scope in self.0.iter().rev() {
            let mut unlocked_scope = scope.lock().unwrap();
            if unlocked_scope.contains_key(&name) {
                unlocked_scope.insert(name, v);
                return true;
            }
        }

        false
    }

    fn get(&self, name: &String) -> Option<ValRef> {
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
