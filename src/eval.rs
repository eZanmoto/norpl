// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;

use ast::*;

pub fn eval_prog(
    scopes: &mut ScopeStack,
    global_bindings: Vec<(Expr, ValRefWithSource)>,
    Prog::Body{stmts}: &Prog,
)
    -> Result<(),String>
{
    let ret_val =
        match eval_stmts(scopes, global_bindings, stmts) {
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
pub fn eval_stmts_in_new_scope(outer_scopes: &mut ScopeStack, stmts: &Block)
    -> Result<Option<ValRefWithSource>,String>
{
    eval_stmts(outer_scopes, vec![], stmts)
}

// `eval_stmts` evaluates `stmts` in a new scope pushed onto `scopes`, with the
// given `new_bindings` declared in the new scope. It returns `Some(v)` if one
// of the statements is evaluates is a a `return` statement, otherwise it
// returns `None`.
pub fn eval_stmts(
    scopes: &mut ScopeStack,
    new_bindings: Vec<(Expr, ValRefWithSource)>,
    stmts: &Block,
)
    -> Result<Option<ValRefWithSource>,String>
{
    let mut inner_scopes = scopes.new_from_push(HashMap::new());

    for (lhs, rhs) in new_bindings {
        let r = bind(&mut inner_scopes, lhs.clone(), rhs, BindType::VarDeclaration);
        if let Err(e) = r {
            return Err(e);
        }
    }

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
    -> Result<Option<ValRefWithSource>,String>
{
    match stmt {
        Stmt::Expr{expr} => {
            if let Err(e) = eval_expr(scopes, &expr) {
                return Err(e);
            }
        },

        Stmt::Declare{lhs, rhs, dt} => {
            let v =
                match eval_expr(scopes, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let bt =
                match dt {
                    DeclarationType::Const => BindType::ConstDeclaration,
                    DeclarationType::Var => BindType::VarDeclaration,
                };

            // TODO Consider whether `clone()` can be avoided here.
            if let Err(e) = bind(scopes, lhs.clone(), v, bt) {
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

        Stmt::OpAssign{lhs, op, rhs} => {
            let lhs =
                match eval_expr(scopes, &lhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let rhs =
                match eval_expr(scopes, &rhs) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let result = apply_binary_operation(
                &op,
                &lhs.lock().unwrap().v,
                &rhs.lock().unwrap().v,
            );

            let v_ =
                match result {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            (*lhs.lock().unwrap()).v = v_;
        },

        Stmt::If{branches, else_stmts} => {
            for Branch{cond, stmts} in branches {
                let cond =
                    match eval_expr(scopes, &cond) {
                        Ok(v) => v,
                        Err(e) => return Err(e),
                    };

                let b =
                    match (*cond.lock().unwrap()).v {
                        Value::Bool{b} => b,
                        _ => return Err(format!("condition must be a `bool`")),
                    };

                if b {
                    return eval_stmts_in_new_scope(scopes, &stmts);
                }
            }

            if let Some(stmts) = else_stmts {
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
                    match (*cond.lock().unwrap()).v {
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

            match &mut (*iter_.lock().unwrap()).v {
                Value::List{xs} => {
                    let mut i = 0;
                    while xs.len() > 0 {
                        // TODO `lhs.clone()` is being used here because
                        // `bind_unspread_list` is destructive; this can be updated to
                        // a reference if this function is updated to be
                        // non-destructive.
                        let entry = new_val_ref(Value::List{xs: vec![
                            new_val_ref(Value::Int{n: i}),
                            xs.remove(0),
                        ]});

                        let new_bindings = vec![(lhs.clone(), entry)];

                        let r = eval_stmts(scopes, new_bindings, &stmts);
                        if let Err(e) = r {
                            return Err(e);
                        }

                        i += 1;
                    }
                },

                Value::Object{props} => {
                    for (key, value) in props {
                        let entry = new_val_ref(Value::List{xs: vec![
                            new_val_ref(Value::Str{s: key.to_string()}),
                            value.clone(),
                        ]});

                        let new_bindings = vec![(lhs.clone(), entry)];

                        let r = eval_stmts(scopes, new_bindings, &stmts);
                        if let Err(e) = r {
                            return Err(e);
                        }
                    }
                },

                _ => {
                    return Err(format!("iterator must be a list or object"));
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
                BindType::VarDeclaration,
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

fn bind(scopes: &mut ScopeStack, lhs: Expr, rhs: ValRefWithSource, bt: BindType)
    -> Result<(),String>
{
    bind_(scopes, &mut HashSet::new(), lhs, rhs, bt)
}

fn bind_(
    scopes: &mut ScopeStack,
    already_declared: &mut HashSet<String>,
    lhs: Expr,
    rhs: ValRefWithSource,
    bt: BindType,
)
    -> Result<(),String>
{
    match lhs {
        Expr::Var{name} => {
            bind_name_(scopes, already_declared, name, rhs, bt)
        }

        Expr::List{xs} => {
            match &(*rhs.lock().unwrap()).v {
                Value::List{xs: ys} => {
                    // TODO Investigate removing the call to `to_vec()`.
                    bind_list(scopes, already_declared, xs, ys.to_vec(), bt)
                },
                _ => {
                    Err(format!("can't destructure non-list into list"))
                },
            }
        },

        Expr::Index{expr, location} => {
            match eval_expr(scopes, &expr) {
                Ok(value) => {
                    match &mut (*value.lock().unwrap()).v {
                        Value::List{xs} => {
                            match eval_expr(scopes, &location) {
                                Ok(index) => {
                                    match &(*index.lock().unwrap()).v {
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
                                    match &(*index.lock().unwrap()).v {
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

        Expr::Prop{expr, name} => {
            match eval_expr(scopes, &expr) {
                Ok(value) => {
                    match &mut (*value.lock().unwrap()).v {
                        Value::Object{props} => props.insert(name, rhs),
                        _ => return Err(format!("can only assign to properties of objects")),
                    }
                },
                Err(e) => return Err(e),
            };

            Ok(())
        },

        Expr::Object{props: lhs_props} => {
            match &(*rhs.lock().unwrap()).v {
                Value::Object{props: rhs_props} => {
                    bind_object(scopes, already_declared, lhs_props, rhs_props.clone(), bt)
                },
                _ => {
                    Err(format!("can't destructure non-object into object"))
                },
            }
        },

        // TODO Investigate binding to index ranges.
        Expr::IndexRange{..} => return Err(format!("cannot bind to an index range")),

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
    ConstDeclaration,
    VarDeclaration,
}

fn bind_name(scopes: &mut ScopeStack, name: String, rhs: ValRefWithSource, bind_type: BindType)
    -> Result<(),String>
{
    bind_name_(scopes, &mut HashSet::new(), name, rhs, bind_type)
}

fn bind_name_(
    scopes: &mut ScopeStack,
    already_declared: &mut HashSet<String>,
    name: String,
    rhs: ValRefWithSource,
    bind_type: BindType,
)
    -> Result<(),String>
{
    if name == "_" {
        return Ok(())
    }

    if already_declared.contains(&name) {
        return Err(format!("'{}' has already been declared in this binding", name));
    }
    already_declared.insert(name.clone());

    let result =
        match bind_type {
            BindType::Assignment => scopes.assign(name.clone(), rhs),
            BindType::ConstDeclaration => scopes.declare(name, rhs, DeclType::Const),
            BindType::VarDeclaration => scopes.declare(name, rhs, DeclType::Var),
        };

    if let Err(e) = result {
        return Err(e);
    }

    Ok(())
}

fn bind_list(
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<ListItem>,
    rhs: Vec<ValRefWithSource>,
    bt: BindType,
)
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
        return bind_unspread_list(scopes, &mut already_declared, lhs, rhs, bt);
    }
    return bind_exact_list(scopes, &mut already_declared, lhs, rhs, bt);
}

fn bind_unspread_list(
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    mut lhs: Vec<ListItem>,
    mut rhs: Vec<ValRefWithSource>,
    bt: BindType,
)
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

        if let Err(e) = bind_(scopes, &mut already_declared, expr, rhs, bt) {
            return Err(e);
        }
    }

    match unspread_expr {
        Expr::Var{name} => {
            bind_name_(scopes, &mut already_declared, name, new_val_ref(Value::List{xs: rhs_rest}), bt)
        }
        _ => {
            Err(format!("can only unspread to a variable"))
        }
    }
}

fn bind_exact_list(
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<ListItem>,
    rhs: Vec<ValRefWithSource>,
    bt: BindType,
)
    -> Result<(),String>
{
    if lhs.len() != rhs.len() {
        return Err(format!("cannot bind {} item(s) to {} item(s)", rhs.len(), lhs.len()));
    }

    for (ListItem{expr, is_spread, ..}, rhs) in lhs.into_iter().zip(rhs.into_iter()) {
        if is_spread {
            return Err(format!("can't use spread operator in list assigment"));
        }

        if let Err(e) = bind_(scopes, &mut already_declared, expr, rhs, bt) {
            return Err(e);
        }
    }

    Ok(())
}

fn bind_object(
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<PropItem>,
    rhs: HashMap<String,ValRefWithSource>,
    bt: BindType,
)
    -> Result<(),String>
{
    // In contrast with lists, we don't explicitly require that the number of
    // elements in the source object is equal to the number of elements in the
    // target object.

    let mut rhs_keys: HashSet<String> = rhs.keys().cloned().collect();

    let lhs_len = lhs.len();
    for (i, prop_item) in lhs.into_iter().enumerate() {
        match prop_item {
            PropItem::Single{expr, is_spread, is_unspread} => {
                if is_spread {
                    return Err(format!("can't use spread operator in object destructuring"));
                }

                let name =
                    if let Expr::Var{name} = &expr {
                        name.clone()
                    } else {
                        // TODO Improve the description of this error message.
                        return Err(format!("can only use variable name for object property shorthand"));
                    };

                if is_unspread {
                    if i == lhs_len - 1 {
                        let mut props: HashMap<String, ValRefWithSource> = HashMap::new();
                        for k in &rhs_keys {
                            if let Some(v) = rhs.get(k) {
                                props.insert(k.to_string(), v.clone());
                            } else {
                                return Err(format!("object doesn't contain property '{}'", k));
                            }
                        }

                        let lhs = Expr::Var{name: name.clone()};
                        let new_rhs = new_val_ref(Value::Object{props});
                        if let Err(e) = bind_(scopes, &mut already_declared, lhs, new_rhs, bt) {
                            return Err(format!("couldn't bind '{}': {}", name, e));
                        }
                    } else {
                        return Err(format!("unspread can only appear on last item in object"));
                    }
                } else {
                    rhs_keys.remove(&name);

                    let item_name = Expr::Str{s: name.clone()};
                    let result = bind_object_pair(scopes, &mut already_declared, expr, &rhs, &item_name, bt);
                    if let Err(e) = result {
                        return Err(format!("couldn't bind object pair '{}': {}", name, e));
                    }
                }
            },
            PropItem::Pair{name, value: new_lhs} => {
                let prop_name =
                    match eval_expr(scopes, &name) {
                        Ok(v) => {
                            match &(*v.lock().unwrap()).v {
                                Value::Str{s} => s.clone(),
                                _ => return Err(format!("property key must be a string")),
                            }
                        },
                        Err(e) => {
                            return Err(e);
                        },
                    };

                rhs_keys.remove(&prop_name);

                let result = bind_object_pair(scopes, already_declared, new_lhs, &rhs, &name, bt);
                if let Err(e) = result {
                    return Err(format!("couldn't bind object pair '{}': {}", prop_name, e));
                }
            },
        }
    }

    Ok(())
}

// TODO This function is used to simplify `bind_object`, but its signature
// isn't very clear at present, and should be clarified when possible.
fn bind_object_pair(
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Expr,
    rhs: &HashMap<String,ValRefWithSource>,
    item_name: &Expr,
    bt: BindType,
)
    -> Result<(),String>
{
    let name =
        match eval_expr(scopes, item_name) {
            Ok(v) => {
                match &(*v.lock().unwrap()).v {
                    Value::Str{s} => s.clone(),
                    _ => return Err(format!("property key must be a string")),
                }
            },
            Err(e) => {
                return Err(e);
            },
        };

    let new_rhs =
        match rhs.get(&name) {
            Some(v) => v.clone(),
            None => return Err(format!("property '{}' not found in source object", name)),
        };

    if let Err(e) = bind_(scopes, &mut already_declared, lhs, new_rhs, bt) {
        return Err(format!("couldn't bind '{}': {}", name, e));
    }

    Ok(())
}

fn eval_expr(scopes: &mut ScopeStack, expr: &Expr) -> Result<ValRefWithSource,String> {
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

                match &(*v.lock().unwrap()).v {
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
                        match (*v.lock().unwrap()).v {
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
                        match (*v.lock().unwrap()).v {
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
                Ok(source) => {
                    match &(*source.lock().unwrap()).v {
                        Value::List{xs} => {
                            match eval_expr(scopes, location) {
                                Ok(v) => {
                                    match &(*v.lock().unwrap()).v {
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
                                    match &(*v.lock().unwrap()).v {
                                        Value::Str{s: name} => {
                                            match props.get(name) {
                                                Some(v) => {
                                                    let prop_val = &(*v.lock().unwrap()).v;
                                                    return Ok(new_val_ref_with_source(
                                                        prop_val.clone(),
                                                        source.clone(),
                                                    ));
                                                },
                                                None => {
                                                    return Err(format!("property name not found"));
                                                },
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

        Expr::IndexRange{expr, start: maybe_start, end: maybe_end} => {
            match eval_expr(scopes, expr) {
                Ok(source) => {
                    match &(*source.lock().unwrap()).v {
                        Value::List{xs} => {
                            if let Some(start) = maybe_start {
                                match eval_expr(scopes, start) {
                                    Ok(v) => {
                                        match &(*v.lock().unwrap()).v {
                                            Value::Int{n: start} => {
                                                if let Some(end) = maybe_end {
                                                    match eval_expr(scopes, end) {
                                                        Ok(v) => {
                                                            match &(*v.lock().unwrap()).v {
                                                                Value::Int{n: end} => {
                                                                    return get_index_range(
                                                                        xs,
                                                                        Some(*start as usize),
                                                                        Some(*end as usize),
                                                                    );
                                                                },
                                                                _ => return Err(format!("index must be an integer")),
                                                            }
                                                        },
                                                        Err(e) => {
                                                            return Err(e);
                                                        },
                                                    }
                                                }

                                                return get_index_range(xs, Some(*start as usize), None);
                                            },
                                            _ => return Err(format!("index must be an integer")),
                                        }
                                    },
                                    Err(e) => {
                                        return Err(e);
                                    },
                                }
                            }

                            if let Some(end) = maybe_end {
                                match eval_expr(scopes, end) {
                                    Ok(v) => {
                                        match &(*v.lock().unwrap()).v {
                                            Value::Int{n: end} => {
                                                return get_index_range(xs, None, Some(*end as usize));
                                            },
                                            _ => return Err(format!("index must be an integer")),
                                        }
                                    },
                                    Err(e) => {
                                        return Err(e);
                                    },
                                }
                            }
                            return get_index_range(xs, None, None);
                        },

                        _ => return Err(format!("can only index range lists")),
                    }
                },
                Err(e) => {
                    return Err(e);
                },
            };
        },

        Expr::Prop{expr, name} => {
            match eval_expr(scopes, expr) {
                Ok(object) => {
                    match &(*object.lock().unwrap()).v {
                        Value::Object{props} => {
                            match props.get(name) {
                                Some(v) => {
                                    let prop_val = &(*v.lock().unwrap()).v;
                                    return Ok(new_val_ref_with_source(
                                        prop_val.clone(),
                                        object.clone(),
                                    ));
                                },
                                None => {
                                    return Err(format!("property name not found"));
                                },
                            };
                        },

                        _ => return Err(format!("can only access properties of objects")),
                    }
                },
                Err(e) => {
                    return Err(e);
                },
            };
        },

        Expr::Object{props} => {
            let mut vals = HashMap::<String, ValRefWithSource>::new();

            for prop in props {
                match prop {
                    PropItem::Pair{name, value} => {
                        let k =
                            match eval_expr(scopes, &name) {
                                Ok(v) => {
                                    match &(*v.lock().unwrap()).v {
                                        Value::Str{s} => s.clone(),
                                        _ => return Err(format!("property key must be a string")),
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                },
                            };

                        let v =
                            match eval_expr(scopes, &value) {
                                Ok(v) => v,
                                Err(e) => return Err(e),
                            };

                        vals.insert(k, v);
                    },
                    PropItem::Single{expr, is_spread, is_unspread} => {
                        if *is_unspread {
                            return Err(format!("can't use unspread operator in objectliteral"));
                        }

                        if *is_spread {
                            let v =
                                match eval_expr(scopes, &expr) {
                                    Ok(v) => v,
                                    Err(e) => return Err(e),
                                };

                            match &(*v.lock().unwrap()).v {
                                Value::Object{props} => {
                                    for (name, value) in props.iter() {
                                        vals.insert(name.to_string(), value.clone());
                                    }
                                },

                                _ => {
                                    return Err(format!("can only spread objects in objects"));
                                },
                            };
                        } else {
                            if let Expr::Var{name} = expr {
                                let v =
                                    match scopes.get(name) {
                                        Some(v) => v.clone(),
                                        None => return Err(format!("'{}' isn't defined", name)),
                                    };

                                vals.insert(name.to_string(), v);
                            } else {
                                // TODO Improve the description of this error message.
                                return Err(format!("can only use variable name for object property shorthand"));
                            }
                        }
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
                match apply_unary_operation(op, &v.lock().unwrap().v) {
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
                    let result = apply_binary_operation(
                        op,
                        &lhs.lock().unwrap().v,
                        &rhs.lock().unwrap().v,
                    );
                    match result {
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

        Expr::Call{expr, args} => {
            let vals =
                match eval_exprs(scopes, &args) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

            let v =
                match eval_expr(scopes, &expr) {
                    Ok(value) => {
                        let ValWithSource{v, source} = &*value.lock().unwrap();
                        match v {
                            Value::BuiltInFunc{f} => {
                                Either::Left{value: (
                                    f.clone(),
                                    vals,
                                )}
                            },

                            Value::Func{args: arg_names, stmts, closure} => {
                                let mut new_bindings: Vec<(Expr, ValRefWithSource)> =
                                    arg_names
                                        .clone()
                                        .into_iter()
                                        .zip(vals)
                                        .collect();

                                // TODO Consider whether to add `this` as
                                // `null` in the case where `source` is `None`.
                                if let Some(this) = source {
                                    // TODO Consider how to avoid creating a
                                    // new AST variable node here.
                                    new_bindings.push((
                                        Expr::Var{name: "this".to_string()},
                                        this.clone(),
                                    ));
                                }

                                Either::Right{value:(
                                    new_bindings,
                                    closure.clone(),
                                    stmts.clone(),
                                )}
                            },

                            _ => return Err(format!("can only call functions")),
                        }
                    },
                    Err(e) => return Err(e),
                };

            let v =
                match v {
                    Either::Left{value: (f, vals)} => {
                        match f(vals) {
                            Ok(v) => v,
                            Err(e) => return Err(e),
                        }
                    },
                    Either::Right{value: (new_bindings, closure, stmts)} => {
                        let r = eval_stmts(&mut closure.clone(), new_bindings, &stmts);

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

fn get_index_range(
    xs: &Vec<ValRefWithSource>,
    mut maybe_start: Option<usize>,
    mut maybe_end: Option<usize>,
)
    -> Result<ValRefWithSource,String>
{
    let start = maybe_start.get_or_insert(0);
    let end = maybe_end.get_or_insert(xs.len());

    if let Some(vs) = xs.get(*start .. *end) {
        let list = Value::List{xs: vs.to_vec()};
        return Ok(new_val_ref(list));
    }

    Err(format!("index out of bounds"))
}

pub fn eval_exprs(scopes: &mut ScopeStack, exprs: &Vec<Expr>)
    -> Result<Vec<ValRefWithSource>,String>
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

// TODO Consider renaming to `new_val_ref_with_no_source`.
pub fn new_val_ref(v: Value) -> ValRefWithSource {
    Arc::new(Mutex::new(ValWithSource{
        v: v,
        source: None,
    }))
}

pub fn new_val_ref_with_source(v: Value, source: ValRefWithSource) -> ValRefWithSource {
    Arc::new(Mutex::new(ValWithSource{
        v: v,
        source: Some(source),
    }))
}

enum Either<A,B> {
    Left{value: A},
    Right{value: B},
}

// `ValRefWithSource` is intended to be used as a regular `ValRef` would, but
// it includes the most recent object it was referenced from. For example, in
// the case of `x['f']`, the `ValRef` is the value stored at the location
// `'f'`, and the `source` of this value is `x`.
pub type ValRefWithSource = Arc<Mutex<ValWithSource>>;

#[derive(Clone,Debug)]
pub struct ValWithSource {
    pub v: Value,
    source: Option<ValRefWithSource>,
}

#[derive(Clone,Debug)]
pub enum Value {
    Null,

    Bool{b: bool},
    Int{n: i64},
    Str{s: String},
    List{xs: Vec<ValRefWithSource>},
    Object{props: HashMap<String,ValRefWithSource>},

    BuiltInFunc{f: fn(Vec<ValRefWithSource>) -> Result<ValRefWithSource, String>},
    Func{args: Vec<Expr>, stmts: Block, closure: ScopeStack},
}

#[derive(Clone,Debug)]
pub struct ScopeStack(Vec<Arc<Mutex<Scope>>>);

pub type Scope = HashMap<String, (ValRefWithSource, DeclType)>;

#[derive(Debug,PartialEq)]
pub enum DeclType {
    Const,
    Var,
}

impl ScopeStack {
    pub fn new(scopes: Vec<Arc<Mutex<Scope>>>) -> ScopeStack {
        ScopeStack(scopes)
    }

    fn new_from_push(&self, scope: Scope) -> ScopeStack {
        let mut scopes = self.0.clone();
        scopes.push(Arc::new(Mutex::new(scope)));

        ScopeStack::new(scopes)
    }

    fn declare(&mut self, name: String, v: ValRefWithSource, decl_type: DeclType)
        -> Result<(), String>
    {
        let mut cur_scope =
            self.0.last()
                .expect("`ScopeStack` stack shouldn't be empty")
                .lock()
                .unwrap();

        if cur_scope.contains_key(&name) {
            return Err(format!("'{}' is already defined in this scope", name));
        }
        cur_scope.insert(name, (v, decl_type));

        Ok(())
    }

    // `assign` replaces `name` in the topmost scope of this `ScopeStack` and
    // returns `true`, or else it returns `false` if `name` wasn't found in
    // this `ScopeStack`. `assign` returns an error if attempting to assign to
    // a constant binding.
    fn assign(&mut self, name: String, v: ValRefWithSource) -> Result<(),String> {
        for scope in self.0.iter().rev() {
            let mut unlocked_scope = scope.lock().unwrap();
            if let Some((_, decl_type)) = unlocked_scope.get(&name) {
                if *decl_type == DeclType::Const {
                    return Err(format!("cannot assign to constant binding"));
                }
                // This should ideally overwrite the value stored in this
                // variable instead of introducing a new variable with a new
                // binding, but this isn't possible at present with the current
                // structure of `ValRefWithSource`; see the comment above
                // `ValRefWithSource` for more details.
                unlocked_scope.insert(name, (v, DeclType::Var));
                return Ok(());
            }
        }

        Err(format!("'{}' isn't defined", name))
    }

    fn get(&self, name: &String) -> Option<ValRefWithSource> {
        for scope in self.0.iter().rev() {
            let unlocked_scope = scope.lock().unwrap();
            if let Some((v, _)) = unlocked_scope.get(name) {
                // TODO Remove `clone()`.
                return Some(v.clone());
            }
        }

        None
    }
}
