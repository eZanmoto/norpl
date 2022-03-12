// Copyright 2021-2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;

pub mod builtins;
pub mod import;
pub mod value;

use ast::*;
use lexer::Lexer;
use parser::ProgParser;
use self::builtins::Builtins;
use self::import::ImportType;
use self::import::ModuleId;
use self::value::DeclType;
use self::value::List;
use self::value::Mutability;
use self::value::ScopeStack;
use self::value::Str;
use self::value::ValRefWithSource;
use self::value::Value;
use self::value::ValWithSource;

macro_rules! match_eval_expr {
    (
        ( $context:ident, $scopes:ident, $expr:expr )
        { $( $key:pat => $value:expr , )* }
    ) => {{
        let value = eval_expr($context, $scopes, $expr)?;
        let unlocked_value = &mut (*value.lock().unwrap()).v;
        match unlocked_value {
            $( $key => $value , )*
        }
    }};
}

pub fn eval_prog(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    global_bindings: Vec<(Expr, ValRefWithSource)>,
    Prog::Body{stmts}: &Prog,
)
    -> Result<(),String>
{
    let ret_val = eval_stmts(context, scopes, global_bindings, stmts)?;

    match ret_val {
        Escape::Return(_) => return Err(format!("`return` outside function")),
        Escape::Break => return Err(format!("`break` outside loop")),
        Escape::Continue => return Err(format!("`continue` outside loop")),
        Escape::None => {},
    }

    Ok(())
}

// See `eval_stmts` for more information on the values
// `eval_stmts_in_new_scope` returns.
pub fn eval_stmts_in_new_scope(
    context: &EvaluationContext,
    outer_scopes: &mut ScopeStack,
    stmts: &Block,
)
    -> Result<Escape,String>
{
    eval_stmts(context, outer_scopes, vec![], stmts)
}

// `eval_stmts` evaluates `stmts` in a new scope pushed onto `scopes`, with the
// given `new_bindings` declared in the new scope. It returns `Some(v)` if one
// of the statements is evaluates is a a `return` statement, otherwise it
// returns `None`.
pub fn eval_stmts(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    new_bindings: Vec<(Expr, ValRefWithSource)>,
    stmts: &Block,
)
    -> Result<Escape,String>
{
    let mut inner_scopes = scopes.new_from_push(HashMap::new());

    for (lhs, rhs) in new_bindings {
        bind(context, &mut inner_scopes, lhs.clone(), rhs, BindType::VarDeclaration)?;
    }

    eval_stmts_with_scope_stack(context, &mut inner_scopes, stmts)
}

pub fn eval_stmts_with_scope_stack(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    stmts: &Block,
)
    -> Result<Escape,String>
{
    for stmt in stmts {
        let v = eval_stmt(context, scopes, &stmt)?;
        match v {
            Escape::None => {},
            _ => return Ok(v),
        }
    }

    Ok(Escape::None)
}

pub enum Escape {
    None,
    Return(ValRefWithSource),
    Break,
    Continue,
}

// `eval_stmt` returns `Some(v)` if a `return` value is evaluated, otherwise it
// returns `None`.
fn eval_stmt(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    stmt: &Stmt,
)
    -> Result<Escape,String>
{
    match stmt {
        Stmt::Expr{expr} => {
            eval_expr(context, scopes, &expr)?;
        },

        Stmt::Import{path} => {
            // TODO Rename `import_path` to highlight the fact that the import
            // type prefix has been removed.
            let (import_type, import_path) = import::parse_import_path_type(path)?;

            let (alias, module): (String, ValRefWithSource) =
                match import_type {
                    ImportType::Relative{parent_depth} => {
                        eval_relative_import_binding(
                            context,
                            import_path,
                            parent_depth,
                        )?
                    },

                    ImportType::StandardLibrary => {
                        let pkg =
                            match context.builtins.std.get(&import_path) {
                                Some(v) => v,
                                None => return Err(format!("'{}' is not a standard package", import_path)),
                            };

                        // TODO Resolve `import_path` into an alias.

                        (import_path.clone(), pkg.clone())
                    }
                };

            bind(
                context,
                scopes,
                Expr::Var{name: alias},
                module,
                BindType::ConstDeclaration,
            )?;
        },

        Stmt::Declare{lhs, rhs, dt} => {
            let v = eval_expr(context, scopes, &rhs)?;

            let bt =
                match dt {
                    DeclarationType::Const => BindType::ConstDeclaration,
                    DeclarationType::Var => BindType::VarDeclaration,
                };

            // TODO Consider whether `clone()` can be avoided here.
            bind(context, scopes, lhs.clone(), v, bt)?;
        },

        Stmt::Assign{lhs, rhs} => {
            let v = eval_expr(context, scopes, &rhs)?;

            // TODO Consider whether `clone()` can be avoided here.
            bind(context, scopes, lhs.clone(), v, BindType::Assignment)?;
        },

        Stmt::OpAssign{lhs, op, rhs} => {
            let lhs = eval_expr(context, scopes, &lhs)?;

            let rhs = eval_expr(context, scopes, &rhs)?;

            let result = apply_binary_operation(
                &op,
                &lhs.lock().unwrap().v,
                &rhs.lock().unwrap().v,
            )?;

            (*lhs.lock().unwrap()).v = result;
        },

        Stmt::If{branches, else_stmts} => {
            for Branch{cond, stmts} in branches {
                let b = eval_expr_to_bool(context, scopes, &cond, "condition")?;

                if b {
                    return eval_stmts_in_new_scope(context, scopes, &stmts);
                }
            }

            if let Some(stmts) = else_stmts {
                return eval_stmts_in_new_scope(context, scopes, &stmts);
            }
        },

        Stmt::While{cond, stmts} => {
            loop {
                let b = eval_expr_to_bool(context, scopes, &cond, "condition")?;

                if !b {
                    break;
                }

                let escape = eval_stmts_in_new_scope(context, scopes, &stmts)?;
                match escape {
                    Escape::None => {},
                    Escape::Break => break,
                    Escape::Continue => continue,
                    Escape::Return(_) => return Ok(escape),
                }
            }
        },

        Stmt::For{lhs, iter, stmts} => {
            let iter_ = eval_expr(context, scopes, &iter)?;

            let pairs = value_to_pairs(&(*iter_.lock().unwrap()).v)?;

            for (key, value) in pairs {
                let entry = value::new_list(vec![key, value], Mutability::Immutable);

                let new_bindings = vec![(lhs.clone(), entry)];

                let escape = eval_stmts(context, scopes, new_bindings, &stmts)?;
                match escape {
                    Escape::None => {},
                    Escape::Break => break,
                    Escape::Continue => continue,
                    Escape::Return(_) => return Ok(escape),
                }
            }
        },

        Stmt::Break => {
            return Ok(Escape::Break);
        },

        Stmt::Continue => {
            return Ok(Escape::Continue);
        },

        Stmt::Func{name, args, stmts} => {
            let closure = scopes.clone();
            bind_name(
                scopes,
                name.clone(),
                value::new_func(args.clone(), stmts.clone(), closure),
                BindType::VarDeclaration,
            )?;
        },

        Stmt::Return{expr} => {
            let v = eval_expr(context, scopes, expr)?;

            return Ok(Escape::Return(v));
        },
    }

    Ok(Escape::None)
}

// `value_to_pairs` returns the "index, value" pairs in `v`, if `v` represents
// an "iterable" type.
fn value_to_pairs(v: &Value)
    -> Result<Vec<(ValRefWithSource, ValRefWithSource)>, String>
{
    let pairs =
        match v {
            Value::Str(s) =>
                s
                    .iter()
                    .enumerate()
                    .map(|(i, c)| {
                        // TODO Handle issues caused by casting.
                        (value::new_int(i as i64), value::new_str(vec![*c]))
                    })
                    .collect(),

            Value::List{list, ..} =>
                list
                    .iter()
                    .enumerate()
                    .map(|(i, value)| {
                        // TODO Handle issues caused by casting.
                        (value::new_int(i as i64), value.clone())
                    })
                    .collect(),

            Value::Object{props, ..} =>
                props
                    .iter()
                    .map(|(key, value)| {
                        (value::new_str_from_string(key.to_string()), value.clone())
                    })
                    .collect(),

            _ =>
                return Err(format!("iterator must be a list, object or string")),
        };

    Ok(pairs)
}

fn eval_relative_import_binding(
    context: &EvaluationContext,
    import_path: String,
    parent_depth: usize,
)
    -> Result<(String, ValRefWithSource),String>
{
    let rel_path = import::parse_import_path_as_path_buf(&import_path)?;

    // TODO Consider creating an abstraction to highlight the relationship
    // between `import_path` and alias, so that it can also be applied for
    // other types of path-based imports.
    //
    // FIXME Validate the alias.
    let raw_alias =
        match rel_path.file_name() {
            Some(v) => v,
            // TODO Clarify this error message.
            None => return Err(format!("couldn't extract alias from file path")),
        };

    let alias =
        match raw_alias.to_str() {
            Some(v) => v,
            None => return Err(format!("couldn't convert import alias to string")),
        };

    let mut full_path = context.cur_script_dir.to_path_buf();
    for _ in 0..parent_depth {
        if !full_path.pop() {
            // TODO Clarify this error message regarding the fact that path
            // traversal couldn't be performed.
            return Err(format!("invalid import path"));
        }
    }
    // TODO Investigate whether `clone` can be removed here.
    full_path.push(rel_path.clone());

    // TODO Extract `"lrl"` to a dependency-injected variable.
    full_path.set_extension("lrl");

    // TODO Expand `full_path` with regards to symbolic links in order to
    // define a canonical module ID.
    //
    // NOTE The module ID could also be defined as the inode that the module
    // file is associated with, though we avoid this approach for now due to
    // platform-dependence.
    let module_id = full_path.clone();

    let mut maybe_module: Option<ValRefWithSource> = None;
    // TODO Consider refactoring this section to remove the new block.
    {
        let modules = context.modules.lock().unwrap();
        if let Some(m) = modules.get(&module_id) {
            maybe_module = Some(m.clone());
        }
    }

    let module =
        if let Some(module) = maybe_module {
            module.clone()
        } else {
            // TODO Prevent more than one process evaluating a module.

            let contents =
                match import::read_file(&full_path) {
                    Ok(v) => v,
                    Err(e) => return Err(format!("couldn't open import file: {:?}", e)),
                };

            let lexer = Lexer::new(&contents);
            let Prog::Body{stmts} =
                match ProgParser::new().parse(lexer) {
                    Ok(v) => v,
                    Err(e) => return Err(format!("couldn't parse import file: {:?}", e)),
                };

            let top_scope = Arc::new(Mutex::new(HashMap::new()));
            let mut scopes = ScopeStack::new(vec![top_scope.clone()]);

            // TODO This is mostly duplicated from `eval_stmts` and should
            // ideally be de-duplicated when time allows.
            for (lhs, rhs) in context.global_bindings.clone() {
                bind(context, &mut scopes, lhs.clone(), rhs, BindType::VarDeclaration)?;
            }

            let mut sub_script_dir = full_path;
            sub_script_dir.pop();
            eval_stmts_with_scope_stack(
                &EvaluationContext{
                    builtins: context.builtins,
                    global_bindings: context.global_bindings,
                    cur_script_dir: sub_script_dir,
                    modules: context.modules.clone(),
                },
                &mut scopes,
                &stmts,
            )?;

            let mut exports = HashMap::new();

            // TODO The top scope should be discarded at the end of this block,
            // so `clone` is a potentially expensive, yet unnecessary
            // operation.
            for (k, (v, _)) in top_scope.lock().unwrap().iter() {
                // TODO Skip non-exported names.

                // TODO Investigate whether `k.clone()` can be avoided
                // (`v.clone()` is a reference clone).
                exports.insert(k.clone(), v.clone());
            }

            let module = value::new_object(exports, Mutability::Immutable);
            context.modules
                .lock()
                .unwrap()
                .insert(module_id, module.clone());

            module
        };

    Ok((alias.to_string(), module))
}

fn bind(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    lhs: Expr,
    rhs: ValRefWithSource,
    bt: BindType,
)
    -> Result<(),String>
{
    bind_(context, scopes, &mut HashSet::new(), lhs, rhs, bt)
}

fn bind_(
    context: &EvaluationContext,
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

        Expr::List{xs: lhs, is_mutable: lhs_is_mutable} => {
            match &(*rhs.lock().unwrap()).v {
                Value::List{list: rhs, ..} => {
                    // TODO Investigate removing the call to `to_vec()`.
                    bind_list(
                        context,
                        scopes,
                        already_declared,
                        lhs,
                        lhs_is_mutable,
                        rhs.to_vec(),
                        bt,
                    )
                },
                _ => {
                    Err(format!("can't destructure non-list into list"))
                },
            }
        },

        Expr::Index{expr, location} => {
            match_eval_expr!((context, scopes, &expr) {
                Value::List{list, is_mutable} => {
                    if !*is_mutable {
                        return Err(format!("cannot assign to an index of an immutable list"));
                    }

                    let n = eval_expr_to_i64(context, scopes, &location, "index")?;

                    // TODO Handle out-of-bounds assignment.
                    list[n as usize] = rhs;
                },

                Value::Object{props, is_mutable} => {
                    match_eval_expr!((context, scopes, &location) {
                        Value::Str(raw_str) => {
                            if !*is_mutable {
                                return Err(format!("cannot assign to a property of an immutable object"));
                            }

                            // TODO Consider whether non-UTF-8 strings can be
                            // used as keys for objects.
                            let s =
                                match String::from_utf8(raw_str.to_vec()) {
                                    Ok(p) => p,
                                    Err(e) => return Err(format!("couldn't convert key to UTF-8: {}", e))
                                };

                            props.insert(s, rhs);
                        },
                        _ => return Err(format!("key must be a string")),
                    });
                },

                _ => return Err(format!("can only assign to indices of lists and objects")),
            });

            Ok(())
        },

        Expr::Prop{expr, name, prototype} => {
            if prototype {
                return Err(format!("can't assign to prototype properties"));
            }

            match_eval_expr!((context, scopes, &expr) {
                Value::Object{props, is_mutable} => {
                    if !*is_mutable {
                        return Err(format!("cannot assign to a property of an immutable object"));
                    }
                    props.insert(name, rhs);
                },
                _ => {
                    return Err(format!("can only assign to properties of objects"));
                },
            });

            Ok(())
        },

        Expr::Object{props: lhs_props, is_mutable: lhs_is_mutable} => {
            match &(*rhs.lock().unwrap()).v {
                Value::Object{props: rhs_props, ..} => {
                    bind_object(
                        context,
                        scopes,
                        already_declared,
                        lhs_props,
                        lhs_is_mutable,
                        rhs_props.clone(),
                        bt,
                    )
                },
                _ => {
                    Err(format!("can't destructure non-object into object"))
                },
            }
        },

        // TODO Investigate binding to index ranges.
        Expr::IndexRange{..} => Err(format!("cannot bind to an index range")),

        Expr::Range{..} => Err(format!("cannot bind to a range")),
        Expr::Null => Err(format!("cannot bind to `null`")),
        Expr::Bool{..} => Err(format!("cannot bind to a boolean literal")),
        Expr::Int{..} => Err(format!("cannot bind to an integer literal")),
        Expr::Str{..} => Err(format!("cannot bind to a string literal")),
        Expr::InterpolatedStr{..} => Err(format!("cannot bind to an interpolated string literal")),
        Expr::Subcommand{..} => Err(format!("cannot bind to a subcommand")),
        Expr::UnaryOp{..} => Err(format!("cannot bind to a unary operation")),
        Expr::BinaryOp{..} => Err(format!("cannot bind to a binary operation")),
        Expr::Func{..} => Err(format!("cannot bind to a function literal")),
        Expr::Call{..} => Err(format!("cannot bind to a function call")),
        Expr::Spawn{..} => Err(format!("cannot bind to a command spawn")),
        Expr::CatchAsBool{..} => Err(format!("cannot bind to a bool catch")),
        Expr::CatchAsError{..} => Err(format!("cannot bind to an error catch")),
    }
}

#[derive(Clone,Copy)]
enum BindType {
    Assignment,
    ConstDeclaration,
    VarDeclaration,
}

fn bind_name(
    scopes: &mut ScopeStack,
    name: String,
    rhs: ValRefWithSource,
    bind_type: BindType,
)
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

    match bind_type {
        BindType::Assignment => scopes.assign(name.clone(), rhs),
        BindType::ConstDeclaration => scopes.declare(name, rhs, DeclType::Const),
        BindType::VarDeclaration => scopes.declare(name, rhs, DeclType::Var),
    }?;

    Ok(())
}

fn bind_list(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<ListItem>,
    lhs_is_mutable: bool,
    rhs: List,
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
        bind_unspread_list(
            context,
            scopes,
            &mut already_declared,
            lhs,
            lhs_is_mutable,
            rhs,
            bt,
        )
    } else {
        bind_exact_list(context, scopes, &mut already_declared, lhs, rhs, bt)
    }
}

fn bind_unspread_list(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    mut lhs: Vec<ListItem>,
    lhs_is_mutable: bool,
    mut rhs: List,
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

        bind_(context, scopes, &mut already_declared, expr, rhs, bt)?;
    }

    match unspread_expr {
        Expr::Var{name} => {
            let mutability = new_mutability_from_is_mutable(lhs_is_mutable);
            let list = value::new_list(rhs_rest, mutability);

            bind_name_(scopes, &mut already_declared, name, list, bt)
        }
        _ => {
            Err(format!("can only unspread to a variable"))
        }
    }
}

fn bind_exact_list(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<ListItem>,
    rhs: List,
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

        bind_(context, scopes, &mut already_declared, expr, rhs, bt)?;
    }

    Ok(())
}

fn bind_object(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Vec<PropItem>,
    lhs_is_mutable: bool,
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
                        let mutability = new_mutability_from_is_mutable(lhs_is_mutable);
                        let new_rhs = value::new_object(props, mutability);
                        if let Err(e) = bind_(context, scopes, &mut already_declared, lhs, new_rhs, bt) {
                            return Err(format!("couldn't bind '{}': {}", name, e));
                        }
                    } else {
                        return Err(format!("unspread can only appear on last item in object"));
                    }
                } else {
                    rhs_keys.remove(&name);

                    let item_name = Expr::Str{s: name.clone()};
                    let result = bind_object_pair(context, scopes, &mut already_declared, expr, &rhs, &item_name, bt);
                    if let Err(e) = result {
                        return Err(format!("couldn't bind object pair '{}': {}", name, e));
                    }
                }
            },
            PropItem::Pair{name, value: new_lhs} => {
                let raw_prop_name = eval_expr_to_str(context, scopes, &name, "property key")?;

                // TODO Consider whether non-UTF-8 strings can be used as keys
                // for objects.
                let prop_name =
                    match String::from_utf8(raw_prop_name.to_vec()) {
                        Ok(p) => p,
                        Err(e) => return Err(format!("couldn't convert key to UTF-8: {}", e))
                    };

                rhs_keys.remove(&prop_name);

                let result = bind_object_pair(context, scopes, already_declared, new_lhs, &rhs, &name, bt);
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
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    mut already_declared: &mut HashSet<String>,
    lhs: Expr,
    rhs: &HashMap<String,ValRefWithSource>,
    item_name: &Expr,
    bt: BindType,
)
    -> Result<(),String>
{
    let raw_name = eval_expr_to_str(context, scopes, &item_name, "property key")?;

    // TODO Consider whether non-UTF-8 strings can be used as keys for objects.
    let name =
        match String::from_utf8(raw_name.to_vec()) {
            Ok(p) => p,
            Err(e) => return Err(format!("couldn't convert key to UTF-8: {}", e))
        };

    let new_rhs =
        match rhs.get(&name) {
            Some(v) => v.clone(),
            None => return Err(format!("property '{}' not found in source object", name)),
        };

    if let Err(e) = bind_(context, scopes, &mut already_declared, lhs, new_rhs, bt) {
        return Err(format!("couldn't bind '{}': {}", name, e));
    }

    Ok(())
}

fn eval_expr(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    expr: &Expr,
) -> Result<ValRefWithSource,String> {
    match expr {
        Expr::Null => Ok(value::new_null()),

        Expr::Bool{b} => Ok(value::new_bool(b.clone())),

        Expr::Int{n} => Ok(value::new_int(n.clone())),

        Expr::Str{s} => Ok(value::new_str_from_string(s.clone())),

        Expr::InterpolatedStr{s} => {
            let raw_str = s.clone().into_bytes();
            let interpolated_str = interpolate_str(scopes, raw_str)?;

            Ok(value::new_str(interpolated_str))
        },

        Expr::List{xs, is_mutable} => {
            let mut vals = vec![];

            for item in xs {
                let value = eval_expr(context, scopes, &item.expr)?;

                if !item.is_spread {
                    vals.push(value);
                    continue;
                }

                match &(*value.lock().unwrap()).v {
                    Value::List{list, ..} => {
                        for x in list {
                            vals.push(x.clone());
                        }
                    },
                    _ => {
                        return Err(format!("only lists can be spread"));
                    },
                };
            }

            let mutability = new_mutability_from_is_mutable(*is_mutable);

            Ok(value::new_list(vals, mutability))
        },

        Expr::Range{start, end} => {
            let start = eval_expr_to_i64(context, scopes, &start, "range start")?;
            let end = eval_expr_to_i64(context, scopes, &end, "range end")?;
            let range =
                (start..end)
                    .map(|n| value::new_int(n))
                    .collect();

            Ok(value::new_list(range, Mutability::Immutable))
        },

        Expr::Index{expr, location} => {
            let source = eval_expr(context, scopes, expr)?;
            match &(*source.lock().unwrap()).v {
                Value::Str(s) => {
                    let n = eval_expr_to_i64(context, scopes, &location, "index")?;
                    match s.get(n as usize) {
                        Some(v) => return Ok(value::new_str(vec![*v])),
                        None => return Err(format!("index out of bounds")),
                    };
                },

                Value::List{list, ..} => {
                    let n = eval_expr_to_i64(context, scopes, &location, "index")?;
                    match list.get(n as usize) {
                        Some(v) => return Ok(v.clone()),
                        None => return Err(format!("index out of bounds")),
                    };
                },

                Value::Object{props, ..} => {
                    match_eval_expr!((context, scopes, &location) {
                        Value::Str(raw_name) => {
                            // TODO Consider whether non-UTF-8 strings can be
                            // used to perform key lookups on objects.
                            let name =
                                match String::from_utf8(raw_name.to_vec()) {
                                    Ok(p) => p,
                                    Err(e) => return Err(format!("couldn't convert key to UTF-8: {}", e))
                                };

                            match props.get(&name) {
                                Some(v) => {
                                    let prop_val = &(*v.lock().unwrap()).v;
                                    return Ok(value::new_val_ref_with_source(
                                        prop_val.clone(),
                                        source.clone(),
                                    ));
                                },
                                None => {
                                    return Err(format!("property name '{}' not found", name));
                                },
                            };
                        },
                        _ => return Err(format!("property name must be a string")),
                    });
                },

                _ => return Err(format!("can only index lists, objects and strings")),
            };
        },

        Expr::IndexRange{expr, start: maybe_start, end: maybe_end} => {
            match_eval_expr!((context, scopes, &expr) {
                Value::Str(s) => {
                    if let Some(start) = maybe_start {
                        let start = eval_expr_to_i64(context, scopes, &start, "index")?;

                        if let Some(end) = maybe_end {
                            let end = eval_expr_to_i64(context, scopes, &end, "index")?;
                            return get_str_index_range(
                                s,
                                Some(start as usize),
                                Some(end as usize),
                            );
                        }

                        return get_str_index_range(s, Some(start as usize), None);
                    }

                    if let Some(end) = maybe_end {
                        let end = eval_expr_to_i64(context, scopes, &end, "index")?;

                        return get_str_index_range(s, None, Some(end as usize));
                    }
                    return get_str_index_range(s, None, None);
                },

                Value::List{list, ..} => {
                    if let Some(start) = maybe_start {
                        let start = eval_expr_to_i64(context, scopes, &start, "index")?;

                        if let Some(end) = maybe_end {
                            let end = eval_expr_to_i64(context, scopes, &end, "index")?;
                            return get_list_index_range(
                                list,
                                Some(start as usize),
                                Some(end as usize),
                            );
                        }

                        return get_list_index_range(list, Some(start as usize), None);
                    }

                    if let Some(end) = maybe_end {
                        let end = eval_expr_to_i64(context, scopes, &end, "index")?;

                        return get_list_index_range(list, None, Some(end as usize));
                    }
                    return get_list_index_range(list, None, None);
                },

                _ => return Err(format!("can only index range lists and strings")),
            });
        },

        Expr::Prop{expr, name, prototype} => {
            let value = eval_expr(context, scopes, expr)?;
            if *prototype {
                let proto_props = context.builtins.prototypes.prototype_for(
                    &(*value.lock().unwrap()).v,
                )?;

                match proto_props.get(name) {
                    Some(v) => {
                        let proto_prop_val = &(*v.lock().unwrap()).v;

                        Ok(value::new_val_ref_with_source(
                            proto_prop_val.clone(),
                            value.clone(),
                        ))
                    },
                    None => {
                        Err(format!("property name '{}' not found", name))
                    },
                }
            } else {
                match &(*value.lock().unwrap()).v {
                    Value::Object{props, ..} => {
                        match props.get(name) {
                            Some(v) => {
                                let prop_val = &(*v.lock().unwrap()).v;

                                Ok(value::new_val_ref_with_source(
                                    prop_val.clone(),
                                    v.clone(),
                                ))
                            },
                            None => {
                                Err(format!("property name '{}' not found", name))
                            },
                        }
                    },

                    _ => Err(format!("can only access properties of objects")),
                }
            }
        },

        Expr::Subcommand{expr, name} => {
            match_eval_expr!((context, scopes, &expr) {
                Value::Str(raw_prog) => {
                    let args = vec![name.clone()];

                    let prog =
                        match String::from_utf8(raw_prog.to_vec()) {
                            Ok(p) => p,
                            Err(e) => return Err(format!("couldn't convert program name to UTF-8: {}", e))
                        };

                    return Ok(value::new_command(prog, args));
                },

                Value::Command{prog, args} => {
                    let mut args_ = args.clone();
                    args_.push(name.clone());
                    return Ok(value::new_command(prog.clone(), args_));
                },

                _ => return Err(format!("can only add subcommands to strings and commands")),
            });
        },

        Expr::CatchAsBool{expr} => {
            let maybe_value = eval_expr(context, scopes, &expr);
            let (maybe_value, maybe_err) =
                match maybe_value {
                    Ok(v) => (v, value::new_bool(true)),
                    Err(_) => (value::new_null(), value::new_bool(false)),
                };

            Ok(value::new_list(vec![maybe_value, maybe_err], Mutability::Immutable))
        },

        Expr::CatchAsError{expr} => {
            let maybe_value = eval_expr(context, scopes, &expr);
            let (maybe_value, maybe_err) =
                match maybe_value {
                    Ok(v) => (v, value::new_null()),
                    Err(e) => (value::new_null(), value::new_str_from_string(e.to_string())),
                };

            Ok(value::new_list(vec![maybe_value, maybe_err], Mutability::Immutable))
        },

        Expr::Object{props, is_mutable} => {
            let mut vals = HashMap::<String, ValRefWithSource>::new();

            for prop in props {
                match prop {
                    PropItem::Pair{name, value} => {
                        let raw_key = eval_expr_to_str(context, scopes, &name, "property key")?;

                        let v = eval_expr(context, scopes, &value)?;

                        // TODO Consider whether non-UTF-8 strings can be used
                        // as keys for objects.
                        let k =
                            match String::from_utf8(raw_key.to_vec()) {
                                Ok(p) => p,
                                Err(e) => return Err(format!("couldn't convert key to UTF-8: {}", e))
                            };

                        vals.insert(k, v);
                    },
                    PropItem::Single{expr, is_spread, is_unspread} => {
                        if *is_unspread {
                            return Err(format!("can't use unspread operator in object literal"));
                        }

                        if *is_spread {
                            match_eval_expr!((context, scopes, &expr) {
                                Value::Object{props, ..} => {
                                    for (name, value) in props.iter() {
                                        vals.insert(name.to_string(), value.clone());
                                    }
                                },

                                _ => {
                                    return Err(format!("can only spread objects in objects"));
                                },
                            });
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

            let mutability = new_mutability_from_is_mutable(*is_mutable);

            Ok(value::new_object(vals, mutability))
        },

        Expr::UnaryOp{op, expr} => {
            let v = eval_expr(context, scopes, &*expr)?;

            let result = apply_unary_operation(op, &v.lock().unwrap().v)?;

            Ok(value::new_val_ref(result))
        },

        Expr::BinaryOp{op, lhs, rhs} => {
            let exprs = vec![lhs, rhs];

            let mut vals = vec![];
            for expr in exprs {
                let v = eval_expr(context, scopes, &*expr)?;
                vals.push(v);
            }

            let v =
                if let [lhs, rhs] = vals.as_slice() {
                    apply_binary_operation(
                        op,
                        &lhs.lock().unwrap().v,
                        &rhs.lock().unwrap().v,
                    )?
                } else {
                    return Err(format!("dev error: unexpected slice size"));
                };

            Ok(value::new_val_ref(v))
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
            let vals = eval_exprs(context, scopes, &args)?;

            let value = eval_expr(context, scopes, &expr)?;

            let v =
                {
                    let ValWithSource{v, source} = &*value.lock().unwrap();
                    match v {
                        Value::BuiltInFunc{f} => {
                            let this =
                                match source {
                                    Some(v) => Some(v.clone()),
                                    None => None,
                                };

                            CallBinding::BuiltInFunc{
                                f: f.clone(),
                                this,
                                args: vals,
                            }
                        },

                        Value::Func{args: arg_names, stmts, closure} => {
                            if arg_names.len() != vals.len() {
                                return Err(format!("function requires {} arguments, got {}", arg_names.len(), vals.len()))
                            }

                            let mut bindings: Vec<(Expr, ValRefWithSource)> =
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
                                bindings.push((
                                    Expr::Var{name: "this".to_string()},
                                    this.clone(),
                                ));
                            }

                            CallBinding::Func{
                                bindings,
                                closure: closure.clone(),
                                stmts: stmts.clone(),
                            }
                        },

                        _ => return Err(format!("can only call functions")),
                    }
                };

            let v =
                match v {
                    CallBinding::BuiltInFunc{f, this, args} => {
                        f(this, args)?
                    },

                    CallBinding::Func{bindings, closure, stmts} => {
                        let ret_val = eval_stmts(
                            context,
                            &mut closure.clone(),
                            bindings,
                            &stmts,
                        )?;

                        match ret_val {
                            Escape::Return(v) => v,
                            Escape::Break => return Err(format!("`break` outside loop")),
                            Escape::Continue => return Err(format!("`continue` outside loop")),
                            Escape::None => value::new_null(),
                        }
                    },
                };

            Ok(v)
        },

        Expr::Spawn{expr, args} => {
            let vals = eval_exprs(context, scopes, &args)?;

            let value = eval_expr(context, scopes, &expr)?;

            let (prog, args) =
                {
                    let ValWithSource{v, ..} = &*value.lock().unwrap();
                    match v {
                        Value::Str(raw_prog) => {
                            let mut args = vec![];
                            for val in vals {
                                match &(*val.lock().unwrap()).v {
                                    Value::Str(raw_str) => {
                                        // TODO Consider whether non-UTF-8
                                        // strings can be used for program
                                        // arguments.
                                        let s =
                                            match String::from_utf8(raw_str.to_vec()) {
                                                Ok(p) => p,
                                                Err(e) => return Err(format!("couldn't convert program argument to UTF-8: {}", e))
                                            };

                                        args.push(s.clone());
                                    },
                                    _ => {
                                        return Err(format!("program arguments must be strings"));
                                    },
                                }
                            }

                            // TODO Consider whether non-UTF-8 strings can be
                            // used for program names.
                            let prog =
                                match String::from_utf8(raw_prog.to_vec()) {
                                    Ok(p) => p,
                                    Err(e) => return Err(format!("couldn't convert program name to UTF-8: {}", e)),
                                };

                            (prog.clone(), args)
                        },

                        Value::Command{prog, args} => {
                            let mut args_ = args.clone();
                            for val in vals {
                                match &(*val.lock().unwrap()).v {
                                    Value::Str(raw_str) => {
                                        // TODO Consider whether non-UTF-8
                                        // strings can be used as program
                                        // arguments.
                                        let s =
                                            match String::from_utf8(raw_str.to_vec()) {
                                                Ok(p) => p,
                                                Err(e) => return Err(format!("couldn't convert program argument to UTF-8: {}", e)),
                                            };
                                        args_.push(s.clone());
                                    },
                                    _ => {
                                        return Err(format!("program arguments must be strings"))
                                    },
                                }
                            }

                            (prog.clone(), args_)
                        },

                        _ => return Err(format!("can only spawn strings or commands")),
                    }
                };

            let mut cmd = Command::new(prog);
            cmd.args(args);

            match cmd.output() {
                Ok(output) => {
                    let exit_code =
                        match output.status.code() {
                            Some(c) => c as i64,
                            None => return Err(format!("process didn't return exit code")),
                        };

                    if exit_code != 0 {
                        return Err(format!("`{:?}` returned a non-0 exit code", cmd));
                    }

                    let mut props = HashMap::new();

                    props.insert("exit_code".to_string(), value::new_int(exit_code));
                    props.insert("stdout".to_string(), value::new_str(output.stdout));
                    props.insert("stderr".to_string(), value::new_str(output.stderr));

                    Ok(value::new_object(props, Mutability::Immutable))
                },
                Err(e) => {
                    return Err(format!("process failed: {:?}", e));
                },
            }
        },

        Expr::Func{args, stmts} => {
            let closure = scopes.clone();

            Ok(value::new_func(args.clone(), stmts.clone(), closure))
        },

        // _ => Err(format!("unhandled expression: {:?}", expr)),
    }
}

enum CallBinding {
    BuiltInFunc{
        f: fn(Option<ValRefWithSource>, List) -> Result<ValRefWithSource, String>,
        this: Option<ValRefWithSource>,
        args: List,
    },
    Func{
        bindings: Vec<(Expr, ValRefWithSource)>,
        closure: ScopeStack,
        stmts: Block,
    },
}

fn get_str_index_range(
    s: &Str,
    mut maybe_start: Option<usize>,
    mut maybe_end: Option<usize>,
)
    -> Result<ValRefWithSource,String>
{
    let start = maybe_start.get_or_insert(0);
    let end = maybe_end.get_or_insert(s.len());

    if let Some(vs) = s.get(*start .. *end) {
        return Ok(value::new_str(vs.to_vec()));
    }

    Err(format!("index out of bounds"))
}

fn get_list_index_range(
    list: &List,
    mut maybe_start: Option<usize>,
    mut maybe_end: Option<usize>,
)
    -> Result<ValRefWithSource,String>
{
    let start = maybe_start.get_or_insert(0);
    let end = maybe_end.get_or_insert(list.len());

    if let Some(vs) = list.get(*start .. *end) {
        return Ok(value::new_list(vs.to_vec(), Mutability::Immutable));
    }

    Err(format!("index out of bounds"))
}

pub fn eval_exprs(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    exprs: &Vec<Expr>,
)
    -> Result<List, String>
{
    let mut vals = vec![];

    for expr in exprs {
        let v = eval_expr(context, scopes, &expr)?;
        vals.push(v);
    }

    Ok(vals)
}

fn apply_unary_operation(op: &UnaryOp, v: &Value) -> Result<Value,String> {
    match v {
        Value::Bool(b) => {
            match op {
                UnaryOp::Not => Ok(Value::Bool(!b)),
            }
        },
        _ => Err(format!("invalid type: {:?}", v)),
    }
}

fn apply_binary_operation(op: &BinaryOp, lhs: &Value, rhs: &Value)
    -> Result<Value,String>
{
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool(lhs == rhs)),
                BinaryOp::NE => Ok(Value::Bool(lhs != rhs)),
                BinaryOp::GT => Ok(Value::Bool(lhs > rhs)),
                BinaryOp::GTE => Ok(Value::Bool(lhs >= rhs)),
                BinaryOp::LT => Ok(Value::Bool(lhs < rhs)),
                BinaryOp::LTE => Ok(Value::Bool(lhs <= rhs)),

                BinaryOp::Sum => Ok(Value::Int(lhs + rhs)),
                BinaryOp::Sub => Ok(Value::Int(lhs - rhs)),
                BinaryOp::Mul => Ok(Value::Int(lhs * rhs)),
                BinaryOp::Div => Ok(Value::Int(lhs / rhs)),
                BinaryOp::Mod => Ok(Value::Int(lhs % rhs)),

                _ => Err(format!("unsupported operation for integers ({:?})", op))
            }
        },
        (Value::Bool(lhs), Value::Bool(rhs)) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool(lhs == rhs)),
                BinaryOp::NE => Ok(Value::Bool(lhs != rhs)),
                BinaryOp::And => Ok(Value::Bool(*lhs && *rhs)),
                BinaryOp::Or => Ok(Value::Bool(*lhs || *rhs)),

                _ => Err(format!("unsupported operation for booleans ({:?})", op))
            }
        },
        (Value::Str(lhs), Value::Str(rhs)) => {
            match op {
                BinaryOp::EQ => {
                    Ok(Value::Bool(lhs == rhs))
                },
                BinaryOp::Sum => {
                    let mut bytes = lhs.clone();
                    bytes.extend(rhs);

                    Ok(Value::Str(bytes))
                },
                _ => {
                    Err(format!("unsupported operation for strings ({:?})", op))
                },
            }
        },
        (
            Value::List{list: lhs, is_mutable: lhs_is_mut},
            Value::List{list: rhs, is_mutable: rhs_is_mut},
        ) => {
            match op {
                BinaryOp::Sum => {
                    if lhs_is_mut != rhs_is_mut {
                        return Err(format!("cannot concatenate mutable and immutable lists"));
                    }

                    let mut list = vec![];
                    for v in lhs {
                        list.push(v.clone());
                    }
                    for v in rhs {
                        list.push(v.clone());
                    }
                    Ok(Value::List{list, is_mutable: *lhs_is_mut})
                },

                _ => Err(format!("unsupported operation for lists ({:?})", op))
            }
        },

        (Value::Null, _) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool(is_null(rhs))),
                BinaryOp::NE => Ok(Value::Bool(!is_null(rhs))),

                _ => Err(format!("unsupported operation for `null` ({:?})", op))
            }
        },

        (_, Value::Null) => {
            match op {
                BinaryOp::EQ => Ok(Value::Bool(is_null(lhs))),
                BinaryOp::NE => Ok(Value::Bool(!is_null(lhs))),

                _ => Err(format!("unsupported operation for `null` ({:?})", op))
            }
        },

        _ => Err(format!("invalid types: {:?}", (lhs, rhs))),
    }
}

fn is_null(v: &Value) -> bool {
    if let Value::Null = v {
        return true
    }
    return false
}

fn eval_expr_to_bool(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    expr: &Expr,
    name: &str,
)
    -> Result<bool, String>
{
    match_eval_expr!((context, scopes, expr) {
        Value::Bool(b) => Ok(*b),
        _ => Err(format!("{} must be a boolean", name)),
    })
}

fn eval_expr_to_i64(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    expr: &Expr,
    name: &str,
)
    -> Result<i64, String>
{
    match_eval_expr!((context, scopes, expr) {
        Value::Int(n) => Ok(*n),
        _ => Err(format!("{} must be an integer", name)),
    })
}

fn eval_expr_to_str(
    context: &EvaluationContext,
    scopes: &mut ScopeStack,
    expr: &Expr,
    name: &str,
)
    -> Result<Str, String>
{
    match_eval_expr!((context, scopes, expr) {
        Value::Str(s) => Ok(s.clone()),
        _ => Err(format!("{} must be a string", name)),
    })
}

fn interpolate_str(scopes: &mut ScopeStack, s: Vec<u8>) -> Result<Str, String> {
    // TODO Add the string index to error strings.

    let mut interpolated_str: Vec<u8> = vec![];

    let open_brace: u8 = 0x7b;
    let close_brace: u8 = 0x7d;
    let dollar: u8 = 0x24;

    let mut scanner = s.iter();

    while let Some(&c) = scanner.next() {
        if c == open_brace {
            let next_c =
                if let Some(&c) = scanner.next() {
                    c
                } else {
                    return Err(format!("unexpected end-of-string when starting interpolation"))
                };

            if next_c == open_brace {
                interpolated_str.push(next_c);
            } else if next_c == dollar {
                let mut raw_ident = vec![];
                loop {
                    if let Some(&ident_c) = scanner.next() {
                        if ident_c == close_brace {
                            break;
                        }

                        // TODO Validate `ident_c` is a valid identifier character.
                        raw_ident.push(ident_c);
                    } else {
                        return Err(format!("unexpected end-of-string when ending interpolation"));
                    }
                }

                let ident =
                    match String::from_utf8(raw_ident.to_vec()) {
                        Ok(s) => s,
                        Err(e) => return Err(format!("couldn't convert identifier to UTF-8: {}", e))
                    };

                let v =
                    if let Some(v) = scopes.get(&ident) {
                        v
                    } else {
                        // TODO Standardise the wording of this error with
                        // similar errors.
                        return Err(format!("'{}' is not defined", ident));
                    };

                // TODO Handle non-`Str` types.
                match &(*v.lock().unwrap()).v {
                    Value::Str(s) => interpolated_str.append(&mut s.clone()),
                    _ => return Err(format!("only strings are currently allowed as interpolation variables")),
                };
            } else {
                // TODO Render argument as character.
                return Err(format!("expected '{{' or '$', got '{}'", next_c));
            }
        } else if c == close_brace {
            if let Some(&next_c) = scanner.next() {
                if next_c == close_brace {
                    interpolated_str.push(next_c);
                } else {
                    // TODO Render argument as character.
                    return Err(format!("expected '}}', got '{}'", next_c));
                }
            } else {
                return Err(format!("expected '}}', got end-of-string"));
            };
        } else {
            interpolated_str.push(c);
        }
    }

    Ok(interpolated_str)
}

fn new_mutability_from_is_mutable(is_mutable: bool) -> Mutability {
    if is_mutable {
        Mutability::Mutable
    } else {
        Mutability::Immutable
    }
}

pub struct EvaluationContext<'a> {
    pub builtins: &'a Builtins,
    pub cur_script_dir: PathBuf,

    // TODO Consider grouping `global_bindings` with `builtins`.
    pub global_bindings: &'a Vec<(Expr, ValRefWithSource)>,

    // NOTE The value referenced by `ValRefWithSource`, i.e. the module value,
    // should always be an object. This is difficult to enforce at the
    // type-level, and so it should ideally be enforced at the point where
    // modules are added to the map.
    pub modules: Arc<Mutex<HashMap<ModuleId, ValRefWithSource>>>,
}
