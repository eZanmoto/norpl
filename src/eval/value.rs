// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use ast::*;

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

// `ValRefWithSource` is intended to be used as a regular `ValRef` would, but
// it includes the most recent object it was referenced from. For example, in
// the case of `x['f']`, the `ValRef` is the value stored at the location
// `'f'`, and the `source` of this value is `x`.
pub type ValRefWithSource = Arc<Mutex<ValWithSource>>;

#[derive(Clone,Debug)]
pub struct ValWithSource {
    pub v: Value,
    pub source: Option<ValRefWithSource>,
}

#[derive(Clone,Debug)]
pub enum Value {
    Null,

    Bool(bool),
    Int(i64),
    Str(String),
    List(List),
    Object{props: Object},

    BuiltInFunc{f: fn(Option<ValRefWithSource>, List) -> Result<ValRefWithSource, String>},
    Func{args: Vec<Expr>, stmts: Block, closure: ScopeStack},

    Command{prog: String, args: Vec<String>},
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

    pub fn new_from_push(&self, scope: Scope) -> ScopeStack {
        let mut scopes = self.0.clone();
        scopes.push(Arc::new(Mutex::new(scope)));

        ScopeStack::new(scopes)
    }

    pub fn declare(&mut self, name: String, v: ValRefWithSource, decl_type: DeclType)
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
    pub fn assign(&mut self, name: String, v: ValRefWithSource) -> Result<(),String> {
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

    pub fn get(&self, name: &String) -> Option<ValRefWithSource> {
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

pub type List = Vec<ValRefWithSource>;

pub type Object = HashMap<String, ValRefWithSource>;

pub fn new_null() -> ValRefWithSource {
    new_val_ref(Value::Null)
}

pub fn new_bool(b: bool) -> ValRefWithSource {
    new_val_ref(Value::Bool(b))
}

pub fn new_int(n: i64) -> ValRefWithSource {
    new_val_ref(Value::Int(n))
}

pub fn new_str(s: String) -> ValRefWithSource {
    new_val_ref(Value::Str(s))
}

pub fn new_list(xs: List) -> ValRefWithSource {
    new_val_ref(Value::List(xs))
}

pub fn new_object(props: Object) -> ValRefWithSource {
    new_val_ref(Value::Object{props})
}

pub fn new_built_in_func(f: fn(Option<ValRefWithSource>, List) -> Result<ValRefWithSource, String>)
    -> ValRefWithSource
{
    new_val_ref(Value::BuiltInFunc{f})
}

pub fn new_func(args: Vec<Expr>, stmts: Block, closure: ScopeStack) -> ValRefWithSource {
    new_val_ref(Value::Func{args, stmts, closure})
}

pub fn new_command(prog: String, args: Vec<String>) -> ValRefWithSource {
    new_val_ref(Value::Command{prog, args})
}
