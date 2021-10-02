// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

mod ast;
mod eval;

use ast::Expr;
use eval::ScopeStack;
use eval::ValRefWithSource;
use eval::Value;
use parser::ProgParser;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(clippy::pedantic)]
    #[allow(dead_code)]
    parser
);

fn main() {
    let test = read_test();

    let global_bindings = vec![
        (Expr::Var{name: "print".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: print_})),
        (Expr::Var{name: "len".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: len_})),
        (Expr::Var{name: "type".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: type_})),
    ];

    let mut scopes = ScopeStack::new(vec![]);
    let ast = ProgParser::new().parse(&test).unwrap();
    let result = eval::eval_prog(&mut scopes, global_bindings, &ast);

    println!("{:?}", result);
}

fn read_test() -> String {
    let f = File::open("src/test.txt").unwrap();
    let lines = BufReader::new(f).lines();
    let mut test = String::new();

    for s in lines {
        test.push_str(&s.unwrap());
    }

    test
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn print_(vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Remove varargs support.
    println!("{:?}", vs[0].v);

    Ok(eval::new_val_ref(Value::Null))
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn len_(vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &*vs[0].v.lock().unwrap() {
        Value::List{xs} => {
            // TODO Investigate casting `usize` to `i64`.
            let n = Value::Int{n: xs.len() as i64};

            Ok(eval::new_val_ref(n))
        },
        _ => {
            Err(format!("can only call `len` on lists"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn type_(vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    let t =
        match &*vs[0].v.lock().unwrap() {
            Value::Null => "null",
            Value::Bool{..} => "bool",
            Value::Int{..} => "int",
            Value::Str{..} => "str",
            Value::List{..} => "list",
            Value::Object{..} => "object",
            Value::BuiltInFunc{..} => "fn",
            Value::Func{..} => "fn",
        };

    Ok(eval::new_val_ref(Value::Str{s: t.to_string()}))
}
