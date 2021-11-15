// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::array::IntoIter;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::FromIterator;

mod ast;
mod eval;
mod prototypes;

use ast::Expr;
use eval::Builtins;
use eval::List;
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
        (Expr::Var{name: "panic".to_string()}, eval::new_built_in_func(panic_)),
        (Expr::Var{name: "print".to_string()}, eval::new_built_in_func(print_)),
        (Expr::Var{name: "len".to_string()}, eval::new_built_in_func(len_)),
        (Expr::Var{name: "type".to_string()}, eval::new_built_in_func(type_)),
    ];

    let std = HashMap::<_, _>::from_iter(IntoIter::new([
        (
            "proc".to_string(),
            eval::new_object(HashMap::<_, _>::from_iter(IntoIter::new([
                (
                    "env".to_string(),
                    eval::new_object(HashMap::<_, _>::from_iter(
                        env::vars()
                            .map(|(k, v)| (k, eval::new_str(v))),
                    )),
                ),

                (
                    "args".to_string(),
                    eval::new_list(vec![
                        eval::new_str("--dry-run".to_string()),
                        eval::new_str("--mirror".to_string()),
                        eval::new_str("abc".to_string()),
                        eval::new_str("--mirror".to_string()),
                        eval::new_str("Aliyun".to_string()),
                    ]),
                ),
            ]))),
        ),
    ]));

    let prototypes = prototypes::prototypes();

    let mut scopes = ScopeStack::new(vec![]);
    let ast = ProgParser::new().parse(&test).unwrap();
    let result = eval::eval_prog(
        &mut scopes,
        global_bindings,
        &Builtins{std, prototypes},
        &ast,
    );

    println!("{:?}", result);
}

fn read_test() -> String {
    let f = File::open("src/docker_get.lrl").unwrap();
    let lines = BufReader::new(f).lines();
    let mut test = String::new();

    for s in lines {
        test.push_str(&s.unwrap());
    }

    test
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn panic_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &(*vs[0].lock().unwrap()).v {
        Value::Str{s} => {
            Err(s.to_string())
        },
        _ => {
            Err(format!("can only call `panic` on strings"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn print_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Remove varargs support.
    println!("{}", render(vs[0].clone()));

    Ok(eval::new_null())
}

fn render(v: ValRefWithSource) -> String {
    let mut s = String::new();

    match &v.lock().unwrap().v {
        Value::Null => {
            s += "null";
        },
        Value::Bool(b) => {
            s += &format!("{}", b);
        },
        Value::Int{n} => {
            s += &format!("{}", n);
        },
        Value::Str{s: s_} => {
            s += &format!("'{}'", s_);
        },
        Value::BuiltInFunc{..} => {
            s += "<built-in function>";
        },
        Value::Func{..} => {
            s += "<user-defined function>";
        },
        Value::List{xs} => {
            s += "[\n";
            for x in xs {
                s += &format!("    {},\n", render(x.clone()));
            }
            s += "]";
        },
        Value::Object{props} => {
            s += "{\n";
            for (name, value) in props {
                s += &format!("    '{}': {},\n", name, render(value.clone()));
            }
            s += "}";
        },
        Value::Command{prog, args} => {
            s += &format!("'{}'({:?})", prog, args);
        },
    }

    s.to_string()
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn len_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &(*vs[0].lock().unwrap()).v {
        Value::List{xs} => {
            // TODO Investigate casting `usize` to `i64`.
            Ok(eval::new_int(xs.len() as i64))
        },
        _ => {
            Err(format!("can only call `len` on lists"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn type_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    let t =
        match &(*vs[0].lock().unwrap()).v {
            Value::Null => "null",
            Value::Bool{..} => "bool",
            Value::Int{..} => "int",
            Value::Str{..} => "str",
            Value::List{..} => "list",
            Value::Object{..} => "object",
            Value::BuiltInFunc{..} => "fn",
            Value::Func{..} => "fn",
            Value::Command{..} => "cmd",
        };

    Ok(eval::new_str(t.to_string()))
}
