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

use ast::Expr;
use eval::Builtins;
use eval::Prototypes;
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
        (Expr::Var{name: "panic".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: panic_})),
        (Expr::Var{name: "print".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: print_})),
        (Expr::Var{name: "len".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: len_})),
        (Expr::Var{name: "type".to_string()}, eval::new_val_ref(Value::BuiltInFunc{f: type_})),
    ];

    let std = HashMap::<_, _>::from_iter(IntoIter::new([
        (
            "proc".to_string(),
            eval::new_val_ref(Value::Object{
                props: HashMap::<_, _>::from_iter(IntoIter::new([
                    (
                        "env".to_string(),
                        eval::new_val_ref(Value::Object{
                            props: HashMap::<_, _>::from_iter(
                                env::vars()
                                    .map(|(k, v)| (k, eval::new_val_ref(Value::Str{s: v}))),
                            ),
                        },
                    )),

                    (
                        "args".to_string(),
                        eval::new_val_ref(Value::List{xs: vec![
                            eval::new_val_ref(Value::Str{s: "--dry-run".to_string()}),
                            eval::new_val_ref(Value::Str{s: "--mirror".to_string()}),
                            eval::new_val_ref(Value::Str{s: "abc".to_string()}),
                            eval::new_val_ref(Value::Str{s: "--mirror".to_string()}),
                            eval::new_val_ref(Value::Str{s: "123".to_string()}),
                        ]},
                    )),
                ])),
            }),
        ),
    ]));

    let prototypes = Prototypes{
        bools: HashMap::<String, ValRefWithSource>::new(),
        ints: HashMap::<String, ValRefWithSource>::new(),

        strs: HashMap::<_, _>::from_iter(IntoIter::new([
            (
                "strip_prefix".to_string(),
                eval::new_val_ref(Value::BuiltInFunc{f: strip_prefix_}),
            ),
            (
                "starts_with".to_string(),
                eval::new_val_ref(Value::BuiltInFunc{f: starts_with_}),
            ),
        ])),

        lists: HashMap::<_, _>::from_iter(IntoIter::new([
            (
                "len".to_string(),
                eval::new_val_ref(Value::BuiltInFunc{f: list_len_}),
            ),
        ])),

        objects: HashMap::<String, ValRefWithSource>::new(),
        funcs: HashMap::<String, ValRefWithSource>::new(),
        commands: HashMap::<String, ValRefWithSource>::new(),
    };

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
fn panic_(_this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
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
fn print_(_this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Remove varargs support.
    println!("{}", render(vs[0].clone()));

    Ok(eval::new_val_ref(Value::Null))
}

fn render(v: ValRefWithSource) -> String {
    let mut s = String::new();

    match &v.lock().unwrap().v {
        Value::Null => {
            s += "null";
        },
        Value::Bool{b} => {
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
fn len_(_this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &(*vs[0].lock().unwrap()).v {
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
fn type_(_this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
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

    Ok(eval::new_val_ref(Value::Str{s: t.to_string()}))
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn strip_prefix_(this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    // TODO Handle out-of-bounds access.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::Str{s: unstripped} => {
            match &(*vs[0].lock().unwrap()).v {
                Value::Str{s: prefix} => {
                    let stripped =
                        match unstripped.strip_prefix(prefix) {
                            Some(s) => s,
                            None => unstripped,
                        };

                    Ok(eval::new_val_ref(Value::Str{s: stripped.to_string()}))
                },
                _ => {
                    Err(format!("the first argument to `strip_prefix` must be a string"))
                },
            }
        }
        _ => {
            Err(format!("can only call `strip_prefix` on strings"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn starts_with_(this: Option<ValRefWithSource>, vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    // TODO Handle out-of-bounds access.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::Str{s} => {
            match &(*vs[0].lock().unwrap()).v {
                Value::Str{s: prefix} => {
                    Ok(eval::new_val_ref(Value::Bool{b: s.starts_with(prefix)}))
                },
                _ => {
                    Err(format!("the first argument to `starts_with` must be a string"))
                },
            }
        }
        _ => {
            Err(format!("dev error: `starts_with_` called on non-string"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn list_len_(this: Option<ValRefWithSource>, _vs: Vec<ValRefWithSource>) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::List{xs} => {
            // TODO Investigate casting `usize` to `i64`.
            let n = Value::Int{n: xs.len() as i64};

            Ok(eval::new_val_ref(n))
        },
        _ => {
            // TODO Consider whether to use a `panic!` here.
            Err(format!("dev error: `list_len_` called on non-list"))
        },
    }
}
