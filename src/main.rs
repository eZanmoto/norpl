// Copyright 2021-2022 Sean Kelleher. All rights reserved.
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
mod builtins;
mod eval;
mod lexer;

use ast::Expr;
use builtins::fns;
use builtins::prototypes;
use eval::value;
use eval::builtins::Builtins;
use eval::value::ScopeStack;
use lexer::Lexer;
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
        (Expr::Var{name: "panic".to_string()}, value::new_built_in_func(fns::panic_)),
        (Expr::Var{name: "print".to_string()}, value::new_built_in_func(fns::print_)),
        (Expr::Var{name: "len".to_string()}, value::new_built_in_func(fns::len_)),
        (Expr::Var{name: "type".to_string()}, value::new_built_in_func(fns::type_)),
    ];

    let std = HashMap::<_, _>::from_iter(IntoIter::new([
        (
            "proc".to_string(),
            value::new_object(HashMap::<_, _>::from_iter(IntoIter::new([
                (
                    "env".to_string(),
                    value::new_object(HashMap::<_, _>::from_iter(
                        env::vars()
                            .map(|(k, v)| (k, value::new_str_from_string(v))),
                    )),
                ),

                (
                    "args".to_string(),
                    value::new_list(vec![
                        value::new_str_from_string("--dry-run".to_string()),
                        value::new_str_from_string("--mirror".to_string()),
                        value::new_str_from_string("abc".to_string()),
                        value::new_str_from_string("--mirror".to_string()),
                        value::new_str_from_string("Aliyun".to_string()),
                    ]),
                ),
            ]))),
        ),
    ]));

    let prototypes = prototypes::prototypes();

    let mut scopes = ScopeStack::new(vec![]);
    let lexer = Lexer::new(&test);
    let ast = ProgParser::new().parse(lexer).unwrap();
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
        test.push_str(&(s.unwrap() + "\n"));
    }

    test
}
