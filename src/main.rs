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
mod builtins;
mod eval;

use ast::Expr;
use builtins::fns;
use builtins::prototypes;
use eval::Builtins;
use eval::ScopeStack;
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
        (Expr::Var{name: "panic".to_string()}, eval::new_built_in_func(fns::panic_)),
        (Expr::Var{name: "print".to_string()}, eval::new_built_in_func(fns::print_)),
        (Expr::Var{name: "len".to_string()}, eval::new_built_in_func(fns::len_)),
        (Expr::Var{name: "type".to_string()}, eval::new_built_in_func(fns::type_)),
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
