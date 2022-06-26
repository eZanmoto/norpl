// Copyright 2021-2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::iter::FromIterator;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

mod ast;
mod builtins;
mod eval;
mod lexer;

use ast::Expr;
use builtins::fns;
use builtins::prototypes;
use eval::builtins::Builtins;
use eval::EvaluationContext;
use eval::value;
use eval::value::Mutability;
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
    let cur_rel_script_path = Path::new("src/docker_get.lrl");

    // FIXME Remove `unwrap()`.
    let cwd = env::current_dir().unwrap().to_path_buf();

    let mut cur_script_path = cwd;
    cur_script_path.push(cur_rel_script_path);

    let test = read_test(&cur_script_path);

    let global_bindings = vec![
        (Expr::Var{name: "panic".to_string()}, value::new_built_in_func(fns::panic_)),
        (Expr::Var{name: "print".to_string()}, value::new_built_in_func(fns::print_)),
        (Expr::Var{name: "len".to_string()}, value::new_built_in_func(fns::len_)),
        (Expr::Var{name: "type".to_string()}, value::new_built_in_func(fns::type_)),
    ];

    let std = HashMap::<_, _>::from_iter(IntoIterator::into_iter([
        (
            "proc".to_string(),
            value::new_object(
                HashMap::<_, _>::from_iter(IntoIterator::into_iter([
                    (
                        "env".to_string(),
                        value::new_object(
                            HashMap::<_, _>::from_iter(
                                env::vars()
                                    .map(|(k, v)| (k, value::new_str_from_string(v))),
                            ),
                            Mutability::Immutable,
                        ),
                    ),

                    (
                        "args".to_string(),
                        value::new_list(
                            vec![
                                value::new_str_from_string("--dry-run".to_string()),
                                value::new_str_from_string("--mirror".to_string()),
                                value::new_str_from_string("abc".to_string()),
                                value::new_str_from_string("--mirror".to_string()),
                                value::new_str_from_string("Aliyun".to_string()),
                            ],
                            Mutability::Immutable,
                        ),
                    ),
                ])),
                Mutability::Immutable,
            ),
        ),
    ]));

    let prototypes = prototypes::prototypes();

    let mut cur_script_dir = cur_script_path;
    cur_script_dir.pop();

    let mut scopes = ScopeStack::new(vec![]);
    let lexer = Lexer::new(&test);
    let ast = ProgParser::new().parse(lexer).unwrap();
    let result = eval::eval_prog(
        &EvaluationContext{
            builtins: &Builtins{std, prototypes},
            global_bindings: &global_bindings,
            cur_script_dir: cur_script_dir,
            modules: Arc::new(Mutex::new(HashMap::new())),
        },
        &mut scopes,
        global_bindings.clone(),
        &ast,
    );

    println!("{:?}", result);
}

fn read_test(path: &Path) -> String {
    let mut f = File::open(&path).unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf);
    let s = String::from_utf8(buf).unwrap();

    s
}
