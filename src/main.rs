// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::sync::Arc;
use std::sync::Mutex;

mod ast;
mod eval;

use eval::ScopeStack;
use eval::Value;

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

    let mut global_frame: HashMap<String, Value> = HashMap::new();
    global_frame.insert("print".to_string(), Value::BuiltInFunc{f: print_});

    let mut scopes = ScopeStack::new(vec![Arc::new(Mutex::new(global_frame))]);
    let ast = parser::ProgParser::new().parse(&test).unwrap();
    let result = eval::eval_prog(&mut scopes, &ast);

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
fn print_(vs: Vec<Value>) -> Result<Value, String> {
    // TODO Remove varargs support.
    println!("{:?}", vs);

    Ok(Value::Null)
}
