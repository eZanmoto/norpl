// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use crate::eval;
use crate::eval::List;
use crate::eval::ValRefWithSource;
use crate::eval::Value;

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
pub fn panic_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &(*vs[0].lock().unwrap()).v {
        Value::Str(s) => {
            Err(s.to_string())
        },
        _ => {
            Err(format!("can only call `panic` on strings"))
        },
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
pub fn print_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
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
        Value::Int(n) => {
            s += &format!("{}", n);
        },
        Value::Str(s_) => {
            s += &format!("'{}'", s_);
        },
        Value::BuiltInFunc{..} => {
            s += "<built-in function>";
        },
        Value::Func{..} => {
            s += "<user-defined function>";
        },
        Value::List(xs) => {
            s += "[\n";
            for x in xs {
                s += &format!("    {},\n", render(x.clone()));
            }
            s += "]";
        },
        Value::Object(props) => {
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
pub fn len_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle out-of-bounds access.
    match &(*vs[0].lock().unwrap()).v {
        Value::List(xs) => {
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
pub fn type_(_this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
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
