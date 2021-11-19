// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::array::IntoIter;
use std::collections::HashMap;
use std::iter::FromIterator;

use crate::eval;
use crate::eval::List;
use crate::eval::Prototypes;
use crate::eval::ValRefWithSource;
use crate::eval::Value;

pub fn prototypes() -> Prototypes {
    Prototypes{
        bools: HashMap::<String, ValRefWithSource>::new(),
        ints: HashMap::<String, ValRefWithSource>::new(),

        strs: HashMap::<_, _>::from_iter(IntoIter::new([
            (
                "strip_prefix".to_string(),
                eval::new_built_in_func(strip_prefix_),
            ),
            (
                "starts_with".to_string(),
                eval::new_built_in_func(starts_with_),
            ),
        ])),

        lists: HashMap::<_, _>::from_iter(IntoIter::new([
            (
                "len".to_string(),
                eval::new_built_in_func(list_len_),
            ),
        ])),

        objects: HashMap::<String, ValRefWithSource>::new(),
        funcs: HashMap::<String, ValRefWithSource>::new(),
        commands: HashMap::<String, ValRefWithSource>::new(),
    }
}

// NOCOMMIT Resolve Clippy issues.
#[allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
fn strip_prefix_(this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    // TODO Handle out-of-bounds access.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::Str(unstripped) => {
            match &(*vs[0].lock().unwrap()).v {
                Value::Str(prefix) => {
                    let stripped =
                        match unstripped.strip_prefix(prefix) {
                            Some(s) => s,
                            None => unstripped,
                        };

                    Ok(eval::new_str(stripped.to_string()))
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
fn starts_with_(this: Option<ValRefWithSource>, vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    // TODO Handle out-of-bounds access.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::Str(s) => {
            match &(*vs[0].lock().unwrap()).v {
                Value::Str(prefix) => {
                    Ok(eval::new_bool(s.starts_with(prefix)))
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
fn list_len_(this: Option<ValRefWithSource>, _vs: List) -> Result<ValRefWithSource, String> {
    // TODO Handle `unwrap` on a "none" `this`.
    match &(*this.unwrap().lock().unwrap()).v {
        Value::List(xs) => {
            // TODO Investigate casting `usize` to `i64`.
            Ok(eval::new_int(xs.len() as i64))
        },
        _ => {
            // TODO Consider whether to use a `panic!` here.
            Err(format!("dev error: `list_len_` called on non-list"))
        },
    }
}
