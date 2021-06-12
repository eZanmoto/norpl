// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

extern crate assert_cmd;

use self::assert_cmd::Command as AssertCommand;

#[test]
// NOCOMMIT Rename `x`.
fn x() {
    // let src = "print(1);";
    let tgt = "1";
    // NOCOMMIT Output `src` to `tests/print.npl`.
    let mut cmd = new_test_cmd("tests/print.npl".to_string());

    let cmd_result = cmd.assert();

    cmd_result
        .code(0)
        .stdout(tgt)
        .stderr("");
}

pub fn new_test_cmd(src_file: String) -> AssertCommand {
    let mut cmd = AssertCommand::cargo_bin(env!("CARGO_PKG_NAME"))
        .expect("couldn't create command for package binary");
    cmd.env_clear();
    cmd.arg(src_file);

    cmd
}
