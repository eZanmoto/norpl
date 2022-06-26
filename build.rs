// Copyright 2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::env;
use std::fs;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::ErrorKind;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

#[macro_use]
extern crate indoc;
extern crate lalrpop;

// TODO This is the first, functional version of this file, which hasn't been
// refactored with best practices (e.g. dependency injection, and verifyng
// abstractions). This should be done before any additional work is applied to
// the file.
fn main() {
    lalrpop::process_root().unwrap();

    // TODO Consider returning an error from `gen_tests` instead of `panic`ing.
    gen_tests();
}

fn gen_tests() {
    let raw_out_dir = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&raw_out_dir);
    let dest = out_dir.join("tests.rs");
    let mut test_file = File::create(&dest)
        .expect("couldn't create test file");

    let test_dir = out_dir.join("tests");
    // TODO Consider removing old test directories on each run.

    // Consider creating the test directory when running generated tests, as
    // opposed to the time of generating the tests.
    if let Err(e) = fs::create_dir(&test_dir) {
        if e.kind() != ErrorKind::AlreadyExists {
            panic!("couldn't create test directory: {}", e);
        }
    }

    write_header(&mut test_file);

    let entries = fs::read_dir("tests/stdout")
        .expect("couldn't read test directory");

    for maybe_entry in entries {
        let entry = maybe_entry
            .expect("couldn't read test directory entry");

        let file_type = entry.file_type()
            .expect("couldn't get file type");

        let entry_path = entry.path();
        if !file_type.is_file() {
            panic!("'{}' isn't a file", entry_path.display());
        }

        if let Some(ext) = entry_path.extension() {
            if ext != "test" {
                continue;
            }
        } else {
            continue;
        }

        let entry_stem_raw = entry_path.file_stem()
            .expect("couldn't extract file stem from path");

        let entry_stem = entry_stem_raw.to_str()
            .expect("file stem contains invalid UTF-8");

        writedoc!(
            test_file,
            "
                mod {mod_name} {{
                    use super::*;
            ",
            mod_name = entry_stem,
        )
            .expect("couldn't write test file module start");

        for test in extract_tests(entry_path) {
            write_test(&test_dir, &mut test_file, &test);
        }

        write!(test_file, "\n}}\n")
            .expect("couldn't write test file module end");
    }
}

fn extract_tests(entry_path: PathBuf) -> Vec<Test> {
    let f = File::open(entry_path)
        .expect("couldn't open test file");

    let mut tests = vec![];
    let mut end_matched = false;
    let mut cur_test: Option<Test> = None;
    let mut reading_src = true;
    for maybe_line in BufReader::new(f).lines() {
        let line = maybe_line
            .expect("couldn't read line from test file");

        if end_matched {
            panic!("extra lines discovered after closing test marker");
        }

        let suffix =
            if let Some(suf) = line.strip_prefix(TEST_MARKER_START) {
                suf
            } else if line == TEST_MARKER_OUTPUT {
                reading_src = false;
                continue;
            } else {
                let mut test = cur_test.take()
                    .expect("lines discovered before first test marker");

                if reading_src {
                    test.src += &(line + "\n");
                } else {
                    test.tgt += &(line + "\n");
                }

                cur_test.replace(test);
                continue;
            };

        if suffix.is_empty() {
            if let Some(t) = cur_test.take() {
                tests.push(t);
            } else {
                panic!("no tests defined");
            }

            end_matched = true;
            continue;
        }

        let test_name =
            if let Some(suf) = suffix.strip_prefix(" ") {
                suf
            } else {
                panic!("expected space before test name");
            };

        if let Some(t) = cur_test.take() {
            if reading_src {
                panic!("expected output not defined for test '{}'", t.name);
            }

            tests.push(t);
        }

        cur_test = Some(Test{
            name: String::from(test_name),
            src: String::from(""),
            tgt: String::from(""),
        });
        reading_src = true;
    }

    if !end_matched {
        panic!("test file didn't end with closing test marker");
    }

    tests
}

#[derive(Clone)]
struct Test {
    name: String,
    src: String,
    tgt: String,
}

const TEST_MARKER_START: &str =
    "==================================================";

const TEST_MARKER_OUTPUT: &str =
    "--------------------------------------------------";

fn write_header(test_file: &mut File) {
    let header = indoc!{"
        use std::fs;

        use crate::assert_cmd::Command;

        struct Test {
            src: String,
            exp: TestExpectation,
        }

        struct TestExpectation {
            code: i32,
            stdout: String,
            stderr: String,
        }

        fn run_test(path: &str, test: Test) {
            let Test{src, exp} = test;

            fs::write(path, src)
                .expect(&format!(\"couldn't create test file '{}'\", path));

            let mut cmd = Command::cargo_bin(env!(\"CARGO_PKG_NAME\")).unwrap();
            let assert = cmd
                .arg(path)
                .assert();

            assert
                .code(exp.code)
                .stdout(exp.stdout)
                .stderr(exp.stderr);
        }
    "};
    write!(test_file, "{}", header)
        .expect("couldn't write test file header");
}

fn write_test(test_dir: &Path, test_file: &mut File, test: &Test) {
    let test_file_path = test_dir.join(test.name.clone() + ".lr");

    // TODO Indent rendered code.
    write!(
        test_file,
        indoc!{"

            #[test]
            fn {name}() {{
                run_test(
                    \"{path}\",
                    Test{{
                        src: String::from(\"{src}\"),
                        exp: TestExpectation{{
                            code: 0,
                            stdout: String::from(\"{tgt}\"),
                            stderr: String::from(\"\"),
                        }},
                    }}
                );
            }}
        "},
        name = test.name,
        path = test_file_path.display(),
        src = test.src,
        tgt = test.tgt,
    )
        .expect(&format!("couldn't write test to test file '{:?}'", test_file_path));
}
