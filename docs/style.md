Code Style
==========

About
-----

This document outlines code style conventions used throughout this codebase.

`rustfmt`
---------

`rustfmt` is not being used by this project because it doesn't honour the rules
defined in this document.

Rule of Thumb
-------------

Rust code tends to be relatively terse due to its explicit nature which means
that there can often be a lot going on on a single line. This project takes the
approach of being liberal with line breaks in an effort to try and increase the
"scan-ability" and readability of individual lines.

Rules
-----

### General

#### Line length

This project uses a maximum line length of 79 for Rust files.

##### Rationale

Rust code tends to be relatively terse due to its explicit nature which means
that there can often be a lot going on on a single line. A small maximum line
length is used to help work around this tendency. See
<https://github.com/rust-lang/rust-guidelines/pull/12> for the reason why 79 is
used instead of 80.

#### `.context()`, `.with_context()` and `.unwrap_or_else()`

`.context()`, `.with_context()` and `.unwrap_or_else()` should always be on
their own line. For example, instead of the following:

    fs::create_dir(&path).context(CreateMainDirFailed{path: path});

Do the following:

    fs::create_dir(&path)
        .context(CreateMainDirFailed{path: path});

#### `.expect()`

`.expect()` should follow the same rules as `.context()`, and additionally must
only be used in tests.

#### `match` branches

Branches of a `match` expression should all be in blocks, or else all outside
blocks; blocks should not be interspersed with non-blocks. For example, instead
of the following:

    let tool = match tool_factories.get(&tool_name) {
        Some(tool_factory) => tool_factory.create(),
        None => {
            let err = ParseDepsError::UnknownTool(ln_num, local_name, tool_name)
            return Err(err);
        },
    };

Do the following:

    let tool = match tool_factories.get(&tool_name) {
        Some(tool_factory) => {
            tool_factory.create()
        },
        None => {
            let err = ParseDepsError::UnknownTool(ln_num, local_name, tool_name)
            return Err(err);
        },
    };

Furthermore, `match` expressions that don't use blocks should be consistent in
terms of newlines. For example, instead of the following:

    match err {
        InstallError::GetCurrentDirFailed(io_err) =>
            eprintln!("Couldn't get the current directory: {}", io_err),
        InstallError::InstallDepsError(err) => print_install_deps_error(err),
        // ...
    }

Do the following:

    match err {
        InstallError::GetCurrentDirFailed(io_err) =>
            eprintln!("Couldn't get the current directory: {}", io_err),
        InstallError::InstallDepsError(err) =>
            print_install_deps_error(err),
        // ...
    }

### Functions

#### Return type line

If a return type is specified on its own line then it should be indented, and
the function's opening brace should be on the following line. For example:

    fn read_deps_file(start: PathBuf, deps_file_name: &str)
        -> Option<(PathBuf, Vec<u8>)>
    {

Another example:

    fn parse_deps_conf<'a>(
        conts: &str,
        tool_factories: &HashMap<String, &'a (dyn DepToolFactory<String> + 'a)>,
    )
        -> Result<DepsConf<'a, String>, ParseDepsConfError>
    {

#### End-of-function returns

Functions that end by returning a value shouldn't use the `return` keyword for
its final expression. The final expression should instead have a blank above it,
unless it is the only expression in the function.

#### Chain head

The "head" of a chain of function calls, when there is more than one chained
call, must be on its own line. For example, instead of the following:

    let maybe_output = Command::new("git")
        .args(git_args)
        .current_dir(out_dir)
        .output();

Do the following:

    let maybe_output =
        Command::new("git")
            .args(git_args)
            .current_dir(out_dir)
            .output();

One chained function call is allowable on a single line. For example:

    let words: Vec<&str> = ln.split_ascii_whitespace().collect();
