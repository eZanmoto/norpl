// Copyright 2022 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

use std::fs::File;
use std::io::Read;
use std::io::Result as IoResult;
use std::path::Path;
use std::path::PathBuf;

pub type ModuleId = PathBuf;

pub fn parse_import_path_type(import_path: &str)
    -> Result<(ImportType, String),String>
{
    if import_path.starts_with('.') {
        let mut parent_depth = 0;
        for c in import_path.chars() {
            if c == '.' {
                parent_depth += 1;
            } else if c == '/' {
                break;
            } else {
                return Err(format!("invalid character in import path prefix: {}", c))
            }
        }
        let (_, sub_import_path) = import_path.split_at(parent_depth + 1);
        parent_depth -= 1;

        return Ok((ImportType::Relative{parent_depth}, sub_import_path.to_string()));
    }

    Ok((ImportType::StandardLibrary, import_path.to_string()))
}

pub enum ImportType {
    Relative{parent_depth: usize},
    StandardLibrary,
}

// `parse_import_path_as_path_buf` returns a `PathBuf` representing
// `import_path`.
//
// NOTE `import_path` cannot start with `/`. TODO Document the reason for this
// in more detail.
pub fn parse_import_path_as_path_buf(import_path: &str)
    -> Result<PathBuf,String>
{
    let path_components = split_import_path(import_path);

    let mut path_buf = PathBuf::new();
    for c in path_components {
        if c == "" {
            return Err(format!("path component can't be empty"));
        } else if c == "." || c == ".." {
            // TODO Document why path components can't be path traversal
            // identifiers.
            return Err(format!("path component can't be '{}'", c));
        }
        path_buf.push(c);
    }

    Ok(path_buf)
}

pub fn split_import_path(import_path: &str) -> Vec<&str> {
    // NOTE This approach to splitting a path into its components prevents path
    // components from containing `/`. This limitation can be lifted by
    // allowing `/`s to be escaped.
    import_path.split('/').collect()
}

pub fn read_file(path: &Path) -> IoResult<String> {
    // TODO Wrap errors to capture which functions errors were generated from.

    let mut file = File::open(&path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}
