// Copyright 2021 Sean Kelleher. All rights reserved.
// Use of this source code is governed by an MIT
// licence that can be found in the LICENCE file.

pub use super::value::Object;
pub use super::value::Value;

pub struct Builtins {
    pub std: Object,
    pub prototypes: Prototypes,
}

pub struct Prototypes {
    pub bools: Object,
    pub ints: Object,
    pub strs: Object,
    pub lists: Object,
    pub objects: Object,
    pub funcs: Object,
    pub commands: Object,
}

impl Prototypes {
    pub fn prototype_for<'a>(&'a self, v: &Value) -> Result<&'a Object, String> {
        let proto =
            match v {
                Value::Bool{..} => &self.bools,
                Value::Int{..} => &self.ints,
                Value::Str{..} => &self.strs,
                Value::List{..} => &self.lists,
                Value::Object{..} => &self.objects,

                Value::BuiltInFunc{..} => &self.funcs,
                Value::Func{..} => &self.funcs,

                Value::Command{..} => &self.commands,

                Value::Null => {
                    return Err(format!("`null` doesn't have an associated prototype"));
                },
            };

        Ok(proto)
    }
}
