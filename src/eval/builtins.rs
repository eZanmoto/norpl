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
    pub fn prototype_for(&self, v: &Value) -> Result<Object, String> {
        let proto =
            match v {
                // TODO Use references instead of `clone()`s.

                Value::Bool{..} => self.bools.clone(),
                Value::Int{..} => self.ints.clone(),
                Value::Str{..} => self.strs.clone(),
                Value::List{..} => self.lists.clone(),
                Value::Object{..} => self.objects.clone(),

                Value::BuiltInFunc{..} => self.funcs.clone(),
                Value::Func{..} => self.funcs.clone(),

                Value::Command{..} => self.commands.clone(),

                Value::Null => {
                    return Err(format!("`null` doesn't have an associated prototype"));
                },
            };

        Ok(proto)
    }
}
