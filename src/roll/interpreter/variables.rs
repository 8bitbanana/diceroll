use std::collections::{HashMap, HashSet};

use serde::{Serialize};

use super::InterpReturn;

#[derive(Debug, Clone)]
pub struct UnresolvedVariables(HashSet<String>);

impl UnresolvedVariables {
    pub fn new() -> Self {
        Self(HashSet::<String>::new())
    }

    pub fn add(&mut self, key: String) {
        self.0.insert(key);
    }

    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.0.iter()
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0.into_iter())
    }
}

impl Default for UnresolvedVariables {
    fn default() -> Self {
        Self::new()
    }
}

// Can't deserialise cause of 'static strings, might not be needed though?
#[derive(Serialize)]
#[derive(Debug)]
pub struct ResolvedVariables(HashMap<String, InterpReturn>);

impl ResolvedVariables {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn add_resolved(&mut self, key: String, value: InterpReturn) {
        self.0.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&InterpReturn> {
        self.0.get(key)
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0.into_iter());
    }
}

impl Default for ResolvedVariables {
    fn default() -> Self {
        Self::new()
    }
}