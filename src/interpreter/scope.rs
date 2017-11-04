use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt;

use super::object::Object;

pub struct Scope {
    name: String,
    variables: HashMap<String, Object>,
    enclosing_scope: Option<Box<Scope>>
}

impl Scope {

    pub fn new(name: String) -> Scope {
        return Scope { name, variables: HashMap::new(), enclosing_scope: None }
    }

    pub fn with_enclosing_scope(name: String, enclosing_scope: Scope) -> Scope {
        return Scope { name, variables: HashMap::new(), enclosing_scope: Some(Box::new(enclosing_scope))}
    }

    pub fn name(&self) -> String {
        return self.name.clone();
    }

    pub fn enclosing_scope(self) -> Option<Scope> {
        return match self.enclosing_scope {
            Some(scope) => Some(*scope),
            None        => None
        };
    }

    pub fn get(&mut self, name: &String) -> Option<&Object> {
        if let Some(object) = self.variables.get(name) {
            return Some(object);
        } else if let Some(ref mut scope) = self.enclosing_scope {
            return scope.get(name);
        } else {
            return None;
        }
    }

    pub fn set(&mut self, name: String, object: Object) {
        self.variables.insert(name, object);
    }
}

impl Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let enclosing_scope_name = match self.enclosing_scope {
            Some(ref scope) => Some(scope.name()),
            None            => None
        };

        write!(f, "\n\tName: {:?}\n\tVariables: {:?}\n\tEnclosing Scope: {:?}", self.name, self.variables, enclosing_scope_name)
    }
}