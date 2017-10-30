#[derive(Debug)]
pub enum Symbol {
    INTEGER(String),
    REAL(String)
}

impl Symbol {

    pub fn name(&self) -> String {
        return match self {
            &Symbol::INTEGER(ref name) |
            &Symbol::REAL(ref name)      => name.clone()
        };
    }
}