use super::Expression;
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Variable {
    pub(crate) var: String,
}

impl Variable {
    pub fn new(var: String) -> Variable {
        Variable { var }
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.var == other.var
    }

    pub fn simplify(self) -> Result<Expression, anyhow::Error> {
        Ok(Expression::Variable(self))
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.var)
    }
}
