use std::fmt::{Display, Formatter};

use super::Expression;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Equality {
    pub(super) left: Expression,
    pub(super) right: Expression,
}

impl Equality {
    pub fn new(left: Expression, right: Expression) -> Equality {
        Equality { left, right }
    }

    pub fn equal(&self, other: &Equality) -> bool {
        (self.left == other.left && self.right == other.right)
            || (self.right == other.left && self.left == other.right)
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        self.left = self.left.simplify()?;
        self.right = self.right.simplify()?;
        Ok(Expression::Equality(Box::new(self)))
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}
