use std::fmt::{Display, Formatter};

use super::{Expression};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Complex {
    pub(super) real: Expression,
    pub(super) imag: Expression,
}

impl Complex {
    pub fn new(real: Expression, imag: Expression) -> Complex {
        Complex { real, imag }
    }

    pub fn equal(&self, other: &Complex) -> bool {
        self.real.equal(&other.real) && self.imag.equal(&other.imag)
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + i{}", self.real, self.imag)
    }
}
