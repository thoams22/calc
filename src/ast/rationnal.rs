use crate::utils::gcd;

use super::{fraction::Fraction, number::{self, Number}, numeral::Numeral};

#[derive(PartialEq, Eq, Debug, Clone, Hash, Copy)]
pub struct Rationnal {
    pub(crate) num: Number,
    pub(crate) den: Number,
}

impl Rationnal {
    pub fn equal(&self, other: &Self) -> bool {
        self.num == other.num && self.den == other.den
    }

    pub fn simplify(self) -> Result<Numeral, anyhow::Error> {
        Fraction::div_num(self.num, self.den)
    }
}
