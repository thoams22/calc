use std::{
    fmt::{Display, Formatter},
    ptr::null,
};

use anyhow::anyhow;

use crate::ast::Errors;

use super::{number::Number, rationnal::Rationnal};

#[derive(PartialEq, Eq, Debug, Clone, Hash, Copy)]
pub enum Numeral {
    Number(Number),
    Rationnal(Rationnal),
}

impl Numeral {
    pub fn number(num: i64) -> Numeral {
        Numeral::Number(Number { num })
    }
    pub fn rationnal(num: i64, den: i64) -> Result<Numeral, anyhow::Error> {
        if den != 0 {
            Ok(Numeral::Rationnal(Rationnal {
                num: Number { num },
                den: Number { num: den },
            }))
        } else {
            Err(anyhow!(Errors::ZeroDiv))
        }
    }
}

impl Numeral {
    pub fn is_negative(&self) -> bool {
        match self {
            Numeral::Number(num) => num.num.is_negative(),
            Numeral::Rationnal(rat) => rat.num.num.is_negative() || rat.den.num.is_negative(),
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Numeral::Number(num), Numeral::Number(num2)) => num.equal(num2),
            (Numeral::Rationnal(rat), Numeral::Rationnal(rat2)) => rat.equal(rat2),
            _ => false,
        }
    }

    pub fn simplify(self) -> Result<Numeral, anyhow::Error> {
        match self {
            Numeral::Number(_) => Ok(self),
            Numeral::Rationnal(rat) => rat.simplify(),
        }
    }
}

impl Display for Numeral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeral::Number(num) => write!(f, "{}", num.num),
            Numeral::Rationnal(rat) => write!(f, "({}/{})", rat.num.num, rat.den.num),
        }
    }
}
