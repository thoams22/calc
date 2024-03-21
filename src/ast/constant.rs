use anyhow::anyhow;
use std::fmt::{Display, Formatter};

use crate::ast::Errors;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Constant {
    E,
    Pi,
    Tau,
}

impl Constant {
    pub fn e() -> Constant {
        Constant::E
    }
    pub fn pi() -> Constant {
        Constant::Pi
    }
    pub fn tau() -> Constant {
        Constant::Tau
    }

    pub fn get_constant(name: &str) -> Result<Constant, anyhow::Error> {
        match name {
            "e" => Ok(Constant::E),
            "pi" => Ok(Constant::Pi),
            "tau" => Ok(Constant::Tau),
            _ => Err(anyhow!(Errors::UnknownConstant(name.into()))),
        }
    }

    pub fn equal(&self, other: &Constant) -> bool {
        matches!(
            (self, other),
            (Constant::E, Constant::E)
                | (Constant::Pi, Constant::Pi)
                | (Constant::Tau, Constant::Tau)
        )
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::E => write!(f, "e"),
            Constant::Pi => write!(f, "pi"),
            Constant::Tau => write!(f, "tau"),
        }
    }
}
