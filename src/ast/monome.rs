use std::fmt::{Display, Formatter};

use anyhow::Ok;

use crate::ast::{number::Number, numeral::Numeral};

use super::Expression;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub(crate) struct Monome {
    pub(super) term: Expression,
    // Power should be different from [Number(0)]
    // if number < 0 => Should be Division bcs Negative power are handle by Division.
    pub(super) power: Expression,
}

impl Monome {
    pub fn new(term: Expression, power: Expression) -> Monome {
        Monome { term, power }
    }

    pub fn equal(&self, other: &Monome) -> bool {
        self.term.equal(&other.term) && self.power.equal(&other.power)
    }

    pub fn get_term(&self) -> &Expression {
        &self.term
    }

    pub fn get_power(&self) -> &Expression {
        &self.power
    }

    pub fn is_reduced(&self) -> bool {
        matches!(
            self.term,
            Expression::Variable(_) | Expression::Function(_) | Expression::Constant(_)
        )
    }

    pub fn to_exponentation(self) -> Expression {
        if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = self.power {
            self.term
        } else {
            Expression::exponentiation(self.term, self.power)
        }
    }

    pub fn simplify(mut self) -> Result<Monome, anyhow::Error> {
        self.term = self.term.simplify()?;
        self.power = self.power.simplify()?;

        let expr = self.to_exponentation().simplify()?;

        if let Some(mon) = expr.to_monome() {
            Ok(mon)
        } else {
            Ok(Monome {
                term: expr,
                power: Expression::number(1),
            })
        }
    }
}

impl Display for Monome {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.term,
            if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = self.power {
                "".to_owned()
            } else {
                format!("^{}", self.power)
            }
        )
    }
}
