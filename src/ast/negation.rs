use std::fmt::{Display, Formatter};

use super::{numeral::Numeral, rationnal, Expression};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Negation {
    pub(super) expr: Expression,
}

impl Negation {
    pub fn new(expr: Expression) -> Negation {
        Negation { expr }
    }

    pub fn equal(&self, other: &Negation) -> bool {
        self.expr.equal(&other.expr)
    }

    pub fn negate_num(numeral: Numeral) -> Result<Numeral, anyhow::Error> {
        match numeral {
            Numeral::Number(number) => Ok(Numeral::number(-number.num)),
            Numeral::Rationnal(rationnal) => {
                Numeral::rationnal(-rationnal.num.num, rationnal.den.num)
                    .unwrap()
                    .simplify()
            }
        }
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {

        self.expr = self.expr.simplify()?;

        match self.expr {
            Expression::Numeral(numeral) => Ok(Expression::Numeral(Negation::negate_num(numeral)?)),
            Expression::Multiplication(mut mul) => {
                mul.coef = Negation::negate_num(mul.coef)?;
                Ok(Expression::multiplication(mul.coef, mul.monomes))
            }
            _ => Ok(Expression::negation(self.expr)),
        }
    }
}

impl Display for Negation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "-({})", self.expr)
    }
}
