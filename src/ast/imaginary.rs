use std::fmt::Display;

use super::{expression::Expression, monome::Monome, number::Number, numeral::Numeral};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Imaginary {
    pub(crate) expr: Expression,
}

impl Imaginary {
    pub fn equal(&self, other: &Self) -> bool {
        self == other
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        match self.expr {
            // Imaginary(Imaginary(expr)) => -expr
            Expression::Imaginary(imaginary) => Expression::multiplication(
                Numeral::number(-1),
                vec![Monome {
                    term: imaginary.expr,
                    power: Expression::number(1),
                }],
            )
            .simplify(),
            Expression::Addition(mut addition) => {
                addition.terms = addition
                    .terms
                    .iter_mut()
                    .map(|expr| Expression::imaginary(expr.clone()))
                    .collect();
                addition.simplify()
            }
            Expression::Negation(negation) => Expression::multiplication(
                Numeral::number(-1),
                vec![
                    Monome {
                        term: negation.expr,
                        power: Expression::number(1),
                    },
                    Monome {
                        term: Expression::imaginary(Expression::number(1)),
                        power: Expression::number(1),
                    },
                ],
            )
            .simplify(),
            Expression::Function(function) => todo!(),
            Expression::Equality(equality) => todo!(),
            Expression::Complex(complex) => todo!(),
            _ => Ok(Expression::imaginary(self.expr.simplify()?)),
        }
    }
}

impl Display for Imaginary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let &Expression::Numeral(Numeral::Number(Number { num: 1 })) = &self.expr {
            write!(f, "i")?;
        } else {
            write!(f, "{}i", self.expr)?;
        }
        Ok(())
    }
}
