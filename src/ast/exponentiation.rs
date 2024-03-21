use std::fmt::{Display, Formatter};

use super::{
    monome::Monome, negation::Negation, number::Number, numeral::Numeral, Errors, Expression,
};
use anyhow::{anyhow, Error};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Exponentiation {
    pub(super) base: Expression,
    pub(super) exponent: Expression,
}

impl Exponentiation {
    pub fn new(base: Expression, exponent: Expression) -> Self {
        Self { base, exponent }
    }

    pub fn get_base(&self) -> &Expression {
        &self.base
    }

    pub fn get_exponent(&self) -> &Expression {
        &self.exponent
    }

    pub fn set_base(&mut self, base: Expression) {
        self.base = base;
    }

    pub fn set_exponent(&mut self, exponent: Expression) {
        self.exponent = exponent;
    }

    pub fn exp_num(base: Numeral, exponent: Numeral) -> Result<Expression, anyhow::Error> {
        match (base, exponent) {
            (Numeral::Number(num), Numeral::Number(num2)) => {
                match num.num.checked_pow(num2.num as u32) {
                    Some(value) => Ok(Expression::number(value)),
                    None => Err(anyhow!(Errors::ExponentiationOverflow)),
                }
            }
            (Numeral::Number(num), Numeral::Rationnal(rat)) => Expression::exponentiation(
                Expression::number(num.num),
                Expression::rationnal(rat.num.num, rat.den.num)?,
            )
            .simplify(),
            (Numeral::Rationnal(rat), Numeral::Number(num)) => Expression::exponentiation(
                Expression::rationnal(rat.num.num, rat.den.num)?,
                Expression::number(num.num),
            )
            .simplify(),
            (Numeral::Rationnal(rat), Numeral::Rationnal(rat2)) => Expression::exponentiation(
                Expression::rationnal(rat.num.num, rat.den.num)?,
                Expression::rationnal(rat2.num.num, rat2.den.num)?,
            )
            .simplify(),
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.base.equal(&other.base) && self.exponent.equal(&other.exponent)
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        self.base = self.base.simplify()?;
        self.exponent = self.exponent.simplify()?;

        match (self.base, self.exponent) {
            // 1^expr => 1
            (Expression::Numeral(Numeral::Number(Number { num: 1 })), _) => {
                Ok(Expression::number(1))
            }
            // 0^0 => Undefined
            (
                Expression::Numeral(Numeral::Number(Number { num: 0 })),
                Expression::Numeral(Numeral::Number(Number { num: 0 })),
            ) => Err(anyhow!(Errors::Undefined("0^0".to_string()))),
            // 0^expr => 0
            (Expression::Numeral(Numeral::Number(Number { num: 0 })), _) => {
                Ok(Expression::number(0))
            }
            // i^Num
            (Expression::Imaginary(im), Expression::Numeral(Numeral::Number(number))) => {
                match number.num.rem_euclid(4) {
                    0 => Expression::exponentiation(
                        im.expr,
                        Expression::Numeral(Numeral::Number(number)),
                    )
                    .simplify(),
                    1 => Ok(Expression::imaginary(Expression::exponentiation(
                        im.expr,
                        Expression::Numeral(Numeral::Number(number)),
                    ))
                    .simplify()?),
                    2 => Ok(Expression::negation(Expression::exponentiation(
                        im.expr,
                        Expression::Numeral(Numeral::Number(number)),
                    ))
                    .simplify()?),
                    3 => Ok(Expression::negation(Expression::imaginary(
                        Expression::exponentiation(
                            im.expr,
                            Expression::Numeral(Numeral::Number(number)),
                        ),
                    ))
                    .simplify()?),
                    x => panic!("Imaginary Unit to the power of {x}"),
                }
            }
            // expr^-Num => 1/(expr^Num)
            (base, Expression::Numeral(x)) if x.is_negative() => Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(base, Expression::Numeral(Negation::negate_num(x)?)),
            )
            .simplify(),
            // expr^Neg(expr) => 1/(expr^expr)
            (base, Expression::Negation(x)) => Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(base, x.expr.clone()),
            )
            .simplify(),
            // Num^Num
            (Expression::Numeral(num), Expression::Numeral(num2)) => {
                Exponentiation::exp_num(num, num2)
            }
            // expr^1 => expr
            (base, Expression::Numeral(Numeral::Number(Number { num: 1 }))) => Ok(base),
            // expr^0 => 1
            (_, Expression::Numeral(Numeral::Number(Number { num: 0 }))) => {
                Ok(Expression::number(1))
            }
            // Frac^expr
            (Expression::Fraction(frac), expr) => Expression::fraction(
                Expression::exponentiation(frac.num, expr.clone()),
                Expression::exponentiation(frac.den, expr),
            )
            .simplify(),
            // Complex^Num =>
            // Add^Num => Multinomial

            // Mul^Num => Mul(coeff^Num, a^Num, b^Num, ...)
            (Expression::Multiplication(mut mul), Expression::Numeral(num)) => {
                mul.monomes.iter_mut().for_each(|mon| {
                    mon.power = Expression::multiplication(
                        Numeral::number(1),
                        vec![
                            Monome::new(mon.power.clone(), Expression::number(1)),
                            Monome::new(Expression::Numeral(num), Expression::number(1)),
                        ],
                    )
                    .simplify()
                    .expect("Expected no error from simplifying Mul^Num")
                });

                if let Expression::Numeral(numeral) = Exponentiation::exp_num(mul.coef, num)? {
                    mul.coef = numeral;
                } else {
                    mul.coef = Numeral::number(1);
                }

                Ok(Expression::Multiplication(mul).simplify()?)
            }
            // Base^Log(base, expr) => expr
            // Exp(a, b)^expr => Exp(a, b*expr)
            // (Expression::Exponentiation(exp), expr) => {
            //     Ok(Expression::exponentiation(exp.base.clone(),
            //     Expression::multiplication_from_expr(Numeral::number(1), vec![exp.exponent.to_monome().clone()])
            // ))
            // }
            // Default
            (base, exponent) => Ok(Expression::exponentiation(base, exponent)),
        }
    }
}

impl Display for Exponentiation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})^({})", self.base, self.exponent)
    }
}
