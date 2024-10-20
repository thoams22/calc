use anyhow::anyhow;
use std::fmt::{Display, Formatter};

use crate::utils::gcd;

use super::{
    exponentiation::Exponentiation, expression::Expression, multiplication::Multiplication, number::Number, numeral::Numeral, rationnal::Rationnal, Errors
};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Fraction {
    pub(super) num: Expression,
    pub(super) den: Expression,
}

impl Fraction {
    pub fn new(num: Expression, den: Expression) -> Self {
        Self { num, den }
    }

    pub fn get_num(&self) -> &Expression {
        &self.num
    }

    pub fn get_den(&self) -> &Expression {
        &self.den
    }

    pub fn set_num(&mut self, num: Expression) {
        self.num = num;
    }

    pub fn set_den(&mut self, den: Expression) {
        self.den = den;
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.num.equal(&other.num) && self.den.equal(&other.den)
    }

    pub fn div_num(numerator: Number, denominator: Number) -> Result<Numeral, anyhow::Error> {
        let gcd = gcd(numerator.num, denominator.num);
        let numerator = numerator.num / gcd;
        let denominator = denominator.num / gcd;

        match (numerator, denominator) {
            (x, y) if (x < 0 && y < 0) || y<0 => {
                if denominator == -1 {
                    Ok(Numeral::number(-numerator))
                } else {
                    Numeral::rationnal(-numerator, -denominator)
                }
            }
            _ => {
                if denominator == 1 {
                    Ok(Numeral::number(numerator))
                } else {
                    Numeral::rationnal(numerator, denominator)
                }
            }
        }
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        self.den = self.den.simplify()?;
        self.num = self.num.simplify()?;

        match (self.num, self.den) {
            // Expr/1 => Expr
            (expr, Expression::Numeral(Numeral::Number(Number { num: 1 }))) => Ok(expr),
            // Expr/0 => Undefined
            (_, Expression::Numeral(Numeral::Number(Number { num: 0 }))) => {
                Err(anyhow!(Errors::Undefined("x/0".to_string())))
            }
            // 0/expr => 0
            (Expression::Numeral(Numeral::Number(Number { num: 0 })), _) => {
                Ok(Expression::number(0))
            }
            // Number/Number
            (
                Expression::Numeral(Numeral::Number(num)),
                Expression::Numeral(Numeral::Number(num2)),
            ) => {
                Ok(Expression::Numeral(Fraction::div_num(num, num2)?))
            }
            // Rationnal/Rationnal
            (Expression::Numeral(Numeral::Rationnal(rat)), Expression::Numeral(Numeral::Rationnal(rat2))) => {
                Expression::rationnal(
                    rat.num.num * rat2.den.num, 
                    rat.den.num * rat2.num.num, 
                )?.simplify()
            }
            // Number/Rationnal
            (Expression::Numeral(Numeral::Number(num)), Expression::Numeral(Numeral::Rationnal(rat))) => {
                Expression::rationnal(
                    num.num * rat.den.num, 
                    rat.num.num, 
                )?.simplify()
            }
            // Rationnal/Number
            (Expression::Numeral(Numeral::Rationnal(rat)), Expression::Numeral(Numeral::Number(num))) => {
                Expression::rationnal(
                    rat.num.num, 
                    num.num * rat.den.num, 
                )?.simplify()
            }
            // Expr/Imaginary(Expr2) => Imaginary(-Expr)/Expr2
            (expr, Expression::Imaginary(im)) => Expression::fraction(
                Expression::negation(Expression::imaginary(expr)),
                im.expr,
            )
            .simplify(),
            // Neg(a)/Neg(b) => a/b
            (Expression::Negation(neg), Expression::Negation(neg2)) => {
                Expression::fraction(neg.expr, neg2.expr).simplify()
            }
            // Expr/Neg(a) => Neg(Expr/a)
            (expr, Expression::Negation(neg)) => {
                Expression::negation(Expression::fraction(expr, neg.expr)).simplify()
            }
            // Neg(a)/Expr => Neg(a/Expr)
            (Expression::Negation(neg), expr) => {
                Expression::negation(Expression::fraction(neg.expr, expr)).simplify()
            }
            // Frac(a, b)/Frac(c, d) => Frac(a*d, b*c)
            (Expression::Fraction(frac), Expression::Fraction(frac2)) => Expression::fraction(
                Expression::multiplication_from_expr(Numeral::number(1), vec![frac.num, frac2.den]),
                Expression::multiplication_from_expr(Numeral::number(1), vec![frac.den, frac2.num]),
            )
            .simplify(),
            // Frac(a, b)/Expr => Frac(a, b*Expr)
            (Expression::Fraction(frac), expr) => Expression::fraction(
                frac.num,
                Expression::multiplication_from_expr(Numeral::number(1), vec![expr, frac.den]),
            )
            .simplify(),
            // Expr/Frac(a, b) => Frac(b*Expr, a)
            (expr, Expression::Fraction(frac)) => Expression::fraction(
                Expression::multiplication_from_expr(Numeral::number(1), vec![expr, frac.den]),
                frac.num,
            )
            .simplify(),
            // Exp(a, b)/Exp(a, c) => Exp(a, b-c)
            (Expression::Exponentiation(exp), Expression::Exponentiation(exp2))
                if exp.base.equal(&exp2.base) =>
            {
                Expression::exponentiation(
                    exp.base,
                    Expression::substraction(exp.exponent, exp2.exponent),
                )
                .simplify()
            }
            // Default
            (expr, expr2) => Ok(Expression::fraction(expr, expr2)),
        }
        // TODO
        // Expr/Comp
        // Sin(x)/Cos(x) => tan(x)
        // Expr/Expr => Find commons => Expr/Expr
    }
}

impl Display for Fraction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})/({})", self.num, self.den)
    }
}
