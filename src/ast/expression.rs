use std::fmt::{Display, Formatter};

use super::{
    addition::Addition, complex::Complex, constant::Constant, equality::Equality,
    exponentiation::Exponentiation, fraction::Fraction, function::Function, imaginary::Imaginary,
    monome::Monome, multiplication::Multiplication, negation::Negation, number::Number,
    numeral::Numeral, variable::Variable,
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Expression {
    // Basic
    Numeral(Numeral),
    Variable(Variable),
    Constant(Constant),
    Imaginary(Box<Imaginary>),

    // Operation
    Addition(Addition),
    Multiplication(Box<Multiplication>),
    Fraction(Box<Fraction>),
    Exponentiation(Box<Exponentiation>),
    Negation(Box<Negation>),
    Function(Box<Function>),
    Equality(Box<Equality>),

    // Other
    Complex(Box<Complex>),
    // Derivation
    // Intergration
}

// Constructors
impl Expression {
    pub fn number(num: i64) -> Expression {
        Expression::Numeral(Numeral::number(num))
    }

    pub fn imaginary(expr: Expression) -> Expression {
        Expression::Imaginary(Box::new(Imaginary { expr }))
    }

    pub fn rationnal(num: i64, den: i64) -> Result<Expression, anyhow::Error> {
        Ok(Expression::Numeral(Numeral::rationnal(num, den)?))
    }

    pub fn variable(var: String) -> Expression {
        Expression::Variable(Variable::new(var))
    }

    pub fn addition(terms: Vec<Expression>) -> Expression {
        Expression::Addition(Addition::new(terms))
    }

    pub fn substraction(left: Expression, right: Expression) -> Expression {
        Expression::addition(vec![left, Expression::negation(right)])
    }

    pub fn e() -> Expression {
        Expression::Constant(Constant::e())
    }
    pub fn pi() -> Expression {
        Expression::Constant(Constant::pi())
    }
    pub fn tau() -> Expression {
        Expression::Constant(Constant::tau())
    }

    pub fn constant(name: &str) -> Result<Expression, anyhow::Error> {
        Ok(Expression::Constant(Constant::get_constant(name)?))
    }

    pub fn negation(neg: Expression) -> Expression {
        Expression::Negation(Box::new(Negation::new(neg)))
    }

    pub fn complex(real: Expression, imag: Expression) -> Expression {
        Expression::Complex(Box::new(Complex::new(real, imag)))
    }

    pub fn multiplication(coef: Numeral, monomes: Vec<Monome>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new(coef, monomes)))
    }

    pub fn multiplication_from_expr(coef: Numeral, monomes: Vec<Expression>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new_from_expr(coef, monomes)))
    }

    pub fn exponentiation(base: Expression, exponent: Expression) -> Expression {
        Expression::Exponentiation(Box::new(Exponentiation::new(base, exponent)))
    }

    pub fn fraction(num: Expression, den: Expression) -> Expression {
        Expression::Fraction(Box::new(Fraction::new(num, den)))
    }

    pub fn abs(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::abs(expr)))
    }

    pub fn sqrt(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::sqrt(expr)))
    }
    pub fn ln(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::ln(expr)))
    }
    pub fn sin(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::sin(expr)))
    }
    pub fn cos(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::cos(expr)))
    }
    pub fn tan(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::tan(expr)))
    }
    pub fn asin(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::asin(expr)))
    }
    pub fn acos(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::acos(expr)))
    }
    pub fn atan(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::atan(expr)))
    }
    pub fn sinh(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::sinh(expr)))
    }
    pub fn cosh(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::cosh(expr)))
    }
    pub fn tanh(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::tanh(expr)))
    }
    pub fn log(base: Expression, expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::log(base, expr)))
    }

    pub fn function(name: &str, args: Vec<Expression>) -> Result<Expression, anyhow::Error> {
        Ok(Expression::Function(Box::new(Function::get_function(
            name, args,
        )?)))
    }

    pub fn equality(left: Expression, right: Expression) -> Expression {
        Expression::Equality(Box::new(Equality::new(left, right)))
    }
}

impl Expression {
    pub fn to_monome(&self) -> Option<Monome> {
        match self {
            Expression::Variable(_) | Expression::Constant(_) | Expression::Function(_) => {
                Some(Monome::new(self.clone(), Expression::number(1)))
            }
            Expression::Exponentiation(exp) => {
                if let Expression::Variable(_) | Expression::Function(_) | Expression::Constant(_) =
                    exp.base
                {
                    Some(Monome::new(exp.base.clone(), exp.exponent.clone()))
                } else {
                    None
                }
            }
            Expression::Multiplication(mul) => {
                if mul.monomes.len() == 1 {
                    if let (
                        Numeral::Number(Number { num: 1 }),
                        Expression::Numeral(Numeral::Number(Number { num: 1 })),
                    ) = (&mul.coef, &mul.monomes[0].power)
                    {
                        return Some(mul.monomes[0].clone());
                    }
                }
                None
            }
            _ => None,
        }
    }

    // pub fn to_monomes(&self) -> Option<Vec<Monome>> {
    //     match self {
    //         _ => None,
    //     }
    // }
}

impl Expression {
    pub fn equal(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Numeral(num), Expression::Numeral(num2)) => num.equal(num2),
            (Expression::Imaginary(im), Expression::Imaginary(im2)) => im.equal(im2),
            (Expression::Variable(var), Expression::Variable(var2)) => var.equal(var2),
            (Expression::Addition(add), Expression::Addition(add2)) => add.equal(add2),
            (Expression::Multiplication(mul), Expression::Multiplication(mul2)) => mul.equal(mul2),
            (Expression::Exponentiation(exp), Expression::Exponentiation(exp2)) => exp.equal(exp2),
            (Expression::Equality(eq), Expression::Equality(eq2)) => eq.equal(eq2),
            (Expression::Negation(neg), Expression::Negation(neg2)) => neg.equal(neg2),
            (Expression::Fraction(frac), Expression::Fraction(frac2)) => frac.equal(frac2),
            (Expression::Constant(cons), Expression::Constant(cons2)) => cons.equal(cons2),
            (Expression::Complex(comp), Expression::Complex(comp2)) => comp.equal(comp2),
            (Expression::Function(fun), Expression::Function(fun2)) => fun.equal(fun2),
            _ => false,
        }
    }

    pub fn simplify(self) -> Result<Expression, anyhow::Error> {
        match self {
            Expression::Numeral(num) => Ok(Expression::Numeral(num.simplify()?)),
            Expression::Variable(_) => Ok(self),
            Expression::Constant(_) => Ok(self),
            Expression::Addition(add) => add.simplify(),
            Expression::Multiplication(mul) => mul.simplify(),
            Expression::Exponentiation(exp) => exp.simplify(),
            Expression::Fraction(frac) => frac.simplify(),
            Expression::Negation(neg) => neg.simplify(),
            Expression::Function(_) => todo!(),
            Expression::Complex(_) => todo!(),
            Expression::Equality(eq) => eq.simplify(),
            Expression::Imaginary(im) => im.simplify(),
            _ => todo!("Simplify not yet implemented for this Type"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Numeral(num) => write!(f, "{}", num),
            Expression::Variable(var) => write!(f, "{}", var),
            Expression::Constant(cons) => write!(f, "{}", cons),
            Expression::Addition(add) => write!(f, "{}", add),
            Expression::Multiplication(mul) => write!(f, "{}", mul),
            Expression::Exponentiation(exp) => write!(f, "{}", exp),
            Expression::Fraction(frac) => write!(f, "{}", frac),
            Expression::Negation(neg) => write!(f, "{}", neg),
            Expression::Function(fun) => write!(f, "{}", fun),
            Expression::Complex(comp) => write!(f, "{}", comp),
            Expression::Equality(eq) => write!(f, "{}", eq),
            Expression::Imaginary(im) => write!(f, "{}", im),
        }
    }
}
