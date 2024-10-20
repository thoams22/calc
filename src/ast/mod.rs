use self::expression::Expression;

mod addition;
mod complex;
mod constant;
mod equality;
mod exponentiation;
pub mod expression;
mod fraction;
pub mod function;
pub mod monome;
mod multiplication;
pub mod negation;
mod number;
pub mod numeral;
pub mod imaginary;
mod rationnal;
mod test;
mod variable;
pub mod statement;

// TODO add state on simplify for angle/expanded/steps

#[derive(Debug, thiserror::Error, Clone)]
pub enum Errors {
    #[error("Trying to construct a <Rationnal> with a denominator equal to 0")]
    ZeroDiv,
    #[error("Trying to construct a <Constant> with {0}")]
    UnknownConstant(String),
    #[error("Trying to construct a <Monome> with {0}")]
    UnknownTerm(Expression),
    #[error("Trying to construct a <Function> with the name: {0} but this nam is unknown")]
    UnknownFunction(String),
    #[error("Number from exponentiation too Big for <i64>")]
    ExponentiationOverflow,
    #[error("The expression '{0}' is Undefined")]
    Undefined(String),
}