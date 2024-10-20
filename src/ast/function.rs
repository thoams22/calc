use crate::ast::Errors;
use anyhow::anyhow;
use std::fmt::{Display, Formatter};

use super::Expression;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Function {
    // 1 arg
    Abs(Expression),
    Sqrt(Expression),
    Ln(Expression),

    Sin(Expression),
    Cos(Expression),
    Tan(Expression),
    Asin(Expression),
    Acos(Expression),
    Atan(Expression),
    Sinh(Expression),
    Cosh(Expression),
    Tanh(Expression),

    // 2 args
    Log(Expression, Expression),
    // Unknown number of args
    // UserDefined()
}

impl Function {
    pub fn abs(expr: Expression) -> Function {
        Function::Abs(expr)
    }

    pub fn sqrt(expr: Expression) -> Function {
        Function::Sqrt(expr)
    }
    pub fn ln(expr: Expression) -> Function {
        Function::Ln(expr)
    }
    pub fn sin(expr: Expression) -> Function {
        Function::Sin(expr)
    }
    pub fn cos(expr: Expression) -> Function {
        Function::Cos(expr)
    }
    pub fn tan(expr: Expression) -> Function {
        Function::Tan(expr)
    }
    pub fn asin(expr: Expression) -> Function {
        Function::Asin(expr)
    }
    pub fn acos(expr: Expression) -> Function {
        Function::Acos(expr)
    }
    pub fn atan(expr: Expression) -> Function {
        Function::Atan(expr)
    }
    pub fn sinh(expr: Expression) -> Function {
        Function::Sinh(expr)
    }
    pub fn cosh(expr: Expression) -> Function {
        Function::Cosh(expr)
    }
    pub fn tanh(expr: Expression) -> Function {
        Function::Tanh(expr)
    }
    pub fn log(base: Expression, expr: Expression) -> Function {
        Function::Log(base, expr)
    }
}

impl Function {
    pub fn get_args(&self) -> Vec<&Expression> {
        match self {
            Function::Abs(expr)
            | Function::Sqrt(expr)
            | Function::Ln(expr)
            | Function::Sin(expr)
            | Function::Cos(expr)
            | Function::Tan(expr)
            | Function::Asin(expr)
            | Function::Acos(expr)
            | Function::Atan(expr)
            | Function::Sinh(expr)
            | Function::Cosh(expr)
            | Function::Tanh(expr) => vec![expr],
            Function::Log(base, expr) => vec![base, expr],
        }
    }

    /// The number of arguments must be the right one.
    ///
    /// Or it will panic
    pub fn get_function(name: &str, args: Vec<Expression>) -> Result<Function, anyhow::Error> {
        match name {
            "abs" => Ok(Function::Abs(args[0].clone())),
            "sqrt" => Ok(Function::Sqrt(args[0].clone())),
            "ln" => Ok(Function::Ln(args[0].clone())),
            "sin" => Ok(Function::Sin(args[0].clone())),
            "cos" => Ok(Function::Cos(args[0].clone())),
            "tan" => Ok(Function::Tan(args[0].clone())),
            "asin" => Ok(Function::Asin(args[0].clone())),
            "acos" => Ok(Function::Acos(args[0].clone())),
            "atan" => Ok(Function::Atan(args[0].clone())),
            "sinh" => Ok(Function::Sinh(args[0].clone())),
            "cosh" => Ok(Function::Cosh(args[0].clone())),
            "tanh" => Ok(Function::Tanh(args[0].clone())),
            "log" => Ok(Function::Log(args[0].clone(), args[1].clone())),
            _ => Err(anyhow!(Errors::UnknownFunction(name.to_string()))),
        }
    }

    /// return 0 if the funciton doesn't exist
    pub fn get_args_number(name: &str) -> Option<usize> {
        match name {
            "abs" | "sqrt" | "ln" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sinh"
            | "cosh" | "tanh" => Some(1),
            "log" => Some(2),
            _ => None,
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Function::Abs(fun), Function::Abs(fun2)) => fun.equal(fun2),
            (Function::Sqrt(fun), Function::Sqrt(fun2)) => fun.equal(fun2),
            (Function::Ln(fun), Function::Ln(fun2)) => fun.equal(fun2),
            (Function::Sin(fun), Function::Sin(fun2)) => fun.equal(fun2),
            (Function::Cos(fun), Function::Cos(fun2)) => fun.equal(fun2),
            (Function::Tan(fun), Function::Tan(fun2)) => fun.equal(fun2),
            (Function::Asin(fun), Function::Asin(fun2)) => fun.equal(fun2),
            (Function::Acos(fun), Function::Acos(fun2)) => fun.equal(fun2),
            (Function::Atan(fun), Function::Atan(fun2)) => fun.equal(fun2),
            (Function::Sinh(fun), Function::Sinh(fun2)) => fun.equal(fun2),
            (Function::Cosh(fun), Function::Cosh(fun2)) => fun.equal(fun2),
            (Function::Tanh(fun), Function::Tanh(fun2)) => fun.equal(fun2),
            (Function::Log(base, expr), Function::Log(base2, expr2)) => {
                base.equal(base2) && expr.equal(expr2)
            }
            _ => false,
        }
    }

    pub fn simplify(self) -> Result<Expression, anyhow::Error> {
        match self {
            // Log(base, Exp(Base, expr)) => Expr
            // Log(a, Exp(b, c)) => c*Log(a, b)
            // Sqrt(Exp(a, 2)) => Abs(a)
            // Abs(Neg(a)) => a
            // Sin(-x) => -Sin(x)
            // Cos(-x) => Cos(x)
            // ...
            _ => todo!("Simplify {self} is not yet implemented"),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Abs(expr) => write!(f, "abs({})", expr),
            Function::Sqrt(expr) => write!(f, "sqrt({})", expr),
            Function::Ln(expr) => write!(f, "ln({})", expr),
            Function::Sin(expr) => write!(f, "sin({})", expr),
            Function::Cos(expr) => write!(f, "cos({})", expr),
            Function::Tan(expr) => write!(f, "tan({})", expr),
            Function::Asin(expr) => write!(f, "asin({})", expr),
            Function::Acos(expr) => write!(f, "acos({})", expr),
            Function::Atan(expr) => write!(f, "atan({})", expr),
            Function::Sinh(expr) => write!(f, "sinh({})", expr),
            Function::Cosh(expr) => write!(f, "cosh({})", expr),
            Function::Tanh(expr) => write!(f, "tanh({})", expr),
            Function::Log(base, expr) => write!(f, "log({}, {})", base, expr),
        }
    }
}
