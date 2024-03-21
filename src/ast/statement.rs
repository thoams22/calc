use std::fmt::{Display, Formatter};

use super::{equality::Equality, expression::Expression, variable::Variable};

// TODO Make an enum for Expression + Equality, bcs Simplify/Replace/Evaluate can take both.
// And for parsing the both Equaltity/Expression can be returned.

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Statement {
    Solve(Expression),
    SolveFor(Expression, Variable),
    Simplify(Expression),
    // Factorize,
    // System
    Evaluate(Expression),
}

impl Statement {
    pub fn equal(&self, other: &Statement) -> bool {
        match (self, other) {
            (Statement::Solve(eq), Statement::Solve(eq2)) => eq.equal(eq2),
            (Statement::SolveFor(eq, var), Statement::SolveFor(eq2, var2)) => {
                eq.equal(eq2) && var.equal(var2)
            }
            (Statement::Simplify(expr), Statement::Simplify(expr2)) => expr.equal(expr2),
            (Statement::Evaluate(expr), Statement::Evaluate(expr2)) => expr.equal(expr2),
            _ => false,
        }
    }

    pub fn reduce(self) -> Result<Expression, anyhow::Error> {
        match self {
            Statement::Solve(expr) => todo!(),
            Statement::SolveFor(expr, var) => todo!(),
            Statement::Simplify(expr) => expr.simplify(),
            Statement::Evaluate(expr) => todo!(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Solve(eq) => write!(f, "Solving: {}", eq),
            Statement::SolveFor(eq, var) => {
                write!(f, "Solving this expression: {} ,for :{}", eq, var)
            }
            Statement::Simplify(exp) => write!(f, "Simplifying: {}", exp),
            Statement::Evaluate(exp) => write!(f, "Evaluating this expression {}", exp),
        }
    }
}
