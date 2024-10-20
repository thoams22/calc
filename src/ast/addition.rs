use std::fmt::{Display, Formatter};

use super::{numeral::Numeral, Expression};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Addition {
    pub(crate) terms: Vec<Expression>,
}

impl Addition {
    pub fn new(terms: Vec<Expression>) -> Addition {
        Addition { terms }
    }

    pub fn add_num(num: Numeral, num2: Numeral) -> Result<Numeral, anyhow::Error> {
        match (num, num2) {
            // a + b = c
            (Numeral::Number(num_1), Numeral::Number(num_2)) => {
                Ok(Numeral::number(num_2.num + num_1.num))
            }
            // a/b + c/d = (a*d + c*b) / (bd)
            (Numeral::Rationnal(rat), Numeral::Rationnal(rat2)) => Numeral::rationnal(
                rat.num.num * rat2.den.num + rat2.num.num * rat.den.num,
                rat.den.num * rat2.den.num,
            ),
            // a + c/d = (a*d + c) / d
            (Numeral::Rationnal(rat), Numeral::Number(num))
            | (Numeral::Number(num), Numeral::Rationnal(rat)) => {
                Numeral::rationnal(rat.den.num * num.num + rat.num.num, rat.den.num)
            }
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        if self.terms.len() != other.terms.len() {
            return false;
        }

        let len = self.terms.len();
        // Verify that all terms are equal to another one time.
        // Because terms may not be in the same order in two Addition
        let mut index: Vec<bool> = vec![false; len * 2];
        'i: for i in 0..len {
            for j in 0..len {
                if !(index[i] || index[j + len]) && self.terms[i].equal(other.terms.get(j).unwrap())
                {
                    index[i] = true;
                    index[j + len] = true;
                    continue 'i;
                }
            }
            return false;
        }
        return index.iter().all(|&x| x);
    }

    // IMG + EXPR => COMP
    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        // Simplify the terms
        self.terms = self
            .terms
            .iter_mut()
            .map(|expr| expr.clone().simplify())
            .collect::<Result<Vec<Expression>, anyhow::Error>>()?;

        // Reduce the nested addition
        let mut i = 0;
        while i < self.terms.len() {
            if let Expression::Addition(add) = &self.terms[i] {
                self.terms.append(&mut add.terms.clone());
                self.terms.swap_remove(i);
            } else {
                i += 1
            }
        }

        let mut i = 0;
        while i < self.terms.len() {
            let mut j = i + 1;
            while j < self.terms.len() {
                match (self.terms.get(j).unwrap(), self.terms.get(i).unwrap()) {
                    // Num + Num = Num
                    (Expression::Numeral(num), Expression::Numeral(num2)) => {
                        self.terms[i] = Expression::Numeral(Addition::add_num(*num, *num2)?);
                        self.terms.swap_remove(j);
                    }
                    // mul_1 + mul_2 = mul
                    (Expression::Multiplication(mul_1), Expression::Multiplication(mul_2)) => {
                        if mul_1.equal_monomes(mul_2) {
                            self.terms[i] = Expression::multiplication(
                                Addition::add_num(mul_2.coef, mul_1.coef)?,
                                mul_1.monomes.clone(),
                            );
                            self.terms.swap_remove(j);
                        } else {
                            j += 1
                        }
                    }
                    // mul(x, var) + var = mul((x + 1), var)
                    (expr, Expression::Multiplication(mul))
                    | (Expression::Multiplication(mul), expr) => {
                        if mul.monomes.len() == 1 {
                            match expr.to_monome() {
                                Some(monome) => {
                                    if monome == mul.monomes[0] {
                                        self.terms[i] = Expression::multiplication(
                                            Addition::add_num(Numeral::number(1), mul.coef)?,
                                            vec![monome],
                                        );
                                        self.terms.swap_remove(j);
                                    } else {
                                        j += 1
                                    }
                                }
                                None => j += 1,
                            }
                        } else {
                            j += 1
                        }
                    }
                    // expr + neg(expr)
                    (x, Expression::Negation(y)) | (Expression::Negation(y), x) => {
                        if x.equal(&y.expr) {
                            self.terms[i] = Expression::number(0);
                            self.terms.swap_remove(j);
                        } else {
                            j += 1
                        }
                    }
                    // comp + comp
                    // comp + expr
                    // // Imaginary + Imaginary
                    // (Expression::Imaginary(img), Expression::Imaginary(img2)) => {
                    //     self.terms[i] =
                    //         Expression::imaginary(Expression::addition(
                    //             vec![img.expr.clone(), img2.expr.clone()],
                    //         ))
                    //         .simplify()?;
                    //     self.terms.swap_remove(j);
                    // }
                    // frac + frac
                    // frac + expr
                    // rat + rat
                    // rat + num

                    // expr + expr = 2 * expr
                    (x, y) if x.equal(y) => match x.to_monome() {
                        Some(monome) => {
                            self.terms[i] =
                                Expression::multiplication(Numeral::number(2), vec![monome]);
                            self.terms.swap_remove(j);
                        }
                        None => j += 1,
                    },

                    _ => j += 1,
                }
            }
            i += 1
        }

        if self.terms.len() == 1 {
            Ok(self.terms[0].clone())
        } else {
            Ok(Expression::Addition(self))
        }
    }
}

impl Display for Addition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;

        write!(f, "(",)?;
        for expr in self.terms.iter() {
            if first {
                first = false;
            } else {
                write!(f, " + ")?;
            }
            write!(f, "{}", expr)?;
        }
        write!(f, ")",)?;
        Ok(())
    }
}
