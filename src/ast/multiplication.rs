use std::fmt::{Display, Formatter};

use super::{
    exponentiation::Exponentiation,
    monome::{self, Monome},
    number::Number,
    numeral::{self, Numeral},
    Expression,
};

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Multiplication {
    // Coeff must be [Number, Rationnal]
    pub(crate) coef: Numeral,
    pub(crate) monomes: Vec<Monome>,
}

impl Multiplication {
    pub fn new(coef: Numeral, monomes: Vec<Monome>) -> Multiplication {
        Multiplication { coef, monomes }
    }

    pub fn new_from_expr(coef: Numeral, monomes: Vec<Expression>) -> Multiplication {
        Multiplication {
            coef,
            monomes: monomes
                .iter()
                .map(|mon| {
                    if let Some(monome) = mon.to_monome() {
                        monome
                    } else {
                        Monome::new(mon.clone(), Expression::number(1))
                    }
                })
                .collect(),
        }
    }

    pub fn set_coef(&mut self, coef: Numeral) {
        self.coef = coef;
    }

    pub fn equal_monomes(&self, other: &Self) -> bool {
        if self.monomes.len() != other.monomes.len() {
            return false;
        }

        let len = self.monomes.len();
        // Verify that all monomes are equal to another one time.
        // Because monomes may not be in the same order in two Multiplication
        let mut index: Vec<bool> = vec![false; len * 2];
        'i: for i in 0..len {
            for j in 0..len {
                if !(index[i] || index[j + len])
                    && self.monomes[i].equal(other.monomes.get(j).unwrap())
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

    pub fn is_reduced(&self) -> bool {
        self.monomes.iter().all(|mon| mon.is_reduced())
    }

    pub fn mul_num(num: Numeral, num2: Numeral) -> Numeral {
        match (num, num2) {
            // a * b = ab
            (Numeral::Number(a), Numeral::Number(mut b)) => {
                b.num *= a.num;
                Numeral::Number(b)
            }
            // a * c/b = (ac)/b
            (Numeral::Number(a), Numeral::Rationnal(mut b))
            | (Numeral::Rationnal(mut b), Numeral::Number(a)) => {
                b.num.num *= a.num;
                Numeral::Rationnal(b)
            }
            // a/b * c/d = (ac)/(bd)
            (Numeral::Rationnal(a), Numeral::Rationnal(mut b)) => {
                b.num.num *= a.num.num;
                b.den.num *= a.den.num;
                Numeral::Rationnal(b)
            }
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.coef == other.coef && self.equal_monomes(other)
    }

    fn new_reduce(mut self) -> Result<Expression, anyhow::Error> {
        let mut i = 0;
        while i < self.monomes.len() {
            let mut j = i + 1;
            while j < self.monomes.len() {
                match (self.monomes.get(i).unwrap(), self.monomes.get(j).unwrap()) {
                    // ADD(a, b, c)*EXPR(z) => ADD(a*z, b*z, c*z)
                    (
                        Monome {
                            term: Expression::Addition(add),
                            power,
                        },
                        monome,
                    ) => {
                        if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = power {
                            let new_add: Vec<Expression> = add
                                .terms
                                .iter()
                                .map(|expr| {
                                    Expression::multiplication_from_expr(
                                        Numeral::number(1),
                                        vec![expr.clone(), monome.clone().to_exponentation()],
                                    )
                                })
                                .collect::<Vec<_>>();

                            self.monomes[i] = Monome {
                                term: Expression::addition(new_add),
                                power: Expression::number(1),
                            };

                            self.monomes.swap_remove(j);
                        } else {
                            j += 1
                        }
                    }
                    // MUL(coef, a, b) * NUM => MUL(coef*NUM, a, b)
                    // (
                    //     Monome {
                    //         term: Expression::Multiplication(mult),
                    //         power,
                    //     },
                    //     Monome {
                    //         term: Expression::Numeral(num),
                    //         power: Expression::Numeral(power2),
                    //     },
                    // ) => {
                    //     if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = power {

                    //     }
                    // }
                    // MUL(coef, a, b) * EXPR => MUL(coef*NUM, a, b, EXPR) EXPR != ADD
                    // EXPR * IMG => IMG(EXPR)
                    _ => j += 1,
                }
            }
            i += 1
        }

        Ok(self.verify())
    }

    fn reduce(mut self) -> Result<Expression, anyhow::Error> {
        if let Some(pos) = self
            .monomes
            .iter()
            .position(|mon| matches!(mon.term, Expression::Addition(_)))
        {
            // (a + b)cd = Mul(acd) + Mul(bcd)
            if let Expression::Addition(add) = self.monomes.swap_remove(pos).term {
                let extended_expr: Vec<Expression> = add
                    .terms
                    .iter()
                    .map(|expr| match expr {
                        Expression::Numeral(num) => Expression::multiplication(
                            Multiplication::mul_num(*num, self.coef),
                            self.monomes.clone(),
                        ),
                        _ => {
                            let mut monomes =
                                vec![Monome::new(expr.clone(), Expression::number(1))];
                            monomes.extend(self.monomes.clone());
                            Expression::multiplication(self.coef, monomes)
                        }
                    })
                    .collect();
                return Expression::addition(extended_expr).simplify();
            }
        } else {
            // Mul(self.coef ,      self.monomes              )
            // Mul( coef,       [Mon(x, z), Mon(x2, z2), ...] ) => Expression(x)
            let mut i = 0;
            while i < self.monomes.len() {
                match (
                    // x
                    self.monomes.get(i).unwrap().get_term(),
                    // z
                    self.monomes.get(i).unwrap().get_power(),
                ) {
                    // x & z are Num
                    (Expression::Numeral(num), Expression::Numeral(exp)) => {
                        match Exponentiation::exp_num(*num, *exp)? {
                            //  Mul(coef*x^z, ...)
                            Expression::Numeral(num) => {
                                self.coef = Multiplication::mul_num(num, self.coef);
                                self.monomes.swap_remove(i);
                            }
                            // Mul(coef, [Mon(x^z, 1), ...])
                            x => {
                                self.monomes[i] = x.to_monome().expect("Monome");
                            }
                        }
                    }
                    // x is Mul(a, b) & z is Expr
                    // b = [Mon(y, w), Mon(y2, w2), ...]
                    (Expression::Multiplication(mul), pow) => {
                        // z = Num(1)
                        // Mul(coef*a, [b, ...])
                        if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = pow {
                            self.coef = Multiplication::mul_num(mul.coef, self.coef);
                            self.monomes.extend(mul.monomes.clone());
                            self.monomes.swap_remove(i);
                        }
                        // z is Expr
                        else {
                            // b => [Mon(y, Mul(1, Mon(w*z)), Mon(y2, Mul(1, Mon(w2*z)), ...]
                            let monomes_powered = mul
                                .monomes
                                .iter()
                                .map(|mon| {
                                    Monome::new(
                                        mon.term.clone(),
                                        Expression::multiplication(
                                            Numeral::number(1),
                                            vec![
                                                Monome::new(
                                                    mon.power.clone(),
                                                    Expression::number(1),
                                                ),
                                                Monome::new(pow.clone(), Expression::number(1)),
                                            ],
                                        ),
                                    )
                                    .simplify()
                                })
                                .collect::<Result<Vec<Monome>, anyhow::Error>>()?;

                            match Expression::exponentiation(
                                Expression::Numeral(mul.coef),
                                pow.clone(),
                            )
                            .simplify()?
                            {
                                // Mul(coef*a^z, [Mon(y, Mul(1, Mon(w*z)), Mon(y2, Mul(1, Mon(w2*z)), ...])
                                Expression::Numeral(num) => {
                                    self.coef = Multiplication::mul_num(num, self.coef);
                                    self.monomes.extend(monomes_powered);
                                }
                                // Mul(coef, [Mon(a, z), [Mon(y, Mul(1, Mon(w*z)), Mon(y2, Mul(1, Mon(w2*z)), ...])
                                x => {
                                    println!("{}", x);
                                    self.monomes.push(x.to_monome().expect("Monome"));
                                    self.monomes.extend(monomes_powered);
                                }
                            }

                            self.monomes.swap_remove(i);
                        }
                    }
                    // x is exp(a, b) & z is Expr
                    (Expression::Exponentiation(exp), pow) => {
                        // Mon(exp(a, b), 1) => Mon(a, b)
                        if let Expression::Numeral(Numeral::Number(Number { num: 1 })) = pow {
                            let reduced_mon = Monome::new(exp.base.clone(), exp.exponent.clone());
                            self.monomes[i] = reduced_mon;
                        }
                        // Mon(exp(a, b), z) => Mon(a, exp(b, z))
                        else {
                            let reduced_mon = Monome::new(
                                exp.base.clone(),
                                Expression::exponentiation(exp.exponent.clone(), pow.clone()),
                            );
                            self.monomes[i] = reduced_mon;
                        }
                    }
                    _ => {
                        // todo!("Add expr to monomes && mult in mult handle");
                        i += 1
                    }
                }
            }
        }
        Ok(Expression::Multiplication(Box::new(self)))
    }

    pub fn simplify(mut self) -> Result<Expression, anyhow::Error> {
        if let Numeral::Number(Number { num: 0 }) = self.coef {
            return Ok(Expression::number(0));
        }

        self.monomes = self
            .monomes
            .iter_mut()
            .map(|expr| expr.clone().simplify())
            .collect::<Result<Vec<Monome>, anyhow::Error>>()?;

        if !self.is_reduced() {
            match self.reduce()? {
                Expression::Multiplication(mul) => self = *mul,
                x => return x.simplify(),
            }
        }

        if self.monomes.len() == 1 {
            if let Expression::Numeral(numeral) = self.monomes[0].term {
                self.coef = Multiplication::mul_num(numeral, self.coef);
                self.monomes.remove(0);
            }
        } else {
            let mut i = 0;
            while i < self.monomes.len() {
                let mut j = i + 1;
                while j < self.monomes.len() {
                    match (self.monomes.get(i).unwrap(), self.monomes.get(j).unwrap()) {
                        // Multiplication * Multiplication
                        
                        // Expression * Addition
                        // Expression * Negation
                        // Complex(a, b) * Complex(c, d) => ac + bic + adi - bidi
                        (
                            Monome {
                                term: Expression::Complex(comp),
                                power: Expression::Numeral(Numeral::Number(Number { num: 1 })),
                            },
                            Monome {
                                term: Expression::Complex(comp2),
                                power: Expression::Numeral(Numeral::Number(Number { num: 1 })),
                            },
                        ) => {
                            self.monomes[i] = Monome {
                                term: Expression::addition(vec![
                                    Expression::multiplication_from_expr(
                                        Numeral::number(1),
                                        vec![comp.real.clone(), comp2.real.clone()],
                                    ),
                                    Expression::multiplication_from_expr(
                                        Numeral::number(1),
                                        vec![
                                            comp.real.clone(),
                                            Expression::imaginary(comp2.imag.clone()),
                                        ],
                                    ),
                                    Expression::multiplication_from_expr(
                                        Numeral::number(1),
                                        vec![
                                            Expression::imaginary(comp.imag.clone()),
                                            Expression::imaginary(comp2.imag.clone()),
                                        ],
                                    ),
                                    Expression::multiplication_from_expr(
                                        Numeral::number(1),
                                        vec![
                                            Expression::imaginary(comp.imag.clone()),
                                            comp2.real.clone(),
                                        ],
                                    ),
                                ]),
                                power: Expression::Numeral(Numeral::Number(Number { num: 1 })),
                            };
                            self.monomes.swap_remove(j);
                        }
                        // Expression * Complex

                        // Expression * 0 => return 0


                        
                        // Expression * 1 => Expression

                        // Fraction * Fraction
                        // Expression(expr) * Fraction(num, den) => Fraction(expr*num, den)
                        (
                            Monome {
                                term: Expression::Fraction(frac),
                                power,
                            },
                            expr,
                        )
                        | (
                            expr,
                            Monome {
                                term: Expression::Fraction(frac),
                                power,
                            },
                        ) => {
                            let new_mult = Expression::multiplication(
                                Numeral::number(1),
                                vec![
                                    Monome {
                                        term: frac.num.clone(),
                                        power: power.clone(),
                                    },
                                    expr.clone(),
                                ],
                            );
                            self.monomes[i] = Monome {
                                term: Expression::fraction(
                                    new_mult,
                                    Expression::exponentiation(frac.den.clone(), power.clone()),
                                ),
                                power: Expression::number(1),
                            }
                            .simplify()?;

                            self.monomes.swap_remove(j);
                        }
                        // Expression * Imaginary
                        (
                            _,
                            Monome {
                                term: Expression::Imaginary(img),
                                power: Expression::Numeral(Numeral::Number(Number { num: 1 })),
                            },
                        ) => {
                            self.monomes.swap_remove(j);
                            return Ok(Expression::imaginary(Expression::Multiplication(
                                Box::new(self),
                            ))
                            .simplify()?);
                        }
                        // Numeral * Expression
                        // (
                        //     _,
                        //     Monome {
                        //         term: Expression::Numeral(numeral),
                        //         power: Expression::Numeral(numeral_pow),
                        //     },
                        // ) => {
                        //     self.coef = Multiplication::mul_num(
                        //         Exponentiation::exp_num(*numeral, *numeral_pow),
                        //         self.coef,
                        //     );
                        //     self.monomes.swap_remove(j);
                        // }
                        // a^y * a^x
                        (monome1, monome2) if monome1.term == monome2.term => {
                            let power = Expression::addition(vec![
                                self.monomes[i].power.clone(),
                                self.monomes[j].power.clone(),
                            ])
                            .simplify()?;
                            // (x + y) != 0
                            if Expression::Numeral(Numeral::Number(Number { num: 0 })) != power {
                                self.monomes[i].power = power;
                                self.monomes.swap_remove(j);
                            } else {
                                self.monomes.swap_remove(j);
                                self.monomes.swap_remove(i);
                            }
                        }
                        _ => {
                            // if i + 1 == self.monomes.len() {
                            //     self.monomes.swap(a, b);
                            // }
                            j += 1
                        }
                    }
                }
                i += 1
            }
        }

        Ok(self.verify())
    }

    fn verify(self) -> Expression {
        match self.coef {
            Numeral::Number(Number { num: 0 }) => Expression::number(0),
            _ if self.monomes.is_empty() => Expression::Numeral(self.coef),
            _ if self.monomes.len() == 1 => match self.coef {
                Numeral::Number(Number { num: 1 }) => self.monomes[0].clone().to_exponentation(),
                Numeral::Number(Number { num: -1 }) => {
                    Expression::negation(self.monomes[0].clone().to_exponentation())
                }
                _ => Expression::Multiplication(Box::new(self)),
            },
            _ => Expression::Multiplication(Box::new(self)),
        }
    }
}

impl Display for Multiplication {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}",
            if let Numeral::Number(Number { num: 1 }) = self.coef {
                self.coef.to_string()
            } else {
                "".to_string()
            }
        )?;
        for expr in self.monomes.iter() {
            write!(f, " * ")?;
            write!(f, "{}", expr)?;
        }
        write!(f, ")",)?;
        Ok(())
    }
}
