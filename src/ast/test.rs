#[cfg(test)]
mod test_simplify {

    use crate::{
        ast::{monome::Monome, number, numeral::Numeral, Expression},
        parser::Parser,
    };

    fn test(input: &str, expected: Expression) -> (bool, String) {
        match Parser::default()
            .lex(input)
            .unwrap()
            .parse()
            .unwrap()
            .reduce()
        {
            Ok(result) => (
                result.equal(&expected),
                format!(
                    "Inuput : {:?} \nResult : {:?}\nExpected : {:?}",
                    input, result, expected
                ),
            ),

            Err(err) => (false, format!("{}", err)),
        }
    }

    #[test]
    fn addition() {
        let mut res = test("2+2", Expression::number(4));
        assert!(res.0, "{}", res.1);

        res = test("4+2", Expression::number(6));
        assert!(res.0, "{}", res.1);

        res = test("4 - 1", Expression::number(3));
        assert!(res.0, "{}", res.1);

        res = test(
            "x+x",
            Expression::multiplication(
                Numeral::number(2),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(1),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x+y",
            Expression::addition(vec![
                Expression::variable("y".to_string()),
                Expression::variable("x".to_string()),
            ]),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x+x",
            Expression::multiplication(
                Numeral::number(3),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(1),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x + 2x",
            Expression::multiplication(
                Numeral::number(4),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(1),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test("x - x", Expression::number(0));
        assert!(res.0, "{}", res.1);

        res = test("e^x - e^x", Expression::number(0));
        assert!(res.0, "{}", res.1);

        // rationnal
        // frction$
        // complex
    }

    #[test]
    fn multiplication() {
        let mut res = test("2 * 2", Expression::number(4));
        assert!(res.0, "{}", res.1);

        res = test("4 * 2", Expression::number(8));
        assert!(res.0, "{}", res.1);

        res = test(" 4 * -1", Expression::number(-4));
        assert!(res.0, "{}", res.1);

        res = test(
            "x * x",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x * y",
            Expression::multiplication(
                Numeral::number(1),
                vec![
                    Monome::new(Expression::variable("y".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("x".to_string()), Expression::number(1)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x * x",
            Expression::multiplication(
                Numeral::number(2),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(2),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x * 2",
            Expression::multiplication(
                Numeral::number(4),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(1),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x * 2x",
            Expression::multiplication(
                Numeral::number(4),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(2),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x^4 * 6x^2",
            Expression::multiplication(
                Numeral::number(12),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(6),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x * (2x * 2x)",
            Expression::multiplication(
                Numeral::number(8),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(3),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2x * (2x * 2x)^2",
            Expression::multiplication(
                Numeral::number(32),
                vec![Monome {
                    term: Expression::variable("x".to_string()),
                    power: Expression::number(5),
                }],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x^3^x * x^x^3",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::addition(vec![
                    Expression::exponentiation(
                        Expression::number(3),
                        Expression::variable("x".to_string()),
                    ),
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(3),
                    ),
                ]),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test("y^x * y^-x", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test("y^(x/z) * y^-(x/z)", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test(
            "(a + b) * (a+b)",
            Expression::addition(vec![
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2),
                ),
                Expression::multiplication(
                    Numeral::number(2),
                    vec![
                        Monome::new(Expression::variable("a".to_string()), Expression::number(1)),
                        Monome::new(Expression::variable("b".to_string()), Expression::number(1)),
                    ],
                ),
                Expression::exponentiation(
                    Expression::variable("b".to_string()),
                    Expression::number(2),
                ),
            ]),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "(a + b) * (a-b)",
            Expression::addition(vec![
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2),
                ),
                Expression::exponentiation(
                    Expression::variable("b".to_string()),
                    Expression::number(2),
                ),
            ]),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "(a + bi)(a-bi)",
            Expression::addition(vec![
                Expression::exponentiation(
                    Expression::variable("a".to_string()),
                    Expression::number(2),
                ),
                Expression::exponentiation(
                    Expression::variable("b".to_string()),
                    Expression::number(2),
                ),
            ]),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn exponentiation() {
        let mut res = test("1^2", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test("0^2", Expression::number(0));
        assert!(res.0, "{}", res.1);

        res = test("2^2", Expression::number(4));
        assert!(res.0, "{}", res.1);

        res = test(
            "0^0",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::exponentiation(
                    Expression::variable("y".to_string()),
                    Expression::variable("z".to_string()),
                ),
            ),
        );
        assert_eq!(res.1, "The expression '0^0' is Undefined", "{}", res.1);

        res = test(
            "x^2",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::number(2),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test("x^1", Expression::variable("x".to_string()));
        assert!(res.0, "{}", res.1);

        res = test("x^0", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test(
            "x^x",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::variable("x".to_string()),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x^y^z",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::exponentiation(
                    Expression::variable("y".to_string()),
                    Expression::variable("z".to_string()),
                ),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "(x^y)^z",
            Expression::exponentiation(
                Expression::variable("x".to_string()),
                Expression::multiplication(
                    Numeral::number(1),
                    vec![
                        Monome::new(Expression::variable("y".to_string()), Expression::number(1)),
                        Monome::new(Expression::variable("z".to_string()), Expression::number(1)),
                    ],
                ),
            ),
        );
        // assert!(res.0, "{}", res.1);

        res = test(
            "x^-2",
            Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2),
                ),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x^-x",
            Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::variable("x".to_string()),
                ),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "x^-(x/2)",
            Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::fraction(
                        Expression::variable("x".to_string()),
                        Expression::number(2),
                    ),
                ),
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "(2xy)^2",
            Expression::multiplication(
                Numeral::number(4),
                vec![
                    Monome::new(Expression::variable("x".to_string()), Expression::number(2)),
                    Monome::new(Expression::variable("y".to_string()), Expression::number(2)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "2xy^2",
            Expression::multiplication(
                Numeral::number(2),
                vec![
                    Monome::new(Expression::variable("x".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("y".to_string()), Expression::number(2)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test("(2i)^2", Expression::Numeral(Numeral::number(-4)));
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn variable() {
        let mut res = test(
            "bln(2)^2",
            Expression::multiplication(
                Numeral::number(4),
                vec![
                    Monome::new(Expression::variable("b".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("l".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("n".to_string()), Expression::number(1)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "bln2^2",
            Expression::multiplication(
                Numeral::number(4),
                vec![
                    Monome::new(Expression::variable("b".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("l".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("n".to_string()), Expression::number(1)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "bln",
            Expression::multiplication(
                Numeral::number(1),
                vec![
                    Monome::new(Expression::variable("b".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("l".to_string()), Expression::number(1)),
                    Monome::new(Expression::variable("n".to_string()), Expression::number(1)),
                ],
            ),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn imaginary() {
        let mut res = test("i", Expression::imaginary(Expression::number(1)));
        assert!(res.0, "{}", res.1);

        res = test(
            "i^-1",
            Expression::negation(Expression::imaginary(Expression::number(1))),
        );
        assert!(res.0, "{}", res.1);

        res = test(
            "1/i",
            Expression::negation(Expression::imaginary(Expression::number(1))),
        );
        assert!(res.0, "{}", res.1);

        res = test("i^2", Expression::number(-1));
        assert!(res.0, "{}", res.1);

        res = test("i^-2", Expression::number(-1));
        assert!(res.0, "{}", res.1);

        res = test(
            "i^3",
            Expression::negation(Expression::imaginary(Expression::number(1))),
        );
        assert!(res.0, "{}", res.1);

        res = test("i^-3", Expression::imaginary(Expression::number(1)));
        assert!(res.0, "{}", res.1);

        res = test("i^4", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test("i^-4", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test("i^5", Expression::imaginary(Expression::number(1)));
        assert!(res.0, "{}", res.1);

        res = test("i^0", Expression::number(1));
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn rationnal() {
        let mut res = test("1/2", Expression::rationnal(1, 2).unwrap());
        assert!(res.0, "{}", res.1);

        res = test("2/2", Expression::number(1));
        assert!(res.0, "{}", res.1);

        res = test("2/1", Expression::number(2));
        assert!(res.0, "{}", res.1);

        res = test("4/6", Expression::rationnal(2, 3).unwrap());
        assert!(res.0, "{}", res.1);

        res = test("-1/-2", Expression::rationnal(1, 2).unwrap());
        assert!(res.0, "{}", res.1);

        res = test("-1/2", Expression::rationnal(-1, 2).unwrap());
        assert!(res.0, "{}", res.1);

        res = test("1/-2", Expression::rationnal(-1, 2).unwrap());
        assert!(res.0, "{}", res.1);
    }
}

