use crate::{
    ast::{expression::Expression, function::Function, numeral::Numeral, statement::Statement},
    lexer::{Lexer, Span, Token, TokenKind},
    utils::gcd,
};

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("No Token to parse")]
    NoToken,
    #[error("Unexpected Token: <{0}>")]
    UnexpectedToken(TokenKind),
    #[error("Invalid TokenKind: Expected <{expected}>, found <{found}>")]
    UnexpectedTokenKind {
        expected: TokenKind,
        found: TokenKind,
    },
    #[error("Invalid Expression: Expected <Variable> OR <Equality>, found <{0}>")]
    ExpectedVarOrEquality(Expression),
    #[error("Invalid Expression: Expected <Variable> found <{0}>")]
    ExpectedVariable(Expression),
    #[error("Invalid Expression: Expected <Equality> found <{0}>")]
    ExpectedEquality(Expression),
    #[error("Invalid Number: Expected <i64> OR <f64>, found <{0}>")]
    ExpectedNumber(String),
    #[error(
        "Invalid number of argument(s): Function '{0}' expects {1} argument(s), but was given {2}"
    )]
    InvalidArgumentCount(String, usize, usize),
    #[error("Invalid degree of derivation: Expected same values, found <{0}> and <{1}>")]
    InvalidDegreeOfDerivation(Expression, Expression),
    #[error("Invalid variable of derivation: Expected <Literal>, found <{0}>")]
    InvalidVariableOfDerivation(TokenKind),
}

#[derive(Debug, Default)]
pub struct Parser {
    pub position: usize,
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<ParserError>,
}

impl Parser {
    /// Lex the input string
    pub fn lex(mut self, text: &str) -> Result<Self, anyhow::Error> {
        let mut lexer = Lexer::new(text.chars().collect());

        let mut token: Token = lexer.next_token()?;
        let mut tokens: Vec<Token> = Vec::new();

        while token.kind != TokenKind::End {
            if token.kind != TokenKind::WhiteSpace {
                tokens.push(token);
            }
            token = lexer.next_token()?;
        }

        if token.kind == TokenKind::End {
            tokens.push(token);
        }
        self.tokens = tokens;
        Ok(self)
    }

    fn insert_tokens(&mut self, elem: Token) {
        self.tokens.insert(self.position, elem);
    }

    fn peek_token(&mut self, offset: isize) -> Token {
        let index: usize = (self.position as isize + offset).try_into().unwrap();
        self.tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.last().unwrap())
            .clone()
    }

    fn next_token(&mut self) -> Token {
        let current = self.current_token().clone();
        self.position += 1;
        current
    }

    fn previous_token(&mut self) -> Token {
        let current = self.current_token().clone();
        self.position -= 1;
        current
    }

    fn current_token(&mut self) -> Token {
        self.peek_token(0)
    }

    fn equal_or_create_next(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.next_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.push(ParserError::UnexpectedTokenKind {
                expected: kind,
                found: current,
            });
            Token {
                kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    fn equal_or_create(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.current_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.push(ParserError::UnexpectedTokenKind {
                expected: kind,
                found: current,
            });
            Token {
                kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    fn is_equal(&mut self, kind: TokenKind) -> bool {
        self.current_token().kind == kind
    }

    /// Parse the tokens previously lexed and return a Statement or error(s)
    pub fn parse(&mut self) -> Result<Statement, Vec<ParserError>> {
        if self.tokens.is_empty() {
            self.diagnostics.push(ParserError::NoToken);
            return Err(self.diagnostics.clone());
        } else {
            // if the statement are explicit
            if let Some(state) = self.parse_statement() {
                self.next_token();
                self.equal_or_create_next(TokenKind::LeftParenthesis);

                match state {
                    StatementKind::Solve => {
                        if let Some(expr) = self.parse_type() {
                            self.equal_or_create(TokenKind::RightParenthesis);
                            if self.diagnostics.is_empty() {
                                return Ok(Statement::Solve(expr));
                            } else {
                                return Err(self.diagnostics.clone());
                            }
                        } else {
                            self.diagnostics.push(ParserError::InvalidArgumentCount(
                                String::from("Solve"),
                                1,
                                0,
                            ))
                        }
                    }
                    StatementKind::Simplify => {
                        if let Some(expr) = self.parse_type() {
                            self.equal_or_create(TokenKind::RightParenthesis);
                            if self.diagnostics.is_empty() {
                                return Ok(Statement::Simplify(expr));
                            } else {
                                return Err(self.diagnostics.clone());
                            }
                        } else {
                            self.diagnostics.push(ParserError::InvalidArgumentCount(
                                String::from("Simplify"),
                                1,
                                0,
                            ));
                            return Err(self.diagnostics.clone());
                        }
                    }
                    StatementKind::SolveFor => {
                        if let Some(expr) = self.parse_type() {
                            if TokenKind::Comma == self.next_token().kind {
                                if let Some(expr2) = self.parse_type() {
                                    if let Expression::Variable(var) = expr2 {
                                        self.equal_or_create(TokenKind::RightParenthesis);
                                        if self.diagnostics.is_empty() {
                                            return Ok(Statement::SolveFor(expr, var));
                                        } else {
                                            return Err(self.diagnostics.clone());
                                        }
                                    } else {
                                        self.diagnostics.push(ParserError::ExpectedVariable(expr2))
                                    }
                                } else {
                                    self.diagnostics.push(ParserError::InvalidArgumentCount(
                                        String::from("SolveFor"),
                                        2,
                                        1,
                                    ))
                                }
                            } else {
                                self.diagnostics.push(ParserError::InvalidArgumentCount(
                                    String::from("SolveFor"),
                                    2,
                                    1,
                                ))
                            }
                        } else {
                            self.diagnostics.push(ParserError::InvalidArgumentCount(
                                String::from("SolveFor"),
                                2,
                                0,
                            ))
                        }
                    }
                }
            } else {
                while let Some(expr) = self.parse_type() {
                    if self.current_token().kind == TokenKind::Comma {
                        // TODO add a loop to get second equality if substitue or for later sys eq
                        self.next_token();
                        let after_comma = self.parse_expression();

                        if let Expression::Variable(var) = after_comma {
                            if self.diagnostics.is_empty() {
                                return Ok(Statement::SolveFor(expr, var));
                            } else {
                                return Err(self.diagnostics.clone());
                            }
                        } else {
                            self.diagnostics
                                .push(ParserError::ExpectedVariable(after_comma));
                            return Err(self.diagnostics.clone());
                        }
                    } else {
                        // if current is not end throw error and try parse the rest to maybe show other error(s)
                        if self.current_token().kind != TokenKind::End {
                            let current = self.current_token().kind;
                            self.diagnostics.push(ParserError::UnexpectedToken(current));
                            self.next_token();
                        } else if self.diagnostics.is_empty() {
                            if let Expression::Equality(_) = expr {
                                return Ok(Statement::Solve(expr));
                            } else {
                                return Ok(Statement::Simplify(expr));
                            }
                        } else {
                            return Err(self.diagnostics.clone());
                        }
                    }
                }
            }
        }
        Err(self.diagnostics.clone())
    }

    fn parse_statement(&mut self) -> Option<StatementKind> {
        if self.current_token().kind == TokenKind::Literal {
            match self.current_token().text.as_str() {
                "simplify" => Some(StatementKind::Simplify),
                "solve" => Some(StatementKind::Solve),
                "solvefor" => Some(StatementKind::SolveFor),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Option<Expression> {
        if self.current_token().kind == TokenKind::End {
            return None;
        }
        Some(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary_expression(None, 0)
    }

    fn parse_binary_expression(
        &mut self,
        left: Option<Expression>,
        precedence: usize,
    ) -> Expression {
        let mut left = if let Some(expr) = left {
            expr
        } else {
            self.parse_unary_expression()
        };

        while let Some(mut operator) = self.parse_binary_operator() {
            let op_precedence = operator.precedence();
            if op_precedence < precedence
                || (precedence != BinaryOperatorKind::Exponentiation.precedence()
                    && op_precedence == precedence)
            {
                break;
            }
            self.next_token();

            let right = self.parse_binary_expression(None, op_precedence);
            left = match operator {
                BinaryOperatorKind::Addition => Expression::addition(vec![left, right]),
                BinaryOperatorKind::Subtraction => Expression::substraction(left, right),
                BinaryOperatorKind::Multiplication => {
                    Expression::multiplication_from_expr(Numeral::number(1), vec![left, right])
                }
                BinaryOperatorKind::Division => Expression::fraction(left, right),
                BinaryOperatorKind::Exponentiation => Expression::exponentiation(left, right),
                BinaryOperatorKind::Equality => return Expression::equality(left, right),
            }
        }
        left
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperatorKind> {
        match self.current_token().kind {
            TokenKind::Minus => Some(BinaryOperatorKind::Subtraction),
            TokenKind::Plus => Some(BinaryOperatorKind::Addition),
            TokenKind::Slash => Some(BinaryOperatorKind::Division),
            TokenKind::Star => Some(BinaryOperatorKind::Multiplication),
            TokenKind::Hat => Some(BinaryOperatorKind::Exponentiation),
            TokenKind::Equal => Some(BinaryOperatorKind::Equality),
            _ => None,
        }
    }

    fn parse_unary_expression(&mut self) -> Expression {
        while let Some(operator) = self.parse_unary_operator() {
            self.next_token();
            let operand = self.parse_unary_expression();

            return match operator {
                UnaryOperatorKind::Negation => Expression::negation(operand),
            };
        }
        self.parse_primary()
    }

    fn parse_unary_operator(&mut self) -> Option<UnaryOperatorKind> {
        match self.current_token().kind {
            TokenKind::Minus => Some(UnaryOperatorKind::Negation),
            _ => None,
        }
    }

    fn parse_primary(&mut self) -> Expression {
        let current = self.current_token();
        match current.kind {
            TokenKind::Number => {
                let expression = self.parse_number_expression();
                self.next_token();

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::LeftParenthesis
                    || self.current_token().kind == TokenKind::Literal
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        "*".to_string(),
                    ))
                }
                expression
            }
            TokenKind::Literal => {
                let expression = self.parse_literal_expression();
                self.next_token();

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::Number
                    || self.current_token().kind == TokenKind::Literal
                    || self.current_token().kind == TokenKind::LeftParenthesis
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        "*".to_string(),
                    ))
                }
                expression
            }
            TokenKind::LeftParenthesis => {
                self.next_token();
                let expression = self.parse_expression();
                self.equal_or_create_next(TokenKind::RightParenthesis);

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::LeftParenthesis
                    || self.current_token().kind == TokenKind::Number
                    || self.current_token().kind == TokenKind::Literal
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        String::from("*"),
                    ))
                }
                expression
            }
            _ => {
                self.diagnostics
                    .push(ParserError::UnexpectedToken(current.kind));
                self.next_token();
                Expression::number(0)
            }
        }
    }

    fn parse_number_expression(&mut self) -> Expression {
        let current = self.current_token();

        match current.text.parse::<i64>() {
            // 5
            Ok(whole_part) => {
                if self.peek_token(1).kind == TokenKind::Dot {
                    self.next_token();
                    self.next_token();

                    // 5.
                    match self.current_token().text.parse::<i64>() {
                        // 5.5
                        Ok(fractionnal_part) => {
                            let n: u32 = self.current_token().text.len() as u32;

                            let denominator: i64 = 10_i64.pow(n);
                            let numerator = whole_part * denominator + fractionnal_part;

                            let gcd = gcd(fractionnal_part, denominator);

                            dbg!(&gcd);

                            Expression::rationnal(numerator / gcd, denominator / gcd).unwrap()
                        }
                        // 5.?
                        Err(_) => {
                            self.diagnostics
                                .push(ParserError::ExpectedNumber(current.text));
                            Expression::number(0)
                        }
                    }
                } else {
                    Expression::number(whole_part)
                }
            }
            // ?
            Err(_) => {
                self.diagnostics
                    .push(ParserError::ExpectedNumber(current.text));
                Expression::number(0)
            }
        }
    }

    fn parse_literal_expression(&mut self) -> Expression {
        // if let Some(expr) = self.parse_derivate() {
        //     return expr;
        // }

        let mut text = self.current_token().text;

        let mut components: Vec<Expression> = vec![];

        if self.peek_token(1).kind == TokenKind::LeftParenthesis {
            if let Some(func) = self.parse_function(&text) {
                return func;
            }
        }
        if let Some(cons) = self.parse_constant(&text) {
            return cons;
        }

        text = self.parse_imaginary_unit(text, &mut components);

        if !text.is_empty() {
            components.push(self.parse_variable(text, true));
        }

        if components.len() == 1 {
            components[0].clone()
        } else {
            Expression::multiplication_from_expr(Numeral::number(1), components)
        }
    }

    /// peek_hat is for abc^2 => a * b * c^2 and not (abc)^2
    fn parse_variable(&mut self, mut text: String, peek_hat: bool) -> Expression {
        let mut components: Vec<Expression> = vec![];

        // check if the last var as a power
        if peek_hat && self.peek_token(1).kind == TokenKind::Hat {
            self.next_token();
            let var = text.chars().last().unwrap();
            components.push(self.parse_binary_expression(
                Some(Expression::variable(var.to_string())),
                BinaryOperatorKind::Multiplication.precedence(),
            ));
            text.pop();
            self.previous_token();
            components.extend(
                text.chars()
                    .map(|var| Expression::variable(var.to_string())),
            );
        } else {
            components.extend(
                text.chars()
                    .map(|var| Expression::variable(var.to_string())),
            );
        }

        if components.len() == 1 {
            components[0].clone()
        } else {
            Expression::multiplication_from_expr(Numeral::number(1), components)
        }
    }

    fn parse_imaginary_unit(
        &mut self,
        mut text: String,
        components: &mut Vec<Expression>,
    ) -> String {
        while let Some(num) = text.find('i') {
            if num == text.len() - 1 {
                if self.peek_token(1).kind == TokenKind::Hat {
                    self.next_token();
                    components.push(self.parse_binary_expression(
                        Some(Expression::imaginary(Expression::number(1))),
                        BinaryOperatorKind::Multiplication.precedence(),
                    ));
                    self.previous_token();
                } else {
                    components.push(Expression::imaginary(Expression::number(1)));
                }
            } else {
                components.push(Expression::imaginary(Expression::number(1)));
            }
            text = text[..num].to_string() + &text[num + 1..];
        }
        text
    }

    /// if `None` is return this means that a function is entered with the wrong numbers of argments
    fn parse_function(&mut self, text: &str) -> Option<Expression> {
        // TODO add parsing of user defined function.

        // If the text is not a function return none
        let arg_number = Function::get_args_number(text)?;

        let mut args = Vec::new();

        self.next_token();
        self.equal_or_create_next(TokenKind::LeftParenthesis);

        if self.current_token().kind != TokenKind::RightParenthesis {
            args.push(self.parse_expression());
            for _ in 0..arg_number {
                if self.current_token().kind == TokenKind::Comma {
                    args.push(self.parse_expression())
                } else {
                    break;
                }
            }
        }

        if self.is_equal(TokenKind::RightParenthesis) {
            if args.len() == arg_number {
                Expression::function(text, args).ok()
            } else {
                self.diagnostics.push(ParserError::InvalidArgumentCount(
                    text.to_string(),
                    arg_number,
                    args.len(),
                ));
                None
            }
        } else {
            while self.next_token().kind != TokenKind::RightParenthesis {
                args.push(Expression::e())
            }
            self.diagnostics.push(ParserError::InvalidArgumentCount(
                text.to_string(),
                arg_number,
                args.len(),
            ));
            None
        }
    }

    fn parse_constant(&mut self, text: &str) -> Option<Expression> {
        Expression::constant(text).ok()
    }

    // d / dy (xxx)
    // fn parse_derivate(&mut self) -> Option<Expression> {
    //     // TODO add degree of derivation

    //     let begin_position = self.position;
    //     let text = self.current_token().text;

    //     if "d" == text {
    //         self.next_token();
    //         let derivation_degree: Option<Expression> = if self.is_equal(TokenKind::Hat) {
    //             // d^
    //             self.next_token();

    //             let degree = if self.is_equal(TokenKind::Number) {
    //                 // d^x
    //                 self.parse_number_expression()
    //             } else {
    //                 self.position = begin_position;
    //                 return None;
    //             };
    //             Some(degree)
    //         } else {
    //             None
    //         };
    //         // d{^x}
    //         if let (TokenKind::Slash, TokenKind::Literal) =
    //             (self.current_token().kind, self.peek_token(1).kind)
    //         {
    //             // d{^x}/lit
    //             let text_peeked = self.peek_token(1).text;
    //             if text_peeked.starts_with('d') && text_peeked.len() >= 2 {
    //                 // d{^x}/dlit
    //                 self.next_token();

    //                 let derivation_variable = if let Expression::Variable(var) =
    //                     self.parse_variable(String::from(&text_peeked[1..]), false)
    //                 {
    //                     var
    //                 } else {
    //                     self.diagnostics
    //                         .push(ParserError::InvalidVariableOfDerivation(
    //                             self.tokens
    //                                 .get(self.position)
    //                                 .unwrap_or_else(|| self.tokens.last().unwrap())
    //                                 .kind,
    //                         ));
    //                     Variable::new(String::from("var€"))
    //                 };

    //                 self.next_token();

    //                 if self.is_equal(TokenKind::Hat) {
    //                     if let Some(degree) = derivation_degree.clone() {
    //                         // d^x/dy^
    //                         self.next_token();
    //                         let num = self.parse_number_expression();
    //                         if !self.is_equal(TokenKind::Number) && !num.equal(&degree) {
    //                             self.diagnostics
    //                                 .push(ParserError::InvalidDegreeOfDerivation(degree, num));
    //                             self.position = begin_position;
    //                             return None;
    //                         }
    //                         // d^x/dy^x
    //                     } else {
    //                         // d/dy^
    //                         self.diagnostics.push(ParserError::UnexpectedTokenKind {
    //                             expected: TokenKind::LeftParenthesis,
    //                             found: TokenKind::Hat,
    //                         });
    //                         self.next_token();
    //                         self.parse_number_expression();
    //                         self.position = begin_position;
    //                         return None;
    //                     }
    //                 }
    //                 // d{^x}/dy{^x}(expr)
    //                 if !self.is_equal(TokenKind::LeftParenthesis) {
    //                     self.position = begin_position;
    //                     return None;
    //                 }
    //                 self.next_token();
    //                 let expression = self.parse_expression();
    //                 self.equal_or_create(TokenKind::RightParenthesis);

    //                 return Some(Expression::derivation(
    //                     expression,
    //                     derivation_variable,
    //                     derivation_degree,
    //                 ));
    //             }
    //             return None;
    //         }
    //         return None;
    //     }
    //     None
    // }
}

#[derive(PartialEq, Debug, Clone)]
pub enum StatementKind {
    Solve,
    SolveFor,
    Simplify,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentiation,
    Equality,
}

impl BinaryOperatorKind {
    pub fn precedence(&mut self) -> usize {
        match self {
            BinaryOperatorKind::Exponentiation => 4,
            BinaryOperatorKind::Multiplication | BinaryOperatorKind::Division => 3,
            BinaryOperatorKind::Addition | BinaryOperatorKind::Subtraction => 2,
            BinaryOperatorKind::Equality => 1,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperatorKind {
    Negation,
}

#[cfg(test)]
mod tests_parser {
    use crate::ast::{
        expression::Expression, monome::Monome, numeral::Numeral, statement::Statement,
    };

    use super::Parser;

    fn verify_parser(input: &str, expected: Statement) -> (bool, String) {
        match Parser::default().lex(input) {
            Ok(mut parser) => match parser.parse() {
                Ok(statement) => (
                    statement.equal(&expected),
                    format!(
                        "Inuput : {:?} \nResult : {:?}\nExpected : {:?}",
                        input, statement, expected
                    ),
                ),
                Err(err) => {
                    let mut errors = String::new();
                    err.iter().for_each(|e| errors += &format!("{} \n", e));
                    (false, errors)
                }
            },
            Err(err) => (false, format!("{}", err)),
        }
    }

    #[test]
    fn negative_number() {
        let mut res = verify_parser(
            "-2",
            Statement::Simplify(Expression::negation(Expression::number(2))),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "--2",
            Statement::Simplify(Expression::negation(Expression::negation(
                Expression::number(2),
            ))),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "-2 + 3",
            Statement::Simplify(Expression::addition(vec![
                Expression::negation(Expression::number(2)),
                Expression::number(3),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3-2",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(3),
                Expression::negation(Expression::number(2)),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3*-2",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::number(3),
                    Expression::negation(Expression::number(2)),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3/-3",
            Statement::Simplify(Expression::fraction(
                Expression::number(3),
                Expression::negation(Expression::number(3)),
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "6^-5",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(6),
                Expression::negation(Expression::number(5)),
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3-2+1",
            Statement::Simplify(Expression::addition(vec![
                Expression::addition(vec![
                    Expression::number(3),
                    Expression::negation(Expression::number(2)),
                ]),
                Expression::number(1),
            ])),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn float() {
        let mut res = verify_parser(
            "2.5",
            Statement::Simplify(Expression::rationnal(5, 2).unwrap()),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "45.76",
            Statement::Simplify(Expression::rationnal(1144, 25).unwrap()),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "0.5",
            Statement::Simplify(Expression::rationnal(1, 2).unwrap()),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "7.0",
            Statement::Simplify(Expression::rationnal(7, 1).unwrap()),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn parenthesis() {
        let mut res = verify_parser(
            "(2+4)",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(2),
                Expression::number(4),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "2+(4+2)",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(2),
                Expression::addition(vec![Expression::number(4), Expression::number(2)]),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "2+(4+2)+3",
            Statement::Simplify(Expression::addition(vec![
                Expression::addition(vec![
                    Expression::number(2),
                    Expression::addition(vec![Expression::number(4), Expression::number(2)]),
                ]),
                Expression::number(3),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "((2/5)(4+1))3",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![
                            Expression::fraction(Expression::number(2), Expression::number(5)),
                            Expression::addition(vec![
                                Expression::number(4),
                                Expression::number(1),
                            ]),
                        ],
                    ),
                    Expression::number(3),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "-(2+4)",
            Statement::Simplify(Expression::negation(Expression::addition(vec![
                Expression::number(2),
                Expression::number(4),
            ]))),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn exponentiation() {
        let mut res = verify_parser(
            "3^2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(Expression::number(2), Expression::number(3)),
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3^2^3^2",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(
                    Expression::number(2),
                    Expression::exponentiation(Expression::number(3), Expression::number(2)),
                ),
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "(3^2)^-8",
            Statement::Simplify(Expression::exponentiation(
                Expression::exponentiation(Expression::number(3), Expression::number(2)),
                Expression::negation(Expression::number(8)),
            )),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "3^-2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(
                    Expression::negation(Expression::number(2)),
                    Expression::number(3),
                ),
            )),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn variable() {
        // a
        let mut res = verify_parser(
            "a",
            Statement::Simplify(Expression::variable("a".to_string())),
        );
        assert!(res.0, "{}", res.1);
        // b
        res = verify_parser(
            "b",
            Statement::Simplify(Expression::variable("b".to_string())),
        );
        assert!(res.0, "{}", res.1);

        // a_1

        // A_1

        // a_a

        // a_A

        // a_(i)

        // a_(5)

        // a_(ab)

        // a_(5b)

        // a_(4b + 8)
    }

    #[test]
    fn implicit_multiplication() {
        let mut res = verify_parser(
            "2(3)",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![Expression::number(2), Expression::number(3)],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2i",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![Expression::number(2), Expression::imaginary(Expression::number(1))],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2(3+2)",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::number(2),
                    Expression::addition(vec![Expression::number(3), Expression::number(2)]),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2(3+2)3",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![
                            Expression::number(2),
                            Expression::addition(vec![
                                Expression::number(3),
                                Expression::number(2),
                            ]),
                        ],
                    ),
                    Expression::number(3),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2(3+2)3(2+1)",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![
                            Expression::multiplication_from_expr(
                                Numeral::number(1),
                                vec![
                                    Expression::number(2),
                                    Expression::addition(vec![
                                        Expression::number(3),
                                        Expression::number(2),
                                    ]),
                                ],
                            ),
                            Expression::number(3),
                        ],
                    ),
                    Expression::addition(vec![Expression::number(2), Expression::number(1)]),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2a^2",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::number(2),
                    Expression::exponentiation(Expression::variable("a".to_string()), Expression::number(2)),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

    }

    #[test]
    fn fraction() {
        // 2/2c
        let mut res = verify_parser(
            "2/2c",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::fraction(Expression::number(2), Expression::number(2)),
                    Expression::variable("c".to_string()),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        // a/b
        res = verify_parser(
            "a/b",
            Statement::Simplify(Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            )),
        );
        assert!(res.0, "{}", res.1);

        // b * 1/a
        res = verify_parser(
            "b * 1/a",
            Statement::Simplify(Expression::fraction(
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![Expression::number(1), Expression::variable("b".to_string())],
                ),
                Expression::variable("a".to_string()),
            )),
        );
        assert!(res.0, "{}", res.1);

        // 2x * 1/(x+x^2)
        res = verify_parser(
            "2x * 1/(x+x^2)",
            Statement::Simplify(Expression::fraction(
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![
                        Expression::multiplication_from_expr(
                            Numeral::number(1),
                            vec![Expression::number(2), Expression::variable("x".to_string())],
                        ),
                        Expression::number(1),
                    ],
                ),
                Expression::addition(vec![
                    Expression::variable("x".to_string()),
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(2),
                    ),
                ]),
            )),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn variables() {
        let mut res = verify_parser(
            "x",
            Statement::Simplify(Expression::variable("x".to_string())),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "x+2",
            Statement::Simplify(Expression::addition(vec![
                Expression::variable("x".to_string()),
                Expression::number(2),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "x+2y",
            Statement::Simplify(Expression::addition(vec![
                Expression::variable("x".to_string()),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![Expression::number(2), Expression::variable("y".to_string())],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "x+2y+3",
            Statement::Simplify(Expression::addition(vec![
                Expression::addition(vec![
                    Expression::variable("x".to_string()),
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![Expression::number(2), Expression::variable("y".to_string())],
                    ),
                ]),
                Expression::number(3),
            ])),
        );
        assert!(res.0, "{}", res.1);
        res = verify_parser(
            "x+2y+3z",
            Statement::Simplify(Expression::addition(vec![
                Expression::addition(vec![
                    Expression::variable("x".to_string()),
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![Expression::number(2), Expression::variable("y".to_string())],
                    ),
                ]),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![Expression::number(3), Expression::variable("z".to_string())],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn function() {
        let mut res = verify_parser(
            "ln(2)",
            Statement::Simplify(Expression::ln(Expression::number(2))),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "cos()",
            Statement::Simplify(Expression::cos(Expression::number(1))),
        );
        assert_eq!(res.1, "Invalid number of argument(s): Function 'cos' expects 1 argument(s), but was given 0 \n", "{}", res.1);

        res = verify_parser(
            "ln(2+3)",
            Statement::Simplify(Expression::ln(Expression::addition(vec![
                Expression::number(2),
                Expression::number(3),
            ]))),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "ln(2)^2",
            Statement::Simplify(Expression::exponentiation(
                Expression::ln(Expression::number(2)),
                Expression::number(2),
            )),
        );
        assert!(res.0, "{}", res.1);

        // sin(2+3)
        res = verify_parser(
            "sin(2+3)",
            Statement::Simplify(Expression::sin(Expression::addition(vec![
                Expression::number(2),
                Expression::number(3),
            ]))),
        );
        assert!(res.0, "{}", res.1);

        // asin(2+3)
        res = verify_parser(
            "asin(2+3)",
            Statement::Simplify(Expression::asin(Expression::addition(vec![
                Expression::number(2),
                Expression::number(3),
            ]))),
        );
        assert!(res.0, "{}", res.1);

        //b sin(pi/2)
        res = verify_parser(
            "b sin(pi/2)",
            Statement::Simplify(Expression::multiplication_from_expr(
                Numeral::number(1),
                vec![
                    Expression::sin(Expression::fraction(
                        Expression::pi(),
                        Expression::number(2),
                    )),
                    Expression::variable("b".to_string()),
                ],
            )),
        );
        assert!(res.0, "{}", res.1);

        //a(x)

        //ab(x, r)

        //ab(x, r)b(x, t)
    }

    #[test]
    fn equality() {
        // 2= 2
        let mut res = verify_parser(
            "2=2",
            Statement::Solve(Expression::equality(
                Expression::number(2),
                Expression::number(2),
            )),
        );
        assert!(res.0, "{}", res.1);

        // 2+3= 2
        res = verify_parser(
            "2+3=2",
            Statement::Solve(Expression::equality(
                Expression::addition(vec![Expression::number(2), Expression::number(3)]),
                Expression::number(2),
            )),
        );
        assert!(res.0, "{}", res.1);

        // 2ln(4) = e^(-pi)
        res = verify_parser(
            "2ln(4) = e^(-pi)",
            Statement::Solve(Expression::equality(
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![Expression::number(2), Expression::ln(Expression::number(4))],
                ),
                Expression::exponentiation(Expression::e(), Expression::negation(Expression::pi())),
            )),
        );
        assert!(res.0, "{}", res.1);
    }

    #[test]
    fn complex() {
        let mut res = verify_parser(
            "a + bi",
            Statement::Simplify(Expression::addition(vec![
                Expression::variable("a".to_string()),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![
                        Expression::imaginary(Expression::number(1)),
                        Expression::variable("b".to_string()),
                    ],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "a + bi + c",
            Statement::Simplify(Expression::addition(vec![
                Expression::addition(vec![
                    Expression::variable("a".to_string()),
                    Expression::multiplication_from_expr(
                        Numeral::number(1),
                        vec![
                            Expression::imaginary(Expression::number(1)),
                            Expression::variable("b".to_string()),
                        ],
                    ),
                ]),
                Expression::variable("c".to_string()),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2 + 3i",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(2),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![
                        Expression::number(3),
                        Expression::imaginary(Expression::number(1)),
                    ],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "2 + 0i",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(2),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![
                        Expression::number(0),
                        Expression::imaginary(Expression::number(1)),
                    ],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "4 + i^2",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(4),
                Expression::exponentiation(
                    Expression::imaginary(Expression::number(1)),
                    Expression::number(2),
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);

        res = verify_parser(
            "4 + ii",
            Statement::Simplify(Expression::addition(vec![
                Expression::number(4),
                Expression::multiplication_from_expr(
                    Numeral::number(1),
                    vec![
                        Expression::imaginary(Expression::number(1)),
                        Expression::imaginary(Expression::number(1)),
                    ],
                ),
            ])),
        );
        assert!(res.0, "{}", res.1);
    }

    // #[test]
    // fn derivation() {
    //     d/dx(x + 1)
    //     let mut res = verify_parser(
    //         "d/dx(x + 1)",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::addition(Expression::variable("x".to_string()), Expression::number(1)),
    //             Variable::new("x".to_string()),
    //             None,
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d/dx(x^2)
    //     res = verify_parser(
    //         "d/dx(x^2)",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::exponentiation(
    //                 Expression::variable("x".to_string()),
    //                 Expression::number(2),
    //             ),
    //             Variable::new("x".to_string()),
    //             None,
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d/dx(2x*(y+2))
    //     res = verify_parser(
    //         "d/dx(2x*(y+2))",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::multiplication(
    //                 Expression::multiplication(
    //                     Expression::number(2),
    //                     Expression::variable("x".to_string()),
    //                 ),
    //                 Expression::addition(
    //                     Expression::variable("y".to_string()),
    //                     Expression::number(2),
    //                 ),
    //             ),
    //             Variable::new("x".to_string()),
    //             None,
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d/dx(2x*(y+2)^2)
    //     res = verify_parser(
    //         "d/dx(2x*(y+2)^2)",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::multiplication(
    //                 Expression::multiplication(
    //                     Expression::number(2),
    //                     Expression::variable("x".to_string()),
    //                 ),
    //                 Expression::exponentiation(
    //                     Expression::addition(
    //                         Expression::variable("y".to_string()),
    //                         Expression::number(2),
    //                     ),
    //                     Expression::number(2),
    //                 ),
    //             ),
    //             Variable::new("x".to_string()),
    //             None,
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d^2/dx^2(x^2)
    //     res = verify_parser(
    //         "d^2/dx^2(x^2)",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::exponentiation(
    //                 Expression::variable("x".to_string()),
    //                 Expression::number(2),
    //             ),
    //             Variable::new("x".to_string()),
    //             Some(Expression::number(2)),
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d^2/dx^2(x^2 + 2x)
    //     res = verify_parser(
    //         "d^2/dx^2(x^2 + 2x)",
    //         Statement::Simplify(Expression::derivation(
    //             Expression::addition(
    //                 Expression::exponentiation(
    //                     Expression::variable("x".to_string()),
    //                     Expression::number(2),
    //                 ),
    //                 Expression::multiplication(
    //                     Expression::number(2),
    //                     Expression::variable("x".to_string()),
    //                 ),
    //             ),
    //             Variable::new("x".to_string()),
    //             Some(Expression::number(2)),
    //         )),
    //     );        assert!(res.0, "{}", res.1);

    //     // d/dx^2(x^2)
    //     res = verify_parser(
    //         "d/dx^2(x^2)",
    //         Statement::Simplify(Expression::multiplication(
    //             Expression::fraction(
    //                 Expression::variable(String::from("d")),
    //                 Expression::multiplication(
    //                     Expression::variable(String::from("d")),
    //                     Expression::exponentiation(
    //                         Expression::variable(String::from("x")),
    //                         Expression::number(2),
    //                     ),
    //                 ),
    //             ),
    //             Expression::exponentiation(
    //                 Expression::variable(String::from("x")),
    //                 Expression::number(2),
    //             ),
    //         )),
    //     );        assert!(res.0, "{}", res.1);
    // }
}
