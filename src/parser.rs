use std::collections::HashMap;

use crate::lexer::{BinaryOperator, Keyword, Token, TokenType};

impl BinaryOperator {
    fn priority(&self) -> u32 {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Or => 0,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::And => 1,
            BinaryOperator::Index => 2,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionType {
    Boolean(bool),
    Natural(u64),
    Real(f64),
    Character(char),
    String(String),
    Noun(String),
    Negate(Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    Field(Box<Expression>, Identifier),
    Call(Vec<Expression>, Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type(pub Vec<Type>, pub Identifier);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument {
    external: bool,
    ty: Type,
    ident: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementType {
    // Conditional
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    // Loop
    Loop(Box<Statement>),
    Break,
    // Definitions
    Constant(Expression, Type, Identifier),
    Variable(Option<Expression>, Type, Identifier),
    FunctionLike {
        params: Option<Vec<Identifier>>,
        args: Vec<Argument>,
        ty: Option<Type>,
        ident: Identifier,
        statement: Option<Box<Statement>>,
    },
    Type {
        params: Vec<Identifier>,
        fields: Vec<(Type, Identifier)>,
        ident: Identifier,
    },
    // Block
    Block(Vec<Statement>),
    // Functions
    Call(Vec<Expression>, Identifier),
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub statement_type: StatementType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NameType {
    Noun,
    Function(usize),
    Procedure,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Scope {
    names: HashMap<String, NameType>,
    types: HashMap<String, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    scopes: Vec<Scope>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut names = HashMap::new();
        names.insert("емес".to_string(), NameType::Function(1));
        names.insert("артық".to_string(), NameType::Function(2));
        names.insert("кем".to_string(), NameType::Function(2));
        names.insert("қалдық".to_string(), NameType::Function(2));
        names.insert("тең".to_string(), NameType::Function(2));
        names.insert("жаз".to_string(), NameType::Procedure);
        names.insert("қос".to_string(), NameType::Procedure);
        names.insert("оқы".to_string(), NameType::Procedure);
        names.insert("орнат".to_string(), NameType::Procedure);
        let mut types = HashMap::new();
        types.insert("логикалық".to_string(), 0);
        types.insert("натурал".to_string(), 0);
        types.insert("бүтін".to_string(), 0);
        types.insert("нақты".to_string(), 0);
        types.insert("жиым".to_string(), 1);
        Self {
            tokens,
            index: 0,
            scopes: vec![Scope { names, types }],
        }
    }

    fn get_name_type(&self, name: &str) -> Option<NameType> {
        for scope in self.scopes.iter().rev() {
            if let Some(&name_type) = scope.names.get(name) {
                return Some(name_type);
            }
        }
        None
    }

    fn get_type_params(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(&params) = scope.types.get(name) {
                return Some(params);
            }
        }
        None
    }

    fn token(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn next_token(&mut self) {
        self.index += 1;
    }

    fn unexpected(&self) -> ! {
        if let Some(Token {
            token_type,
            line,
            column,
            ..
        }) = self.token()
        {
            panic!(
                "unexpected token {token_type:?} at {}:{}",
                line + 1,
                column + 1,
            );
        } else {
            panic!("unexpected end of file");
        }
    }

    fn expect(&self, expected: &str) -> ! {
        if let Some(Token {
            token_type,
            line,
            column,
            ..
        }) = self.token()
        {
            panic!(
                "unexpected token {token_type:?} at {}:{}, expected {expected}",
                line + 1,
                column + 1,
            );
        } else {
            panic!("unexpected end of file, expected {expected}");
        }
    }

    fn parse_term(&mut self) -> Option<Expression> {
        let &Token {
            ref token_type,
            line,
            column,
        } = self.token()?;
        let mut result = 'expr: {
            Expression {
                expression_type: 'expression_type: {
                    let expression_type = match token_type {
                        TokenType::Keyword(Keyword::True) => ExpressionType::Boolean(true),
                        TokenType::Keyword(Keyword::False) => ExpressionType::Boolean(false),
                        &TokenType::Natural(number) => ExpressionType::Natural(number),
                        &TokenType::Real(number) => ExpressionType::Real(number),
                        &TokenType::Character(ch) => ExpressionType::Character(ch),
                        TokenType::String(string) => ExpressionType::String(string.clone()),
                        TokenType::Identifier(name)
                            if self
                                .get_name_type(name)
                                .map_or(false, |name_type| name_type == NameType::Noun) =>
                        {
                            ExpressionType::Noun(name.clone())
                        }
                        TokenType::BinaryOperator(BinaryOperator::Subtract) => {
                            self.next_token();
                            if let Some(result) = self.parse_term() {
                                break 'expression_type ExpressionType::Negate(Box::new(result));
                            } else {
                                self.expect("an expression");
                            };
                        }
                        TokenType::OpeningParenthesis => {
                            self.next_token();
                            let Some(result) = self.parse_expression() else {
                                self.expect("an expression");
                            };
                            if let Some(Token {
                                token_type: TokenType::ClosingParenthesis,
                                ..
                            }) = self.token()
                            {
                                self.next_token();
                                break 'expr result;
                            } else {
                                self.expect("')'");
                            }
                        }
                        _ => return None,
                    };
                    self.next_token();
                    expression_type
                },
                line,
                column,
            }
        };
        loop {
            let (line, column) = match self.token() {
                Some(&Token {
                    token_type: TokenType::Field,
                    line,
                    column,
                }) => (line, column),
                _ => break Some(result),
            };
            self.next_token();
            let Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) = self.token()
            else {
                self.expect("an identifier");
            };
            result = Expression {
                expression_type: ExpressionType::Field(
                    Box::new(result),
                    Identifier {
                        name: name.clone(),
                        line,
                        column,
                    },
                ),
                line,
                column,
            };
            self.next_token();
        }
    }

    fn parse_postfix_operation(&mut self) -> Option<Expression> {
        let mut stack = Vec::new();
        let mut last_index = None;
        loop {
            if let Some(expr) = self.parse_term() {
                stack.push(expr);
                if stack.len() == 1 {
                    last_index = Some(self.index);
                }
                continue;
            }
            if let Some(&Token {
                token_type: TokenType::Identifier(ref name),
                line,
                column,
            }) = self.token()
            {
                if let Some(NameType::Function(count)) = self.get_name_type(name) {
                    if stack.len() < count {
                        panic!(
                            "not enough arguments for function {name} at {}:{}",
                            line + 1,
                            column + 1,
                        );
                    }
                    let args = stack.drain(stack.len() - count..).collect();
                    stack.push(Expression {
                        expression_type: ExpressionType::Call(
                            args,
                            Identifier {
                                name: name.clone(),
                                line,
                                column,
                            },
                        ),
                        line,
                        column,
                    });
                    self.next_token();
                    if stack.len() == 1 {
                        last_index = Some(self.index);
                    }
                    continue;
                }
            }
            if stack.is_empty() {
                break None;
            }
            if stack.len() > 1 {
                self.index = last_index.unwrap();
                stack.drain(1..);
            }
            break stack.pop();
        }
    }

    fn parse_binary_operation(&mut self, priority: u32) -> Option<Expression> {
        let mut result = self.parse_postfix_operation()?;
        loop {
            let (operator, line, column) = match self.token() {
                Some(&Token {
                    token_type: TokenType::BinaryOperator(operator),
                    line,
                    column,
                }) if operator.priority() >= priority => (operator, line, column),
                _ => break Some(result),
            };
            self.next_token();
            let Some(rhs) = self.parse_binary_operation(priority + 1) else {
                self.expect("an expression");
            };
            result = Expression {
                expression_type: ExpressionType::BinaryOperation(
                    Box::new(result),
                    operator,
                    Box::new(rhs),
                ),
                line,
                column,
            };
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_operation(0)
    }

    fn parse_conditional(&mut self) -> Option<Statement> {
        let Some(&Token {
            token_type: TokenType::Keyword(Keyword::If),
            line,
            column,
        }) = self.token()
        else {
            return None;
        };
        self.next_token();
        let Some(expr) = self.parse_expression() else {
            self.expect("an expression");
        };
        let Some(Token {
            token_type: TokenType::Keyword(Keyword::Then),
            ..
        }) = self.token()
        else {
            self.expect("a 'then' token");
        };
        self.next_token();
        let Some(then_stmt) = self.parse_statement() else {
            self.expect("a statement for the 'then' branch");
        };
        Some(Statement {
            statement_type: 'statement_type: {
                let Some(Token {
                    token_type: TokenType::Keyword(Keyword::Else),
                    ..
                }) = self.token()
                else {
                    break 'statement_type StatementType::Conditional(
                        expr,
                        Box::new(then_stmt),
                        None,
                    );
                };
                self.next_token();
                let Some(else_stmt) = self.parse_statement() else {
                    self.expect("a statement for the 'else' branch");
                };
                StatementType::Conditional(expr, Box::new(then_stmt), Some(Box::new(else_stmt)))
            },
            line,
            column,
        })
    }

    fn parse_loop(&mut self) -> Option<Statement> {
        let Some(&Token {
            token_type: TokenType::Keyword(Keyword::Loop),
            line,
            column,
        }) = self.token()
        else {
            return None;
        };
        self.next_token();
        let Some(stmt) = self.parse_statement() else {
            self.expect("a statement");
        };
        Some(Statement {
            statement_type: StatementType::Loop(Box::new(stmt)),
            line,
            column,
        })
    }

    fn parse_break(&mut self) -> Option<Statement> {
        let Some(&Token {
            token_type: TokenType::Keyword(Keyword::Break),
            line,
            column,
        }) = self.token()
        else {
            return None;
        };
        self.next_token();
        Some(Statement {
            statement_type: StatementType::Break,
            line,
            column,
        })
    }

    fn parse_type(&mut self, end_index: usize) -> Type {
        let mut stack = Vec::new();
        while self.index < end_index {
            let Some(&Token {
                token_type: TokenType::Identifier(ref name),
                line,
                column,
            }) = self.token()
            else {
                break;
            };
            let params = self
                .get_type_params(name)
                .unwrap_or_else(|| self.expect("a type"));
            if stack.len() < params {
                self.unexpected();
            }
            let ty = Type(
                stack.drain(stack.len() - params..).collect(),
                Identifier {
                    name: name.clone(),
                    line,
                    column,
                },
            );
            stack.push(ty);
            self.next_token();
        }
        if stack.len() != 1 {
            self.expect("a type");
        }
        stack.pop().unwrap()
    }

    fn parse_definition(&mut self) -> Option<Statement> {
        let statement_start_index = self.index;
        let &Token {
            ref token_type,
            line,
            column,
        } = self.token()?;
        let (value, params) = if let TokenType::Keyword(Keyword::Value) = token_type {
            self.next_token();
            (
                Some(
                    self.parse_expression()
                        .unwrap_or_else(|| self.expect("an expression")),
                ),
                None,
            )
        } else if let TokenType::Keyword(Keyword::Parameters) = token_type {
            self.next_token();
            let mut params = Vec::new();
            while let Some(&Token {
                token_type: TokenType::Identifier(ref name),
                line,
                column,
            }) = self.token()
            {
                params.push(Identifier {
                    name: name.clone(),
                    line,
                    column,
                });
                self.next_token();
                let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = self.token()
                else {
                    break;
                };
                self.next_token();
            }
            (None, Some(params))
        } else {
            (None, None)
        };
        let Some(Token {
            token_type: TokenType::Keyword(Keyword::New),
            ..
        }) = self.token()
        else {
            if value.is_none() && params.is_none() {
                return None;
            }
            self.expect("a 'new' keyword");
        };
        self.next_token();
        let start_index = self.index;
        let mut for_index = None;
        loop {
            if let Some(Token {
                token_type:
                    TokenType::Keyword(
                        Keyword::Constant
                        | Keyword::Variable
                        | Keyword::Procedure
                        | Keyword::Function
                        | Keyword::Type,
                    ),
                ..
            }) = self.token()
            {
                break;
            }
            if let Some(Token {
                token_type: TokenType::Keyword(Keyword::For),
                ..
            }) = self.token()
            {
                if for_index.is_some() {
                    self.unexpected();
                }
                for_index = Some(self.index);
            }
            self.next_token();
        }
        let Some(&Token {
            token_type:
                TokenType::Keyword(
                    keyword @ (Keyword::Constant
                    | Keyword::Variable
                    | Keyword::Procedure
                    | Keyword::Function
                    | Keyword::Type),
                ),
            ..
        }) = self.token()
        else {
            self.expect("the kind of definition");
        };
        let end_index = self.index;
        self.index = start_index;
        match keyword {
            Keyword::Constant | Keyword::Variable => {
                if keyword == Keyword::Constant && value.is_none() {
                    self.index = statement_start_index;
                    self.expect("a value for the constant");
                }
                if params.is_some() {
                    self.index = statement_start_index;
                    self.unexpected();
                }
                let ty = self.parse_type(end_index - 1);
                let Some(&Token {
                    token_type: TokenType::Identifier(ref name),
                    line: name_line,
                    column: name_column,
                }) = self.token()
                else {
                    self.expect("and identifier");
                };
                let name = name.clone();
                self.next_token();
                if self.index != end_index {
                    self.expect("a 'constant' or 'variable' keyword");
                }
                self.next_token();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .names
                    .insert(name.clone(), NameType::Noun);
                Some(Statement {
                    statement_type: if keyword == Keyword::Constant {
                        StatementType::Constant(
                            value.unwrap(),
                            ty,
                            Identifier {
                                name,
                                line: name_line,
                                column: name_column,
                            },
                        )
                    } else {
                        StatementType::Variable(
                            value,
                            ty,
                            Identifier {
                                name,
                                line: name_line,
                                column: name_column,
                            },
                        )
                    },
                    line,
                    column,
                })
            }
            Keyword::Procedure | Keyword::Function => {
                if value.is_some() {
                    self.index = statement_start_index;
                    self.unexpected();
                }
                self.scopes.push(Scope {
                    names: HashMap::new(),
                    types: if let Some(params) = params.as_ref() {
                        let mut types = HashMap::new();
                        for param in params {
                            types.insert(param.name.clone(), 0);
                        }
                        types
                    } else {
                        HashMap::new()
                    },
                });
                let args = if let Some(for_index) = for_index {
                    let mut args = Vec::new();
                    loop {
                        let external = if let Some(Token {
                            token_type: TokenType::Keyword(Keyword::External),
                            ..
                        }) = self.token()
                        {
                            self.next_token();
                            true
                        } else {
                            false
                        };
                        let ty = self.parse_type(
                            self.tokens[self.index..for_index]
                                .iter()
                                .enumerate()
                                .find_map(|(index, token)| {
                                    if token.token_type == TokenType::Comma {
                                        Some(self.index + index)
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or(for_index)
                                - 1,
                        );
                        let Some(&Token {
                            token_type: TokenType::Identifier(ref name),
                            line,
                            column,
                        }) = self.token()
                        else {
                            self.expect("an identifier");
                        };
                        args.push(Argument {
                            external,
                            ty,
                            ident: Identifier {
                                name: name.clone(),
                                line,
                                column,
                            },
                        });
                        self.next_token();
                        if let Some(Token {
                            token_type: TokenType::Keyword(Keyword::For),
                            ..
                        }) = self.token()
                        {
                            self.next_token();
                            break args;
                        }
                        let Some(Token {
                            token_type: TokenType::Comma,
                            ..
                        }) = self.token()
                        else {
                            self.expect("a comma");
                        };
                        self.next_token();
                    }
                } else {
                    Vec::new()
                };
                let external = if let (
                    None,
                    Some(Token {
                        token_type: TokenType::Keyword(Keyword::External),
                        ..
                    }),
                ) = (&params, self.token())
                {
                    self.next_token();
                    true
                } else {
                    false
                };
                let ty = if keyword == Keyword::Function {
                    Some(self.parse_type(end_index - 1))
                } else {
                    None
                };
                let Some(&Token {
                    token_type: TokenType::Identifier(ref name),
                    line: name_line,
                    column: name_column,
                }) = self.token()
                else {
                    self.expect("and identifier");
                };
                let name = name.clone();
                self.next_token();
                if self.index != end_index {
                    self.expect("a 'procedure' or 'function' keyword");
                }
                self.next_token();
                let scopes_len = self.scopes.len();
                self.scopes[scopes_len - 2].names.insert(
                    name.clone(),
                    if keyword == Keyword::Procedure {
                        NameType::Procedure
                    } else {
                        NameType::Function(args.len())
                    },
                );
                for arg in &args {
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .names
                        .insert(arg.ident.name.clone(), NameType::Noun);
                }
                let statement = if let Some(Token {
                    token_type: TokenType::Keyword(Keyword::Prototype),
                    ..
                }) = self.token()
                {
                    None
                } else {
                    Some(
                        self.parse_statement()
                            .unwrap_or_else(|| self.expect("a statement")),
                    )
                };
                self.scopes.pop().unwrap();
                Some(Statement {
                    statement_type: StatementType::FunctionLike {
                        params: if external {
                            None
                        } else {
                            Some(params.unwrap_or_else(Vec::new))
                        },
                        args,
                        ty,
                        ident: Identifier {
                            name,
                            line: name_line,
                            column: name_column,
                        },
                        statement: statement.map(Box::new),
                    },
                    line,
                    column,
                })
            }
            Keyword::Type => {
                if value.is_some() {
                    self.index = statement_start_index;
                    self.unexpected();
                }
                self.scopes.push(Scope {
                    names: HashMap::new(),
                    types: if let Some(params) = params.as_ref() {
                        let mut types = HashMap::new();
                        for param in params {
                            types.insert(param.name.clone(), 0);
                        }
                        types
                    } else {
                        HashMap::new()
                    },
                });
                let Some(for_index) = for_index else {
                    self.index = end_index;
                    self.expect("a 'for' keyword");
                };
                let mut fields = Vec::new();
                loop {
                    let ty = self.parse_type(
                        self.tokens[self.index..for_index]
                            .iter()
                            .enumerate()
                            .find_map(|(index, token)| {
                                if token.token_type == TokenType::Comma {
                                    Some(self.index + index)
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(for_index)
                            - 1,
                    );
                    let Some(&Token {
                        token_type: TokenType::Identifier(ref name),
                        line,
                        column,
                    }) = self.token()
                    else {
                        self.expect("an identifier");
                    };
                    fields.push((
                        ty,
                        Identifier {
                            name: name.clone(),
                            line,
                            column,
                        },
                    ));
                    self.next_token();
                    if let Some(Token {
                        token_type: TokenType::Keyword(Keyword::For),
                        ..
                    }) = self.token()
                    {
                        self.next_token();
                        break;
                    }
                    let Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) = self.token()
                    else {
                        self.expect("a comma");
                    };
                    self.next_token();
                }
                let Some(&Token {
                    token_type: TokenType::Identifier(ref name),
                    line: name_line,
                    column: name_column,
                }) = self.token()
                else {
                    self.expect("and identifier");
                };
                let name = name.clone();
                self.next_token();
                if self.index != end_index {
                    self.expect("a 'type' keyword");
                }
                self.next_token();
                self.scopes.pop().unwrap();
                Some(Statement {
                    statement_type: StatementType::Type {
                        params: params.unwrap_or_else(Vec::new),
                        fields,
                        ident: Identifier {
                            name,
                            line: name_line,
                            column: name_column,
                        },
                    },
                    line,
                    column,
                })
            }
            _ => unreachable!("Keyword is guaranteed to be one of those"),
        }
    }

    fn parse_block(&mut self) -> Option<Statement> {
        let Some(&Token {
            token_type: TokenType::Keyword(Keyword::Begin),
            line,
            column,
        }) = self.token()
        else {
            return None;
        };
        self.next_token();
        let stmts = self.parse_statements();
        let Some(Token {
            token_type: TokenType::Keyword(Keyword::End),
            ..
        }) = self.token()
        else {
            self.expect("an 'end' token");
        };
        self.next_token();
        Some(Statement {
            statement_type: StatementType::Block(stmts),
            line,
            column,
        })
    }

    fn parse_procedure_call(&mut self) -> Option<Statement> {
        let mut args = Vec::new();
        while let Some(expr) = self.parse_expression() {
            args.push(expr);
        }
        let Some(&Token {
            ref token_type,
            line,
            column,
        }) = self.token()
        else {
            if args.is_empty() {
                return None;
            }
            self.unexpected();
        };
        let statement_type = if *token_type == TokenType::Keyword(Keyword::Return) {
            if args.len() > 1 {
                self.unexpected();
            }
            StatementType::Return(args.pop())
        } else if let TokenType::Identifier(ident) = token_type {
            if self.get_name_type(ident) != Some(NameType::Procedure) {
                self.unexpected();
            }
            StatementType::Call(
                args,
                Identifier {
                    name: ident.clone(),
                    line,
                    column,
                },
            )
        } else if args.is_empty() {
            return None;
        } else {
            self.unexpected();
        };
        self.next_token();
        Some(Statement {
            statement_type,
            line,
            column,
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_conditional()
            .or_else(|| self.parse_loop())
            .or_else(|| self.parse_break())
            .or_else(|| self.parse_definition())
            .or_else(|| self.parse_block())
            .or_else(|| self.parse_procedure_call())
    }

    fn parse_statements(&mut self) -> Vec<Statement> {
        let mut stmts = Vec::new();
        while let Some(stmt) = self.parse_statement() {
            stmts.push(stmt);
        }
        stmts
    }

    pub fn parse(&mut self) -> Statement {
        let stmts = self.parse_statements();
        if self.token().is_some() {
            self.unexpected();
        }
        Statement {
            statement_type: StatementType::Block(stmts),
            line: 0,
            column: 0,
        }
    }
}
