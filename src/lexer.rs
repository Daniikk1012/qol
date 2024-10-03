use std::iter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    // Structural data
    Index,
    // Logic
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    // Literal
    True,
    False,
    // Conditional
    If,
    Then,
    Else,
    // Loop
    Loop,
    Break,
    // Return
    Return,
    // Block
    Begin,
    End,
    // Name definition
    Parameters,
    Value,
    New,
    External,
    For,
    Constant,
    Variable,
    Procedure,
    Function,
    Type,
    Prototype,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Field,
    Ternary,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    BinaryOperator(BinaryOperator),
    Character(char),
    String(String),
    Natural(u64),
    Real(f64),
    Keyword(Keyword),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

enum LexerState {
    None,
    Comment,
    Character {
        line: usize,
        column: usize,
    },
    String {
        string: String,
        line: usize,
        column: usize,
    },
    StringClosing {
        string: String,
        line: usize,
        column: usize,
    },
    Number {
        number: String,
        line: usize,
        column: usize,
    },
    IdentifierOrKeyword {
        ident_or_keyword: String,
        line: usize,
        column: usize,
    },
}

pub fn tokenize(code: &str) -> Vec<Token> {
    let mut result = Vec::new();
    let mut state = LexerState::None;
    let mut line = 0;
    let mut column = 0;
    for ch in code.chars().chain(iter::once('\n')) {
        // Inner loop for backtracking, iterates at most 2 times
        loop {
            state = match (ch, state) {
                // Comments and whitespace
                (_, LexerState::None) if ch.is_whitespace() => LexerState::None,
                ('\n', LexerState::Comment) => LexerState::None,
                ('!', LexerState::None) | (_, LexerState::Comment) => LexerState::Comment,
                // Special characters
                (':' | '?' | '(' | ')' | ',' | '+' | '-' | '*' | '/' | '№', LexerState::None) => {
                    result.push(Token {
                        token_type: match ch {
                            ':' => TokenType::Field,
                            '?' => TokenType::Ternary,
                            '(' => TokenType::OpeningParenthesis,
                            ')' => TokenType::ClosingParenthesis,
                            ',' => TokenType::Comma,
                            _ => TokenType::BinaryOperator(match ch {
                                '+' => BinaryOperator::Add,
                                '-' => BinaryOperator::Subtract,
                                '*' => BinaryOperator::Multiply,
                                '/' => BinaryOperator::Divide,
                                '№' => BinaryOperator::Index,
                                _ => unreachable!("Handled in the outermost match"),
                            }),
                        },
                        line,
                        column,
                    });
                    LexerState::None
                }
                // Character literal
                ('\\', LexerState::None) => LexerState::Character { line, column },
                (_, LexerState::Character { line, column }) => {
                    result.push(Token {
                        token_type: TokenType::Character(ch),
                        line,
                        column,
                    });
                    LexerState::None
                }
                // String literal
                ('"', LexerState::None) => LexerState::String {
                    string: String::new(),
                    line,
                    column,
                },
                (
                    '"',
                    LexerState::String {
                        string,
                        line,
                        column,
                    },
                ) => LexerState::StringClosing {
                    string,
                    line,
                    column,
                },
                (
                    _,
                    LexerState::String {
                        mut string,
                        line,
                        column,
                    },
                )
                | (
                    '"',
                    LexerState::StringClosing {
                        mut string,
                        line,
                        column,
                    },
                ) => {
                    string.push(ch);
                    LexerState::String {
                        string,
                        line,
                        column,
                    }
                }
                (
                    _,
                    LexerState::StringClosing {
                        string,
                        line,
                        column,
                    },
                ) => {
                    result.push(Token {
                        token_type: TokenType::String(string),
                        line,
                        column,
                    });
                    state = LexerState::None;
                    continue;
                }
                // Number literal
                (_, LexerState::None) if ch.is_ascii_digit() || ch == '.' => LexerState::Number {
                    number: ch.to_string(),
                    line,
                    column,
                },
                (
                    _,
                    LexerState::Number {
                        mut number,
                        line,
                        column,
                    },
                ) if ch.is_ascii_digit() || ch == '.' && !number.contains('.') => {
                    number.push(ch);
                    LexerState::Number {
                        number,
                        line,
                        column,
                    }
                }
                (
                    _,
                    LexerState::Number {
                        number,
                        line,
                        column,
                    },
                ) => {
                    result.push(Token {
                        token_type: if let Ok(number) = number.parse() {
                            TokenType::Natural(number)
                        } else if let Ok(number) = number.parse() {
                            TokenType::Real(number)
                        } else {
                            panic!("invalid number {number} at {}:{}", line + 1, column + 1);
                        },
                        line,
                        column,
                    });
                    state = LexerState::None;
                    continue;
                }
                // Identifier/keyword
                (_, LexerState::None) => LexerState::IdentifierOrKeyword {
                    ident_or_keyword: ch.to_string(),
                    line,
                    column,
                },
                (
                    _,
                    LexerState::IdentifierOrKeyword {
                        ident_or_keyword,
                        line,
                        column,
                    },
                ) if ch.is_whitespace() || "!:?(),+-*/№\\\".".contains(ch) => {
                    result.push(Token {
                        token_type: 'token_type: {
                            TokenType::Keyword(match ident_or_keyword.as_str() {
                                "және" => {
                                    break 'token_type TokenType::BinaryOperator(
                                        BinaryOperator::And,
                                    );
                                }
                                "немесе" => {
                                    break 'token_type TokenType::BinaryOperator(
                                        BinaryOperator::Or,
                                    );
                                }
                                "Ақиқат" => Keyword::True,
                                "Жалған" => Keyword::False,
                                "басы" => Keyword::Begin,
                                "аяғы" => Keyword::End,
                                "егер" => Keyword::If,
                                "онда" => Keyword::Then,
                                "болмаса" => Keyword::Else,
                                "қайтала" => Keyword::Loop,
                                "тоқта" => Keyword::Break,
                                "қайтар" => Keyword::Return,
                                "параметрлері" => Keyword::Parameters,
                                "мәні" => Keyword::Value,
                                "жаңа" => Keyword::New,
                                "сыртқы" => Keyword::External,
                                "үшін" => Keyword::For,
                                "айнымалысы" => Keyword::Variable,
                                "процедурасы" => Keyword::Procedure,
                                "функциясы" => Keyword::Function,
                                "түрі" => Keyword::Type,
                                "тұрақтысы" => Keyword::Constant,
                                "прототипі" => Keyword::Prototype,
                                _ => break 'token_type TokenType::Identifier(ident_or_keyword),
                            })
                        },
                        line,
                        column,
                    });
                    state = LexerState::None;
                    continue;
                }
                (
                    _,
                    LexerState::IdentifierOrKeyword {
                        mut ident_or_keyword,
                        line,
                        column,
                    },
                ) => {
                    ident_or_keyword.push(ch);
                    LexerState::IdentifierOrKeyword {
                        ident_or_keyword,
                        line,
                        column,
                    }
                }
            };
            break;
        }
        if ch == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    match state {
        LexerState::Character { .. } => panic!("expected '\\', got end of file"),
        LexerState::String { .. } => panic!("expected '\"', got end of file"),
        _ => {}
    }
    result
}
