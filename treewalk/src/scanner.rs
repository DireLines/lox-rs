use phf::phf_map;
const KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" =>    TokenType::AND,
    "class" =>  TokenType::CLASS,
    "else" =>   TokenType::ELSE,
    "false" =>  TokenType::FALSE,
    "for" =>    TokenType::FOR,
    "fun" =>    TokenType::FUN,
    "if" =>     TokenType::IF,
    "nil" =>    TokenType::NIL,
    "or" =>     TokenType::OR,
    "print" =>  TokenType::PRINT,
    "return" => TokenType::RETURN,
    "super" =>  TokenType::SUPER,
    "this" =>   TokenType::THIS,
    "true" =>   TokenType::TRUE,
    "var" =>    TokenType::VAR,
    "while" =>  TokenType::WHILE,
};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    next_unparsed: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            next_unparsed: 0,
            line: 0,
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.source.len() == self.next_unparsed {
                return None;
            }
            let unparsed = &self.source[self.next_unparsed..];
            let c: char = unparsed.chars().next()?;
            //skip whitespace
            match c {
                '\n' => {
                    self.line += 1;
                    self.next_unparsed += 1;
                    continue;
                }
                ' ' | '\r' | '\t' => {
                    self.next_unparsed += 1;
                    continue;
                }
                _ => {}
            };
            //one character tokens
            let t = match c {
                '(' => Some(TokenType::LEFT_PAREN),
                ')' => Some(TokenType::RIGHT_PAREN),
                '{' => Some(TokenType::LEFT_BRACE),
                '}' => Some(TokenType::RIGHT_BRACE),
                ',' => Some(TokenType::COMMA),
                '.' => Some(TokenType::DOT),
                '-' => Some(TokenType::MINUS),
                '+' => Some(TokenType::PLUS),
                ';' => Some(TokenType::SEMICOLON),
                '*' => Some(TokenType::STAR),
                _ => None,
            };
            if let Some(token) = t {
                let token_length = c.len_utf8();
                self.next_unparsed += token_length;
                return Some(Token {
                    token,
                    lexeme: &unparsed[..token_length],
                    line: self.line,
                });
            }
            //two-character tokens
            let after_c = unparsed.chars().nth(1);
            let equals_next = after_c == Some('=');
            let token_advance = match c {
                '!' => {
                    if equals_next {
                        Some((TokenType::BANG_EQUAL, 2))
                    } else {
                        Some((TokenType::BANG, 1))
                    }
                }
                '=' => {
                    if equals_next {
                        Some((TokenType::EQUAL_EQUAL, 2))
                    } else {
                        Some((TokenType::EQUAL, 1))
                    }
                }
                '<' => {
                    if equals_next {
                        Some((TokenType::LESS_EQUAL, 2))
                    } else {
                        Some((TokenType::LESS, 1))
                    }
                }
                '>' => {
                    if equals_next {
                        Some((TokenType::GREATER_EQUAL, 2))
                    } else {
                        Some((TokenType::GREATER, 1))
                    }
                }
                _ => None,
            };
            if let Some((token, token_length)) = token_advance {
                self.next_unparsed += token_length;
                return Some(Token {
                    token,
                    lexeme: &unparsed[..token_length],
                    line: self.line,
                });
            }
            // / or comment
            if c == '/' {
                if after_c == Some('/') {
                    //eat comment
                    if let Some((comment, _rest)) = unparsed.split_once('\n') {
                        self.next_unparsed += comment.len();
                    } else {
                        //end of file with no newline
                        self.next_unparsed += unparsed.len();
                    }
                    continue;
                } else {
                    let token_length = c.len_utf8();
                    self.next_unparsed += token_length;
                    return Some(Token {
                        token: TokenType::SLASH,
                        lexeme: &unparsed[..token_length],
                        line: self.line,
                    });
                }
            }
            //string literal
            if c == '"' {
                if let Some((literal, _rest)) = unparsed[1..].split_once('"') {
                    self.line += literal.chars().filter(|c| *c == '\n').count();
                    self.next_unparsed += literal.len() + 2;
                    return Some(Token {
                        token: TokenType::STRING(literal),
                        lexeme: literal,
                        line: self.line,
                    });
                } else {
                    //unterminated string
                    panic!("Unterminated string at line {}", self.line);
                }
            }
            if c.is_ascii_digit() {
                //number literal
                let mut num_chars = 0;
                let mut found_dot = false;
                //integer part

                for c in unparsed.chars() {
                    if c.is_ascii_digit() {
                        num_chars += 1;
                    } else if c == '.' && !found_dot {
                        num_chars += 1;
                        found_dot = true;
                    } else {
                        break;
                    }
                }
                self.next_unparsed += num_chars;
                return Some(Token {
                    token: TokenType::NUMBER(unparsed[0..num_chars].parse().unwrap()),
                    lexeme: &unparsed[0..num_chars],
                    line: self.line,
                });
            }
            let is_alpha = |c: char| c.is_ascii_alphabetic() || c == '_';
            let is_alphanumeric = |c: char| c.is_ascii_alphanumeric() || c == '_';
            if is_alpha(c) {
                //ident or keyword
                let mut num_chars = 0;
                for c in unparsed.chars() {
                    if is_alphanumeric(c) {
                        num_chars += 1;
                    } else {
                        break;
                    }
                }
                self.next_unparsed += num_chars;
                let lexeme = &unparsed[0..num_chars];
                let token = KEYWORDS
                    .get(lexeme)
                    .copied()
                    .unwrap_or(TokenType::IDENTIFIER(lexeme));

                return Some(Token {
                    token,
                    lexeme,
                    line: self.line,
                });
            }
            panic!("Unexpected input: {:?}!", c);
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub token: TokenType<'a>,
    pub lexeme: &'a str,
    pub line: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[rustfmt::skip]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub enum TokenType<'a> {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,


    // Literals.
    IDENTIFIER(&'a str),
    STRING(&'a str),
    NUMBER(f64),

    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF
}

#[test]
fn test_simple_lexer() {
    let sample = "> << / //  this is a comment\n> / +";
    let scanner = Scanner::new(sample);
    let x = scanner.collect::<Vec<_>>();
    use TokenType::*;
    assert_eq!(
        x,
        vec![
            Token {
                token: GREATER,
                lexeme: ">",
                line: 0
            },
            Token {
                token: LESS,
                lexeme: "<",
                line: 0
            },
            Token {
                token: LESS,
                lexeme: "<",
                line: 0
            },
            Token {
                token: SLASH,
                lexeme: "/",
                line: 0
            },
            Token {
                token: GREATER,
                lexeme: ">",
                line: 1
            },
            Token {
                token: SLASH,
                lexeme: "/",
                line: 1
            },
            Token {
                token: PLUS,
                lexeme: "+",
                line: 1
            }
        ]
    );
}

#[test]
fn test_string_literal() {
    let sample = " > \"multiline\nstring\nliteral\" > ";
    let scanner = Scanner::new(sample);
    let x = scanner.collect::<Vec<_>>();
    use TokenType::*;
    assert_eq!(
        x,
        vec![
            Token {
                token: GREATER,
                lexeme: ">",
                line: 0
            },
            Token {
                token: STRING("multiline\nstring\nliteral"),
                lexeme: "multiline\nstring\nliteral",
                line: 2
            },
            Token {
                token: GREATER,
                lexeme: ">",
                line: 2
            }
        ]
    );
}

#[test]
fn test_number_literal() {
    let sample = " = 5 35.3 0.32.33 -4 = ";
    let scanner = Scanner::new(sample);
    let x = scanner.collect::<Vec<_>>();
    use TokenType::*;
    assert_eq!(
        x,
        vec![
            Token {
                token: EQUAL,
                lexeme: "=",
                line: 0
            },
            Token {
                token: NUMBER(5.0),
                lexeme: "5",
                line: 0
            },
            Token {
                token: NUMBER(35.3),
                lexeme: "35.3",
                line: 0
            },
            Token {
                token: NUMBER(0.32),
                lexeme: "0.32",
                line: 0
            },
            Token {
                token: DOT,
                lexeme: ".",
                line: 0
            },
            Token {
                token: NUMBER(33.0),
                lexeme: "33",
                line: 0
            },
            Token {
                token: MINUS,
                lexeme: "-",
                line: 0
            },
            Token {
                token: NUMBER(4.0),
                lexeme: "4",
                line: 0
            },
            Token {
                token: EQUAL,
                lexeme: "=",
                line: 0
            }
        ]
    );
}

#[test]
fn test_keyword_ident() {
    let sample = "var x = 5;\n var y = -x;";
    let scanner = Scanner::new(sample);
    let x = scanner.collect::<Vec<_>>();
    use TokenType::*;
    assert_eq!(
        x,
        vec![
            Token {
                token: VAR,
                lexeme: "var",
                line: 0
            },
            Token {
                token: IDENTIFIER("x"),
                lexeme: "x",
                line: 0
            },
            Token {
                token: EQUAL,
                lexeme: "=",
                line: 0
            },
            Token {
                token: NUMBER(5.0),
                lexeme: "5",
                line: 0
            },
            Token {
                token: SEMICOLON,
                lexeme: ";",
                line: 0
            },
            Token {
                token: VAR,
                lexeme: "var",
                line: 1
            },
            Token {
                token: IDENTIFIER("y"),
                lexeme: "y",
                line: 1
            },
            Token {
                token: EQUAL,
                lexeme: "=",
                line: 1
            },
            Token {
                token: MINUS,
                lexeme: "-",
                line: 1
            },
            Token {
                token: IDENTIFIER("x"),
                lexeme: "x",
                line: 1
            },
            Token {
                token: SEMICOLON,
                lexeme: ";",
                line: 1
            }
        ]
    );
}
