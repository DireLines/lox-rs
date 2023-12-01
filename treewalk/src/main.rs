use phf::phf_map;
use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};

macro_rules! grammar_rule {
    /*
        (@munch $expr:expr  ----  $($rest:tt)*) => {
            let expr2 = {
                $expr;
                // asdfasdf
            }
            mixed_result!(@munch expr2 $($rest)*)

        };
    (trace $name:ident; $($tail:tt)*) => {
        {
            println!(concat!(stringify!($name), " = {:?}"), $name);
            mixed_rules!($($tail)*);
        }
    };
    (trace $name:ident = $init:expr; $($tail:tt)*) => {
        {
            let $name = $init;
            println!(concat!(stringify!($name), " = {:?}"), $name);
            mixed_rules!($($tail)*);
        }
    };
    ($name:ident → $($items:tt)*) => {
        fn $name () {}
        mixed_rules!(@munch {} $($items)*);
    }
   *() }*/

   ($functionname:ident -> $($tail:tt)*) => {
    fn $function_name<'a>(
        tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        grammar_rule!(@munch tokens $($tail)*)
    }
   }

   (@munch $tokens:ident $literal:lit $($tail:tt)*) => {
    let first_token = tokens.get(0)?;
    if first_token == TokenType::STRING($literal) {
        let mut tokens = &tokens[1..];
        grammar_rule!(@munch tokens, $($tail)*)
    } else {
        return Err();
    }
   }
}

grammar_rule!(classDecl -> "class");

//mixed_rules!(classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}");

macro_rules! make_parse_binary_group {
    ( $function_name:ident, $constituent_function_name:ident, $comparison_function:expr) => {
        fn $function_name<'a>(
            tokens: &'a [Token<'a>],
        ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
            let (mut expr, mut tokens) = Self::$constituent_function_name(tokens)?;

            while let Some(operator) = tokens.get(0).and_then(|t| $comparison_function(t.token)) {
                let rest = &tokens[1..];
                let (unary, tok) = Self::$constituent_function_name(rest)?;
                tokens = tok;
                expr = Self::BINARY {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(unary),
                };
            }

            Ok((expr, tokens))
        }
    };
}

macro_rules! try_match_patterns {($($tok:tt), *) => {
    |value| match value {
        $(TokenType::$tok => Some(BinaryOperator::$tok),)*
        _ => None
    }
}
}

#[derive(Debug, PartialEq, Clone)]
enum LoxSyntaxError<'a> {
    UnexpectedEof,
    UnexpectedToken {
        lexeme: &'a str,
        line: usize,
        message: &'static str,
    },
}
#[derive(Debug, PartialEq, Clone)]
enum Expression {
    NUMBER(f64),
    STRING(String),
    BOOL(bool),
    NIL,
    GROUPING {
        expression: Box<Expression>,
    },
    BINARY {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UNARY {
        operator: UnaryOperator,
        right: Box<Expression>,
    },
}

impl TryFrom<TokenType<'_>> for UnaryOperator {
    type Error = ();
    fn try_from(value: TokenType) -> std::result::Result<Self, Self::Error> {
        use crate::TokenType::*;
        match value {
            MINUS => Ok(Self::MINUS),
            BANG => Ok(Self::BANG),
            _ => Err(()),
        }
    }
}

//trait Parse {
//fn parse<'a>(tokens: &'a [Tokens<'a>]) ->
// std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> ;
//}

impl Expression {
    fn parse<'a>(
        tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        Self::parse_equality(tokens)
    }

    // pay no attention to the man behind the curtain!
    //fn parse_many<'a>(tokens: &'a, item: xxx, sep: yyyy)

    make_parse_binary_group!(
        parse_equality,
        parse_comparison,
        try_match_patterns!(EQUAL_EQUAL, BANG_EQUAL)
    );
    make_parse_binary_group!(
        parse_comparison,
        parse_term,
        try_match_patterns!(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)
    );
    make_parse_binary_group!(parse_term, parse_factor, try_match_patterns!(PLUS, MINUS));
    make_parse_binary_group!(parse_factor, parse_unary, try_match_patterns!(SLASH, STAR));

    fn parse_unary<'a>(
        tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        let t = tokens.get(0).ok_or(LoxSyntaxError::UnexpectedEof)?;
        if let Ok(operator) = UnaryOperator::try_from(t.token) {
            let rest = &tokens[1..];
            let (expr, tokens) = Self::parse_unary(rest)?;
            Ok((
                Self::UNARY {
                    operator,
                    right: Box::new(expr),
                },
                tokens,
            ))
        } else {
            Self::parse_primary(tokens)
        }
    }

    fn parse_primary<'a>(
        tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        let t = tokens.get(0).ok_or(LoxSyntaxError::UnexpectedEof)?;
        use crate::TokenType::*;
        match t.token {
            LEFT_PAREN => {
                let (expr, tokens) = Self::parse(&tokens[1..])?;
                let token_after = tokens.get(0).ok_or(LoxSyntaxError::UnexpectedEof)?;
                if token_after.token != RIGHT_PAREN {
                    return Err(LoxSyntaxError::UnexpectedToken {
                        lexeme: token_after.lexeme,
                        line: token_after.line,
                        message: "missing right paren",
                    });
                }
                return Ok((
                    Self::GROUPING {
                        expression: Box::new(expr),
                    },
                    &tokens[1..],
                ));
            }
            RIGHT_PAREN => {
                return Err(LoxSyntaxError::UnexpectedToken {
                    lexeme: t.lexeme,
                    line: t.line,
                    message: "right paren with no left paren",
                });
            }
            NUMBER(n) => Ok((Self::NUMBER(n), &tokens[1..])),
            STRING(s) => Ok((Self::STRING(String::from(s)), &tokens[1..])),
            TRUE => Ok((Self::BOOL(true), &tokens[1..])),
            FALSE => Ok((Self::BOOL(false), &tokens[1..])),
            NIL => Ok((Self::NIL, &tokens[1..])),
            _ => Err(LoxSyntaxError::UnexpectedToken {
                lexeme: t.lexeme,
                line: t.line,
                message: "Unexpected token",
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum UnaryOperator {
    MINUS,
    BANG,
}

#[derive(Debug, PartialEq, Clone)]
enum BinaryOperator {
    MINUS,
    PLUS,
    SLASH,
    STAR,
    EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
}

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

type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let args: Vec<String> = args().collect();
    if args.len() > 2 {
        println!("Usage: lox [script]");
        return Ok(());
    } else if args.len() == 2 {
        runFile(&args[1])?;
    } else {
        runPrompt();
    }
    Ok(())
}

fn runFile(path: &str) -> Result<()> {
    //get all bytes of file
    let s = std::fs::read_to_string(path)?;
    //run on the string
    run(&s);
    Ok(())
}

fn runPrompt() -> Result<()> {
    let mut stdin = std::io::stdin().lock().lines();
    loop {
        print!("> ");
        std::io::stdout().flush()?;
        let Some(line) = stdin.next() else {
            return Ok(());
        };
        let line = line?;
        if line == "exit" {
            break;
        }
        run(&line);
    }
    Ok(())
}

#[derive(Debug)]
struct Scanner<'a> {
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
                let unparsed_chars = &mut unparsed.chars();
                let mut found_dot = false;
                //integer part
                loop {
                    match unparsed_chars.next() {
                        Some(c) => {
                            if c.is_ascii_digit() {
                                num_chars += 1;
                            } else if c == '.' && !found_dot {
                                num_chars += 1;
                                found_dot = true;
                            } else {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                self.next_unparsed += num_chars;
                return Some(Token {
                    token: TokenType::NUMBER((&unparsed[0..num_chars]).parse().unwrap()),
                    lexeme: &unparsed[0..num_chars],
                    line: self.line,
                });
            }
            let is_alpha = |c: char| c.is_ascii_alphabetic() || c == '_';
            let is_alphanumeric = |c: char| c.is_ascii_alphanumeric() || c == '_';
            if is_alpha(c) {
                //ident or keyword
                let mut num_chars = 0;
                let unparsed_chars = &mut unparsed.chars();
                loop {
                    match unparsed_chars.next() {
                        Some(c) => {
                            if is_alphanumeric(c) {
                                num_chars += 1;
                            } else {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                self.next_unparsed += num_chars;
                let lexeme = &unparsed[0..num_chars];
                if let Some(&token_type) = KEYWORDS.get(lexeme) {
                    return Some(Token {
                        token: token_type,
                        lexeme,
                        line: self.line,
                    });
                } else {
                    return Some(Token {
                        token: TokenType::IDENTIFIER(lexeme),
                        lexeme,
                        line: self.line,
                    });
                }
            }
            panic!("Unexpected input: {:?}!", c);
        }
    }
}

#[derive(Debug, PartialEq)]
struct Token<'a> {
    token: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[rustfmt::skip]
enum TokenType<'a> {
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

fn run(source: &str) {
    //make a Scanner
    let mut scanner = Scanner::new(source);
    //get tokens from scanner
    for token in scanner {
        println!("{token:#?}");
    }
    //print tokens
    println!("running on {source}");
}

#[test]
fn test_simple_lexer() {
    let sample = "> << / //  this is a comment\n> / +";
    let mut scanner = Scanner::new(sample);
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
    let mut scanner = Scanner::new(sample);
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
    let mut scanner = Scanner::new(sample);
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
    let mut scanner = Scanner::new(sample);
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

#[test]
fn test_parse_expr() {
    let sample = "(3)";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse_unary(&tokens);
    assert_eq!(
        x.unwrap().0,
        Expression::GROUPING {
            expression: Box::new(Expression::NUMBER(3.0))
        }
    );
}

#[test]
fn test_parse_binary() {
    let sample = "3 * 4";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    assert_eq!(
        x.unwrap().0,
        Expression::BINARY {
            left: Box::new(Expression::NUMBER(3.0)),
            operator: BinaryOperator::STAR,
            right: Box::new(Expression::NUMBER(4.0))
        }
    );
}

#[test]
fn test_parse_binary_assoc() {
    let sample = "3 * 4 * 5";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    use crate::Expression::*;
    assert_eq!(
        x.unwrap().0,
        BINARY {
            left: Box::new(BINARY {
                left: Box::new(NUMBER(3.0)),
                operator: BinaryOperator::STAR,
                right: Box::new(NUMBER(4.0))
            }),
            operator: BinaryOperator::STAR,
            right: Box::new(NUMBER(5.0))
        }
    );
}

#[test]
fn test_syntax_error_unexpected() {
    let sample = "/";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    assert_eq!(
        x,
        Err(LoxSyntaxError::UnexpectedToken {
            lexeme: "/",
            line: 0,
            message: "Unexpected token"
        })
    );
}

#[test]
fn test_syntax_error_unexpected_paren() {
    let sample = ")";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    assert_eq!(
        x,
        Err(LoxSyntaxError::UnexpectedToken {
            lexeme: ")",
            line: 0,
            message: "right paren with no left paren"
        })
    );
}

#[test]
fn test_syntax_error_missing_paren() {
    let sample = "(1 2";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    assert_eq!(
        x,
        Err(LoxSyntaxError::UnexpectedToken {
            lexeme: "2",
            line: 0,
            message: "missing right paren"
        })
    );
}

#[test]
fn test_syntax_error_incomplete_binary() {
    let sample = "3 +";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::parse(&tokens);
    assert_eq!(x, Err(LoxSyntaxError::UnexpectedEof));
}
