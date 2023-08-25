use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};

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
                let l = c.len_utf8();
                self.next_unparsed += l;
                return Some(Token {
                    token,
                    lexeme: &unparsed[..l],
                    line: self.line,
                });
            }
            //two-character tokens
            let after_c = unparsed.chars().nth(1)?;
            let equals_next = after_c == '=';
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
                if after_c == '/' {
                    //eat comment
                    if let Some((comment, _rest)) = unparsed.split_once('\n') {
                        self.next_unparsed += comment.len();
                    } else {
                        //end of file with no newline
                        self.next_unparsed += unparsed.len();
                    }
                    continue;
                } else {
                    let l = c.len_utf8();
                    self.next_unparsed += l;
                    return Some(Token {
                        token: TokenType::SLASH,
                        lexeme: &unparsed[..l],
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

#[derive(Debug, PartialEq)]
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
