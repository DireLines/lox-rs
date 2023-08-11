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
        let unparsed = &self.source[self.next_unparsed..];
        let c: char = unparsed.chars().next()?;
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
        let t = match c {
            '!' => {
                if equals_next {
                    Some(TokenType::BANG_EQUAL)
                } else {
                    Some(TokenType::BANG)
                }
            }
            '=' => {
                if equals_next {
                    Some(TokenType::EQUAL_EQUAL)
                } else {
                    Some(TokenType::EQUAL)
                }
            }
            '<' => {
                if equals_next {
                    Some(TokenType::LESS_EQUAL)
                } else {
                    Some(TokenType::LESS)
                }
            }
            '>' => {
                if equals_next {
                    Some(TokenType::GREATER_EQUAL)
                } else {
                    Some(TokenType::GREATER)
                }
            }
            _ => None,
        };
        if let Some(token) = t {
            let l = 2;
            self.next_unparsed += l;
            return Some(Token {
                token,
                lexeme: &unparsed[..l],
                line: self.line,
            });
        }
        None
    }
}

#[derive(Debug)]
struct Token<'a> {
    token: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

#[derive(Debug)]
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
    for token in scanner {}
    //print tokens
    println!("running on {source}");
}
