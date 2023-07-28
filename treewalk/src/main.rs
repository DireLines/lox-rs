// use bpaf::*;
use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};

type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    // let args = positional::<PathBuf>("args")
    //     .optional()
    //     .to_options()
    //     .descr()
    //     .run();
    let args: Vec<String> = args().collect();
    if (args.len() > 2) {
        println!("Usage: lox [script]");
        return Ok(());
    } else if (args.len() == 2) {
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
        let Some(line) = stdin.next() else { return Ok(())};
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
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

struct Token<'a> {
    token: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

enum TokenType<'a> {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER(&'a str),
    STRING(&'a str),
    NUMBER(f64),

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}
fn run(source: &str) {
    //make a Scanner
    let mut scanner = Scanner::new(source);
    //get tokens from scanner
    for token in scanner {}
    //print tokens
    println!("running on {source}");
}
