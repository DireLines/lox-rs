use phf::phf_map;
use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};

macro_rules! grammar_rule {
    ($functionname:ident -> $($tail:tt)*) => {
        fn $functionname<'a>(
            tokens: &'a [Token<'a>],
        ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
            let (body, tokens) = grammar_rule!(@munch tokens $($tail)*)?;
            Ok((body,tokens))
        }
       };
   ($ast_build_fn:path : $functionname:ident -> $($tail:tt)*) => {
    fn $functionname<'a>(
        tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        let (body, tokens) = grammar_rule!(@munch tokens $($tail)*)?;
        Ok(($ast_build_fn(body),tokens))
    }
   };

   (@munch $tokens:ident IDENTIFIER $($tail:tt)*) => {{
    match $tokens.get(0){
        Some(first_token) =>{
            let tokens = &$tokens[1..];
            match first_token.token {
                TokenType::IDENTIFIER(ident) => {
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((ident,parsed_tail_ast),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ =>  Err(LoxSyntaxError::UnexpectedToken{
                    lexeme: first_token.lexeme,
                    line: first_token.line,
                    message: "Unexpected token",
            })
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

   (@munch $tokens:ident NUMBER $($tail:tt)*) => {{
    match $tokens.get(0){
        Some(first_token) =>{
            let tokens = &$tokens[1..];
            match first_token.token {
                TokenType::NUMBER(value) => {
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((value,parsed_tail_ast),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ =>  Err(LoxSyntaxError::UnexpectedToken{
                    lexeme: first_token.lexeme,
                    line: first_token.line,
                    message: "Unexpected token",
            })
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

   (@munch $tokens:ident STRING $($tail:tt)*) => {{
    match $tokens.get(0){
        Some(first_token) =>{
            let tokens = &$tokens[1..];
            match first_token.token {
                TokenType::STRING(value) => {
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((value,parsed_tail_ast),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ =>  Err(LoxSyntaxError::UnexpectedToken{
                    lexeme: first_token.lexeme,
                    line: first_token.line,
                    message: "Unexpected token",
            })
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

    //REPEATED GROUP ( a b )*
    (@munch $tokens:ident ($($subtree:tt)+)* $($tail:tt)*) => {{
    let mut tokens = $tokens;
    let mut results = vec![];
    loop {
        let subtree = grammar_rule!(@munch tokens $($subtree)+);
        match subtree {
            Ok((parsed_subtree_ast,leftover_tokens))=>{
                //consume tokens for subtree
                results.push(parsed_subtree_ast);
                tokens = leftover_tokens;
            },
            Err(_)=>{
                break;
            }
        }
    }
    let(parsed_tail_ast,tokens)=grammar_rule!(@munch tokens $($tail)*)?;
    Ok(((results,parsed_tail_ast),tokens))
    }};

    //OR GROUP ( a | b | c )
    (@munch $tokens:ident ($($variants:tt)|*) $($tail:tt)*) => {{
        let x = 5;
        let tokens = $tokens;
        let subtree = grammar_rule!(@orgroup tokens $($variants)|*);
        match subtree {
            Ok((parsed_subtree_ast,leftover_tokens))=>{
                //consume tokens for subtree
                let(parsed_tail_ast,tokens)=grammar_rule!(@munch leftover_tokens $($tail)*)?;
                Ok(((Some(parsed_subtree_ast),parsed_tail_ast),tokens))
            },
            Err(e)=>{
                //TODO: did not match any of the acceptable variants
                Err(e)
            }
        }
        }};
    (@orgroup $tokens:ident $first_variant:tt | $($remaining_variants:tt)|*) => {{
        let tokens = $tokens;
        let subtree = grammar_rule!(@munch tokens $first_variant);
        match subtree {
            Ok(_)=>{
                subtree
            },
            Err(_)=>{
                grammar_rule!(@orgroup tokens $($remaining_variants)|*)
            }
        }
    }};
    (@orgroup $tokens:ident $first_variant:tt) => {{
        grammar_rule!(@munch $tokens $first_variant)
    }};


    //OPTIONAL GROUP ( a b )?
   (@munch $tokens:ident ($($subtree:tt)+)? $($tail:tt)*) => {{
    let tokens = $tokens;
    let subtree = grammar_rule!(@munch tokens $($subtree)+);
    match subtree {
    Ok((parsed_subtree_ast,leftover_tokens))=>{
        //consume tokens for subtree
        let(parsed_tail_ast,tokens)=grammar_rule!(@munch leftover_tokens $($tail)*)?;
        Ok(((Some(parsed_subtree_ast),parsed_tail_ast),tokens))
    },
    Err(_)=>{
        //parse without consuming tokens for subtree
        let(parsed_tail_ast,tokens)=grammar_rule!(@munch tokens $($tail)*)?;
        Ok(((None,parsed_tail_ast),tokens))
    }
   }
   }};

   //generic token but capture which token it was
   (@munch $tokens:ident {$tt:ident} $($tail:tt)*) => {{
    match $tokens.get(0) {
        Some(first_token)=>{
            if first_token.token == TokenType::$tt {
                let tokens = &$tokens[1..];
                let subtree = grammar_rule!(@munch tokens $($tail)*);
                match subtree {
                    Ok((parsed_tail_ast,tokens)) => {
                        Ok::<_, LoxSyntaxError>(((TokenType::$tt,parsed_tail_ast),tokens))
                    },
                    Err(e) =>Err(e)
                }
            } else {
                Err(LoxSyntaxError::UnexpectedToken{
                        lexeme: first_token.lexeme,
                        line: first_token.line,
                        message: "Unexpected token",
                })
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

   //generic token (do not record token type)
   (@munch $tokens:ident $tt:ident $($tail:tt)*) => {{
    match $tokens.get(0) {
        Some(first_token)=>{
            if first_token.token == TokenType::$tt {
                let tokens = &$tokens[1..];
                grammar_rule!(@munch tokens $($tail)*)
            } else {
                Err(LoxSyntaxError::UnexpectedToken{
                        lexeme: first_token.lexeme,
                        line: first_token.line,
                        message: "Unexpected token",
                })
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

    //RECURSIVE PARSING FUNCTION CALL
    (@munch $tokens:ident [$parse_fn:path] $($tail:tt)*) => {{
    let tokens = $tokens;
    match $parse_fn(tokens) {
    Ok((parsed_subtree_ast,leftover_tokens))=>{
        //consume tokens for subtree
        let(parsed_tail_ast,tokens)=grammar_rule!(@munch leftover_tokens $($tail)*)?;
        Ok(((Some(parsed_subtree_ast),parsed_tail_ast),tokens))
    },
    Err(e)=>{
        //pass along error from parsing subtree
        Err(e)
    }
    }
    }};

   (@munch $tokens:ident) => {{
    Ok(((),$tokens))
   }}
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
#[derive(Debug, PartialEq, Clone)]
enum Declaration {
    ClassDecl {
        name: String,
        parent_name: Option<String>,
        body: Vec<()>,
    },
    FunDecl(()),
    VarDecl {
        name: String,
        definition: Option<Expression>,
    },
    Statement,
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    ExprStmt,
    ForStmt,
    IfStmt,
    PrintStmt,
    ReturnStmt,
    WhileStmt,
    Block,
}
impl Statement {
    grammar_rule!(statement -> ([Statement::exprStmt] | [Statement::forStmt] | [Statement::ifStmt] | [Statement::printStmt] | [Statement::returnStmt] | [Statement::whileStmt] | [Statement::block]) );
    grammar_rule!(Self::build_exprStmt : exprStmt -> [Expression::expression] SEMICOLON );
    grammar_rule!(Self::build_forStmt : forStmt -> FOR RIGHT_PAREN ( [Declaration::varDecl] | [Statement::exprStmt] | SEMICOLON ) ( [Expression::expression] )? SEMICOLON ([Expression::expression])? LEFT_PAREN [Statement::statement] );
    grammar_rule!(Self::build_ifStmt : ifStmt -> IF RIGHT_PAREN [Expression::expression] LEFT_PAREN [Statement::statement] ( ELSE [Statement::statement] )? );
    grammar_rule!(Self::build_printStmt : printStmt -> PRINT [Expression::expression] SEMICOLON );
    grammar_rule!(Self::build_returnStmt : returnStmt -> RETURN ([Expression::expression])? SEMICOLON );
    grammar_rule!(Self::build_whileStmt : whileStmt -> WHILE RIGHT_PAREN [Expression::expression] LEFT_PAREN [Statement::statement] );
    grammar_rule!(Self::build_block : block -> LEFT_BRACE ([Declaration::declaration])* RIGHT_BRACE );
    fn build_exprStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_forStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_ifStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_printStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_returnStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_whileStmt(data: ()) -> Self {
        unimplemented!()
    }
    fn build_block(data: ()) -> Self {
        unimplemented!()
    }
}
#[derive(Debug, PartialEq, Clone)]
struct Function {
    name: String,
    parameters: Vec<String>,
    body: Box<Option<Statement>>,
}
impl Function {
    fn build_function(
        data: (
            &str,
            (
                Option<(&str, (Vec<(&str, ())>, ()))>,
                (Option<Statement>, ()),
            ),
        ),
    ) -> Self {
        let (name, (params, (stmt, ()))) = data;
        let params = if let Some((first_param, (other_params, ()))) = params {
            let mut params: Vec<String> = other_params.iter().map(|e| e.0.to_owned()).collect();
            params.insert(0, first_param.to_owned());
            params
        } else {
            Vec::new()
        };
        Function {
            name: name.to_string(),
            parameters: params,
            body: Box::new(stmt),
        }
    }
    grammar_rule!(Self::build_function : function -> IDENTIFIER LEFT_PAREN (IDENTIFIER (COMMA IDENTIFIER)* )? RIGHT_PAREN [Statement::block]);
}
impl Declaration {
    grammar_rule!(declaration ->([Declaration::classDecl] | [Declaration::funDecl] | [Declaration::varDecl] | [Statement::statement]) );
    grammar_rule!(Self::build_classDecl : classDecl -> CLASS IDENTIFIER ( LESS IDENTIFIER )? LEFT_BRACE ([Function::function])* RIGHT_BRACE);
    grammar_rule!(Self::build_funDecl: funDecl -> FUN [Function::function] );
    grammar_rule!(Self::build_varDecl: varDecl -> VAR IDENTIFIER ( EQUAL [Expression::expression] )? SEMICOLON );
    fn build_classDecl(data: (&str, (Option<(&str, ())>, ()))) -> Self {
        let (ident, (parent_name, _)) = data;
        Self::ClassDecl {
            name: ident.to_string(),
            parent_name: parent_name.map(|s| s.0.to_string()),
            body: vec![],
        }
    }
    fn build_funDecl(data: ()) -> Self {
        unimplemented!()
    }
    fn build_varDecl(data: ()) -> Self {
        unimplemented!()
    }
}

impl Expression {
    grammar_rule!(expression -> [Expression::assignment] );
    grammar_rule!(Self::build_assignment : assignment -> (( [Expression::call] DOT )? IDENTIFIER EQUAL [Expression::assignment] | [Expression::logic_or] ));
    grammar_rule!(Self::build_logic_or : logic_or -> [Expression::logic_and] ( OR [Expression::logic_and] )* );
    grammar_rule!(Self::build_logic_and : logic_and -> [Expression::equality] ( AND [Expression::equality] )* );
    grammar_rule!(Self::build_equality : equality -> [Expression::comparison] ( ( {BANG_EQUAL} | {EQUAL_EQUAL} ) [Expression::comparison] )* );
    grammar_rule!(Self::build_comparison : comparison -> [Expression::term] ( ( {GREATER} | {GREATER_EQUAL} | {LESS} | {LESS_EQUAL} ) [Expression::term] )* );
    grammar_rule!(Self::build_term : term -> [Expression::factor] ( ( {MINUS} | {PLUS} ) [Expression::factor] )* );
    grammar_rule!(Self::build_factor : factor -> [Expression::unary] ( ( {SLASH} | {STAR} ) [Expression::unary] )* );
    grammar_rule!(Self::build_unary : unary -> (( {BANG} | {MINUS} ) [Expression::unary] | [Expression::call] ));
    grammar_rule!(Self::build_call : call -> [Expression::primary] ( (LEFT_PAREN([Expression::expression] ( COMMA [Expression::expression] )* )? RIGHT_PAREN | DOT IDENTIFIER) )* );
    grammar_rule!(Self::build_primary : primary -> ({TRUE} | {FALSE} | {NIL} | {THIS} | NUMBER | STRING | IDENTIFIER | LEFT_PAREN [Expression::expression] RIGHT_PAREN | SUPER DOT IDENTIFIER ));
    fn build_assignment(data: ()) -> Self {
        unimplemented!()
    }
    fn build_logic_or(data: ()) -> Self {
        unimplemented!()
    }
    fn build_logic_and(data: ()) -> Self {
        unimplemented!()
    }
    fn build_equality(data: ()) -> Self {
        unimplemented!()
    }
    fn build_comparison(data: ()) -> Self {
        unimplemented!()
    }
    fn build_term(data: ()) -> Self {
        unimplemented!()
    }
    fn build_factor(data: ()) -> Self {
        unimplemented!()
    }
    fn build_unary(data: ()) -> Self {
        unimplemented!()
    }
    fn build_call(data: ()) -> Self {
        unimplemented!()
    }
    fn build_primary(data: ()) -> Self {
        unimplemented!()
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
    let x = Expression::unary(&tokens);
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
    let x = Expression::expression(&tokens);
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
    let x = Expression::expression(&tokens);
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
    let x = Expression::expression(&tokens);
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
    let x = Expression::expression(&tokens);
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
    let x = Expression::expression(&tokens);
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
    let x = Expression::expression(&tokens);
    assert_eq!(x, Err(LoxSyntaxError::UnexpectedEof));
}

#[test]
fn test_parse_class_decl_no_inherit() {
    let sample = "class Foo";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Declaration::classDecl(&tokens);
    assert_eq!(
        x,
        Ok((
            Declaration::ClassDecl {
                name: String::from("Foo"),
                parent_name: None,
                body: vec![]
            },
            &[][..]
        ))
    );
}

#[test]
fn test_parse_class_decl_inherit() {
    let sample = "class Foo < Bar";
    let mut scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Declaration::classDecl(&tokens);
    assert_eq!(
        x,
        Ok((
            Declaration::ClassDecl {
                name: String::from("Foo"),
                parent_name: Some(String::from("Bar")),
                body: vec![]
            },
            &[][..]
        ))
    );
}
