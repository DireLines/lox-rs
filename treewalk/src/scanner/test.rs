use super::*;

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
