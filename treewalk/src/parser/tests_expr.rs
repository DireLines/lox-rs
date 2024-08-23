use super::*;
use crate::parser::Expression;

// expression     → assignment ;
//
// assignment     → ( call "." )? IDENTIFIER "=" assignment
//                | logic_or ;
//
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
//
// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// primary        → "true" | "false" | "nil" | "this"
//                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
//                | "super" "." IDENTIFIER ;

/// assignment     → ( call "." )? IDENTIFIER "=" assignment
///                | logic_or ;
mod assignment {
    use super::super::*;

    #[test]
    fn simple_assignment() {
        assert_eq!(
            Expression::MemberAssign {
                path: vec![],
                field: "time".to_string(),
                value: Box::new(Expression::Identifier("money".into()))
            },
            parse_str_with("time = money", Expression::new)
        );
    }

    #[test]
    #[should_panic]
    fn this_should_fail() {
        parse_str_with("2 = 3", Expression::assignment);
    }
}

/// logic_or       → logic_and ( "or" logic_and )* ;
mod logic_or {
    use super::super::*;
    #[test]
    fn simple_logic() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Binary {
                    left: Box::new(Expression::Identifier("coffee".into())),
                    operator: BinaryOperator::Or,
                    right: Box::new(Expression::Identifier("tea".into())),
                }),
                operator: BinaryOperator::Or,
                right: Box::new(Expression::Identifier("me".into())),
            },
            parse_str_with("coffee or tea or me", Expression::new)
        )
    }
}

/// logic_and      → equality ( "and" equality )* ;
mod logic_and {
    use super::super::*;
    #[test]
    fn simple_logic() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Identifier("Egg".into())),
                operator: BinaryOperator::And,
                right: Box::new(Expression::Identifier("spam".into())),
            },
            parse_str_with("Egg and spam", Expression::new)
        )
    }
}

/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
mod equality {
    use super::super::*;
    #[test]
    fn simple_equality() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Nil),
                operator: BinaryOperator::NotEqual,
                right: Box::new(Expression::Nil)
            },
            parse_str_with("nil !=nil", Expression::new)
        )
    }
}

/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
mod comparison {
    use super::super::*;
    #[test]
    fn simple_comparison() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Nil),
                operator: BinaryOperator::GreaterEqual,
                right: Box::new(Expression::Nil)
            },
            parse_str_with("nil >= nil", Expression::new)
        )
    }
}

/// term           → factor ( ( "-" | "+" ) factor )* ;
mod term {
    use super::super::*;
    #[test]
    fn simple_term() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Number(1.0)),
                operator: BinaryOperator::Plus,
                right: Box::new(Expression::Binary {
                    left: Box::new(Expression::Number(3.0)),
                    operator: BinaryOperator::Mul,
                    right: Box::new(Expression::Nil)
                })
            },
            parse_str_with("1 + 3 * nil", Expression::new)
        )
    }
}

/// factor         → unary ( ( "/" | "*" ) unary )* ;
mod factor {
    use super::super::*;
    #[test]
    fn simple_factor() {
        assert_eq!(
            Expression::Binary {
                left: Box::new(Expression::Binary {
                    left: Box::new(Expression::Number(1.0)),
                    operator: BinaryOperator::Div,
                    right: Box::new(Expression::Number(3.0))
                }),
                operator: BinaryOperator::Mul,
                right: Box::new(Expression::Nil)
            },
            parse_str_with("1 / 3 * nil", Expression::new)
        )
    }
}

/// unary          → ( "!" | "-" ) unary | call ;
mod unary {
    use super::super::*;
    #[test]
    fn negated_string_literal() {
        assert_eq!(
            Expression::Unary {
                operator: UnaryOperator::Neg,
                right: Box::new(Expression::Unary {
                    operator: UnaryOperator::Not,
                    right: Box::new(Expression::Unary {
                        operator: UnaryOperator::Neg,
                        right: Box::new(Expression::Unary {
                            operator: UnaryOperator::Neg,
                            right: Box::new(Expression::Bool(false))
                        })
                    })
                })
            },
            parse_str_with("- ! - - false", Expression::new)
        )
    }
}

/// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
mod call {
    use super::super::*;

    #[test]
    fn call_fn() {
        assert_eq!(
            Expression::Call {
                base: Box::new(Expression::Number(3.0)),
                path: vec![MemberAccess::Args {
                    args: vec![Expression::Number(4.0)]
                }]
            },
            parse_str_with("3(4)", Expression::new)
        );
    }

    #[test]
    fn member_access() {
        assert_eq!(
            Expression::Call {
                base: Box::new(Expression::Identifier("alpha".into())),
                path: vec![MemberAccess::Field("alice".into())]
            },
            parse_str_with("alpha.alice", Expression::call)
        );
    }

    #[test]
    fn mix() {
        assert_eq!(
            Expression::Call {
                base: Box::new(Expression::Identifier("alpha".into())),
                path: vec![
                    MemberAccess::Args { args: vec![] },
                    MemberAccess::Args {
                        args: vec![Expression::Number(3.0)]
                    },
                    MemberAccess::Field("omega".into())
                ]
            },
            parse_str_with("alpha()(3).omega", Expression::new)
        );
    }
}

/// Cover tests of "primary" lox epressions
/// primary        → "true" | "false" | "nil" | "this"
///                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
///                | "super" "." IDENTIFIER ;
mod primary {
    use super::super::*;

    #[test]
    fn bool() {
        assert_eq!(
            Expression::Bool(false),
            parse_str_with("false", Expression::new)
        );

        assert_eq!(
            Expression::Bool(true),
            parse_str_with("true", Expression::new)
        );
    }

    #[test]
    fn string_lit() {
        assert_eq!(
            Expression::String("hello".into()),
            parse_str_with("\"hello\"", Expression::new)
        );
    }

    #[test]
    fn identifier() {
        assert_eq!(
            Expression::Identifier("atticus".into()),
            parse_str_with("atticus", Expression::new)
        );
    }

    #[test]
    fn nil() {
        assert_eq!(Expression::Nil, parse_str_with("nil", Expression::new));
    }

    #[test]
    fn this() {
        assert_eq!(Expression::This, parse_str_with("this", Expression::new));
    }

    #[test]
    fn parens() {
        assert_eq!(Expression::This, parse_str_with("(this)", Expression::new));
    }

    #[test]
    fn super_access() {
        assert_eq!(
            Expression::SuperFieldAccess {
                field: "dot".into()
            },
            parse_str_with("super.dot", Expression::new)
        );
    }
}

#[test]
fn test_parse_expr() {
    let sample = "(3)";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::unary(&tokens);
    assert_eq!(x.unwrap().0, Expression::Number(3.0),);
}

#[test]
fn test_parse_binary() {
    let sample = "3 * 4";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    assert_eq!(
        x.unwrap().0,
        Expression::Binary {
            left: Box::new(Expression::Number(3.0)),
            operator: BinaryOperator::Mul,
            right: Box::new(Expression::Number(4.0))
        }
    );
}

#[test]
fn test_parse_binary_assoc() {
    let sample = "3 * 4 * 5";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    use Expression::*;
    assert_eq!(
        x.unwrap().0,
        Binary {
            left: Box::new(Binary {
                left: Box::new(Number(3.0)),
                operator: BinaryOperator::Mul,
                right: Box::new(Number(4.0))
            }),
            operator: BinaryOperator::Mul,
            right: Box::new(Number(5.0))
        }
    );
}

#[test]
fn test_syntax_error_unexpected() {
    let sample = "/";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
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
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    assert_eq!(
        x,
        Err(LoxSyntaxError::UnexpectedToken {
            lexeme: ")",
            line: 0,
            message: "Unexpected token"
        })
    );
}

#[test]
fn test_parse_after_fun_decl() {
    let sample = "
    fun returnSum(a, b) {
  return a + b;
}
var a = returnSum(5,6);
";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Program::new(&tokens);
    assert!(x.unwrap().1.is_empty());
}

// #[test]
// fn test_syntax_error_missing_paren() {
//     let sample = "(1 2";
//     let scanner = Scanner::new(sample);
//     let tokens = scanner.collect::<Vec<_>>();
//     let x = Expression::new(&tokens);
//     assert_eq!(
//         x,
//         Err(LoxSyntaxError::UnexpectedToken {
//             lexeme: "2",
//             line: 0,
//             message: "Unexpected token"
//         })
//     );
// }

#[test]
fn test_syntax_error_incomplete_binary() {
    let sample = "3 +";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    let ex = &[Token {
        token: TokenType::PLUS,
        lexeme: "+",
        line: 0,
    }][..];
    assert_eq!(x, Ok((Expression::Number(3.0), ex)));
}

#[test]
fn test_parse_class_decl_no_inherit() {
    let sample = "class Foo {}";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Declaration::class_decl(&tokens);
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
    let sample = "class Foo < Bar {}";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Declaration::class_decl(&tokens);
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

#[test]
fn test_parse_demo() {
    let sample = "
    var a = 1;
    var b = 2;

    if (condition) {
        print a;
      } else {
        print b;
      }
    while (a < 10) {
      print a;
      a = a + 1;
    }

    for (var a = 1; a < 10; a = a + 1) {
        print a;
      }

      a.room(kitchen).makeBreakfast(bacon, eggs, toast)(hash_browns);

      fun printSum(a, b) {
        print a + b;
      }
      fun returnSum(a, b) {
        return a + b;
      }

      fun addPair(a, b) {
        return a + b;
      }

      fun identity(a) {
        return a;
      }

      print identity(addPair)(1, 2);

      fun outerFunction() {
        fun localFunction() {
          print \"I'm local!\";
        }

        localFunction();
      }

      fun returnFunction() {
        var outside = \"outside\";

        fun inner() {
          print outside;
        }

        return inner;
      }

      var fn = returnFunction();
      fn();

      class Breakfast {
        cook() {
          print \"Eggs a-fryin'!\";
        }

        serve(who) {
          print \"Enjoy your breakfast, \" + who + \".\";
        }
      }

      class Brunch < Breakfast {
        drink() {
          print \"How about a Bloody Mary?\";
        }
      }

      class Breakfast2 {
        init(meat, bread) {
            this.meat = meat;
            this.bread = bread;
        }
    }
    class Brunch < Breakfast {
        init(meat, bread, drink) {
            super.init(meat, bread);
            this.drink = drink;
        }
    }
    ";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Program::new(&tokens);
    println!("{:#?}", x);
}

#[test]
fn test_parse_member_assign() {
    let x = parse_str_with("x.y = 5", Expression::member_assign);
    assert_eq!(
        x,
        Expression::MemberAssign {
            path: vec![MemberAccess::Field("x".to_owned())],
            field: "y".to_owned(),
            value: Box::new(Expression::Number(5.0))
        },
    );
}

#[test]
fn test_parse_assign_const() {
    let sample = "
6=5
    ";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    println!("{:#?}", x);
}
