#![allow(dead_code)] // <- TODO - remove

use phf::phf_map;
use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};

/// Prepend a new item to a Heterogenous collection
///
/// While parsing AST we want to collect items of different types - one way to store them
/// without making a dedicated type for each combination is with using tuples. This trait pushes
/// an item to such collection
trait Prepend<T> {
    type Out;
    /// Prepend an item to a Heterogenous collection
    fn prep(self, item: T) -> Self::Out;
}

/// `()` is a tuple of size 0 and prepending to it gives a tuple of size 1 - a problematic size
/// Instead we define prepending to `()` to give tuple of size 2
impl<T> Prepend<T> for () {
    type Out = (T, ());
    fn prep(self, item: T) -> Self::Out {
        (item, self)
    }
}
impl Done for () {
    type Out = ();
    fn done(self) {}
}

impl<T, I: Prepend<T>, E> Prepend<T> for ::std::result::Result<I, E> {
    type Out = std::result::Result<<I as Prepend<T>>::Out, E>;
    fn prep(self, item: T) -> Self::Out {
        match self {
            Ok(v) => Ok(v.prep(item)),
            Err(e) => Err(e),
        }
    }
}

/// Finalize Heterogenous collection and get back the results
///
/// Since we are treating collection of size 1 in a special way: `(T, ())` we want
/// to be able to deal with that in some universal way. This trait gives us done method
/// that strips () from a collection giving us either a tuple that is 1 smaller or just T
/// if it's a tuple of size 1
trait Done {
    type Out;
    fn done(self) -> Self::Out;
}

/// Implement Prepend for a tuple of some specific size
///
/// Since a tuple of every size is a separate datatype we have have to write Prepend instance
/// for each one of them. Doing it by hand is error prone - we can implement it with macro_rules
macro_rules! make_prepend  {
    ($($name:ident)*) => {
        impl<T, $($name),*> Prepend<T> for ($($name),*, ()) {
            type Out = (T, $($name),*, ());
            fn prep(self, item: T) -> Self::Out {
                #[allow(non_snake_case)]
                match self {
                    ($($name),*, ()) => (item, $($name),*, ()),
                }
            }
        }
        impl<$($name),*> Done for ($($name),*, ()) {
            #[allow(unused_parens)]
            type Out = ($($name),*);
            fn done(self) -> Self::Out {
                #[allow(non_snake_case)]
                match self {
                    ($($name),*, ()) => ( $($name),*),
                }
            }
        }
    }
}

/// Implement Prepend for a number of tuple sizes
///
/// Calling `make_prepend!` for each tuple size can also be error prone, we can use macro_rules for
/// that too!
macro_rules! make_prepends {
    ($first:ident $($name:ident)*) => {
        // this generates prepend for current size
        make_prepend!($first $($name)*);
        // this generates prepends for smaller sizes
        make_prepends!($($name)*);
    };
    // and that's the base case
    () => {};
}

// By magic of recursive macro we are covering tuples of size up to 13
make_prepends!(A B C D E F G H J K L M N);

const fn id<T>(item: T) -> T {
    item
}

macro_rules! grammar_rule {
    // Final goal for the grammar_rule rule is to generate a parser. This and the following
    // cases are entry points.
    //
    // Function generated in this entry point transforms the result before returning it
    ($ast_build_fn:path : $functionname:ident -> $($tail:tt)*) => {
        fn $functionname<'a>(
            tokens: &'a [Token<'a>],
        ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
            let desc = format!("{}::{}",std::any::type_name::<Self>(),stringify!($functionname));
            let res = grammar_rule!(@munch tokens $($tail)*);
            match res {
                Ok((body,tokens))=>{
                    // println!("{desc} ok");
                    Ok(($ast_build_fn(body.done()),tokens))
                },
                Err(e)=>{
                    // println!("{desc} not ok");
                    Err(e)
                }
            }
        }
    };

    // This entry point generates the function that returns the result unchanged
    ($functionname:ident -> $($tail:tt)*) => {
        grammar_rule!(id : $functionname -> $($tail)*);
    };

   (@munch $tokens:ident IDENTIFIER $($tail:tt)*) => {{
    match $tokens.get(0){
        Some(first_token) =>{
            let tokens = &$tokens[1..];
            match first_token.token {
                TokenType::IDENTIFIER(ident) => {
                    // println!("IDENTIFIER \"{}\" found",ident);
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(ident)),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ => {
                    // println!("IDENTIFIER not found");
                    Err(LoxSyntaxError::UnexpectedToken{
                    lexeme: first_token.lexeme,
                    line: first_token.line,
                    message: "Unexpected token",
                })
                }
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
                    // println!("NUMBER {} found",value);
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(Expression::Number(value))),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ => {
                    // println!("NUMBER not found");
                    Err(LoxSyntaxError::UnexpectedToken{
                        lexeme: first_token.lexeme,
                        line: first_token.line,
                        message: "Unexpected token",
                })
            }
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
                    // println!("STRING \"{}\" found",value);
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(Expression::String(value.to_string()))),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ =>  {
                    // println!("STRING not found");
                    Err(LoxSyntaxError::UnexpectedToken{
                        lexeme: first_token.lexeme,
                        line: first_token.line,
                        message: "Unexpected token",
                })
            }
            }
        },
        None => Err(LoxSyntaxError::UnexpectedEof)
    }
   }};

    //REPEATED GROUP ( a b )*
    (@munch $tokens:ident ($($subtree:tt)+)* $($tail:tt)*) => {{
        // println!("repeated group");
    let mut tokens = $tokens;
    let mut results = vec![];
    loop {
        let subtree = grammar_rule!(@munch tokens $($subtree)+);
        match subtree {
            Ok((parsed_subtree_ast,leftover_tokens))=>{
                //consume tokens for subtree
                results.push(parsed_subtree_ast.done());
                tokens = leftover_tokens;
            },
            Err(_)=>{
                break;
            }
        }
    }
    let res =grammar_rule!(@munch tokens $($tail)*);
    match res {
        Ok((parsed_tail_ast,tokens))=> Ok((parsed_tail_ast.prep(results),tokens)),
        Err(e)=>Err(e),
    }
    }};


    //OR GROUP ( a | b | c )
    (@munch $tokens:ident ($first:tt | $($variants:tt)|*) $($tail:tt)*) => {{
        let tokens = $tokens;
        let subtree = grammar_rule!(@orgroup tokens $first | $($variants)|*);
        match subtree {
            Ok((parsed_subtree_ast,leftover_tokens))=>{
                // println!("or group: {} ok",stringify!($first));
                //consume tokens for subtree
                let res = grammar_rule!(@munch leftover_tokens $($tail)*);
                match res {
                    Ok((parsed_tail_ast,tokens))=>Ok((parsed_tail_ast.prep(parsed_subtree_ast.done()), tokens)),
                    Err(e)=>Err(e)
                }
            },
            Err(e)=>{
                // println!("or group: {} not ok",stringify!($first));
                //TODO: did not match any of the acceptable variants
                Err(e)
            }
        }
        }};

    // this handles continuation in or group
    (@orgroup $tokens:ident $first_variant:tt | $($remaining_variants:tt)|*) => {{
        let tokens = $tokens;
        let subtree = grammar_rule!(@munch tokens $first_variant);
        match subtree {
            Ok(_)=>{
                // println!("or group: {} ok",stringify!($first_variant));
                subtree
            },
            Err(_)=>{
                // println!("or group: {} not ok",stringify!($first_variant));
                grammar_rule!(@orgroup tokens $($remaining_variants)|*)
            }
        }
    }};
    (@orgroup $tokens:ident $first_variant:tt) => {{
        grammar_rule!(@munch $tokens $first_variant)
    }};


    // OPTIONAL GROUP ( a b )?
   (@munch $tokens:ident ($($subtree:tt)+)? $($tail:tt)*) => {{
    println!("optional group");
    let tokens = $tokens;
    let subtree = grammar_rule!(@munch tokens $($subtree)+);
    match subtree {
    Ok((parsed_subtree_ast,leftover_tokens))=>{
        // println!("optional group: {} valid",stringify!($($subtree)+));
        // consume tokens for subtree
        let res = grammar_rule!(@munch leftover_tokens $($tail)*);
        match res {
            Ok((parsed_tail_ast,tokens))=>Ok(((parsed_tail_ast.prep(Some(parsed_subtree_ast.done()))),tokens)),
            Err(e)=>Err(e)
        }
    },
    Err(_)=>{
        // println!("optional group: {} not valid",stringify!($($subtree)+));
        //parse without consuming tokens for subtree
        let res = grammar_rule!(@munch tokens $($tail)*);
        match res {
            Ok((parsed_tail_ast,tokens))=>Ok(((parsed_tail_ast.prep(None)),tokens)),
            Err(e)=>Err(e)
        }
    }
   }
   }};

    //PARENTHESIZED GROUP ( a b )
    (@munch $tokens:ident ($($subtree:tt)+) $($tail:tt)*) => {{
    // println!("parenthesized group");
    let mut tokens = $tokens;
    let subtree = grammar_rule!(@munch tokens $($subtree)+);
    match subtree {
        Ok((parsed_subtree_ast,leftover_tokens))=>{
            //consume tokens for subtree
            let results = parsed_subtree_ast.done();
            tokens = leftover_tokens;
            let res = grammar_rule!(@munch tokens $($tail)*);
            match res {
                Ok((parsed_tail_ast,tokens))=>Ok((parsed_tail_ast.prep(results),tokens)),
                Err(e)=>Err(e)
            }
        },
        Err(e)=>{
            Err(e)
        }
    }
    }};

    //generic token but represent it in the output as rep
    (@munch $tokens:ident {$tt:ident : $rep:expr} $($tail:tt)*) => {{
        match $tokens.get(0) {
        Some(first_token)=>{
            if first_token.token == TokenType::$tt {
                // println!("{:?} found",TokenType::$tt);
                let tokens = &$tokens[1..];
                let subtree = grammar_rule!(@munch tokens $($tail)*);
                match subtree {
                    Ok((parsed_tail_ast,tokens)) => {
                        Ok::<_, LoxSyntaxError>((parsed_tail_ast.prep($rep),tokens))
                    },
                    Err(e) =>Err(e)
                }
            } else {
                // println!("{:?} not found",TokenType::$tt);
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

   //generic token but capture which token it was
   (@munch $tokens:ident {$tt:ident} $($tail:tt)*) => {{
    match $tokens.get(0) {
        Some(first_token)=>{
            if first_token.token == TokenType::$tt {
                // println!("{:?} found",TokenType::$tt);
                let tokens = &$tokens[1..];
                let subtree = grammar_rule!(@munch tokens $($tail)*);
                match subtree {
                    Ok((parsed_tail_ast,tokens)) => {
                        Ok::<_, LoxSyntaxError>((parsed_tail_ast.prep(TokenType::$tt),tokens))
                    },
                    Err(e) =>Err(e)
                }
            } else {
                // println!("{:?} not found",TokenType::$tt);
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
                // println!("{:?} found",TokenType::$tt);
                let tokens = &$tokens[1..];
                grammar_rule!(@munch tokens $($tail)*)
            } else {
                // println!("{:?} not found",TokenType::$tt);
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
                let res = grammar_rule!(@munch leftover_tokens $($tail)*);
                match res {
                    Ok((parsed_tail_ast,tokens)) => Ok((parsed_tail_ast.prep(parsed_subtree_ast), tokens)),
                    Err(e)=>Err(e),
                }
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
struct Program {
    body: Vec<Declaration>,
}

impl Program {
    grammar_rule!(Self::build_program : new -> ([Declaration::new])*);
    fn build_program(body: Vec<Declaration>) -> Self {
        Self { body }
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
    Number(f64),
    String(String),
    Identifier(String),
    Bool(bool),
    Nil,
    This,
    Grouping {
        expression: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expression>,
    },
    MemberAssign {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Call {
        base: Box<Expression>,
        path: Vec<MemberAccess>,
    },
    SuperFieldAccess {
        field: String,
    },
}

#[derive(Debug, PartialEq, Clone)]
enum Declaration {
    ClassDecl {
        name: String,
        parent_name: Option<String>,
        body: Vec<Function>,
    },
    FunDecl(Function),
    VarDecl {
        name: String,
        definition: Option<Expression>,
    },
    Statement(Box<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    ExprStmt(Expression),
    ForStmt {
        stmt: Box<Statement>,
        condition: Box<Option<Expression>>,
        increment: Box<Option<Expression>>,
        body: Box<Statement>,
    },
    IfStmt {
        condition: Box<Expression>,
        if_case: Box<Statement>,
        else_case: Box<Option<Statement>>,
    },
    PrintStmt(Expression),
    ReturnStmt(Option<Expression>),
    WhileStmt {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    Block {
        body: Vec<Declaration>,
    },
    VarDecl(Declaration),
    EmptyStmt,
}

impl Statement {
    grammar_rule!(new -> ([Statement::expr_stmt] | [Statement::for_stmt] | [Statement::if_stmt] | [Statement::print_stmt] | [Statement::return_stmt] | [Statement::while_stmt] | [Statement::block]) );
    grammar_rule!(Self::build_expr_stmt : expr_stmt -> [Expression::new] SEMICOLON );
    grammar_rule!(Self::build_for_stmt : for_stmt -> FOR LEFT_PAREN
        ( [Statement::var_decl] | [Statement::expr_stmt] | [Statement::empty_stmt] )
        ( [Expression::new] )? SEMICOLON
        ( [Expression::new] )? RIGHT_PAREN [Statement::new] );
    grammar_rule!(Self::build_if_stmt : if_stmt -> IF LEFT_PAREN [Expression::new] RIGHT_PAREN [Statement::new] ( ELSE [Statement::new] )? );
    grammar_rule!(Self::build_print_stmt : print_stmt -> PRINT [Expression::new] SEMICOLON );
    grammar_rule!(Self::build_return_stmt : return_stmt -> RETURN ([Expression::new])? SEMICOLON );
    grammar_rule!(Self::build_while_stmt : while_stmt -> WHILE LEFT_PAREN [Expression::new] RIGHT_PAREN [Statement::new] );
    grammar_rule!(Self::build_block : block -> LEFT_BRACE ([Declaration::new])* RIGHT_BRACE );
    grammar_rule!(Self::build_empty_stmt : empty_stmt -> SEMICOLON);
    grammar_rule!(Self::build_var_decl : var_decl -> [Declaration::var_decl]);

    fn build_empty_stmt(_: ()) -> Self {
        Self::EmptyStmt
    }
    fn build_expr_stmt(expr: Expression) -> Self {
        Self::ExprStmt(expr)
    }
    fn build_var_decl(decl: Declaration) -> Self {
        Self::VarDecl(decl)
    }
    // fn varDecl<'a>(
    //     tokens: &'a [Token<'a>],
    // ) -> ::std::result::Result<(Self, &'a [Token<'a>]), LoxSyntaxError<'a>> {
    //     todo!()
    // }

    fn build_for_stmt(
        (stmt, condition, increment, body): (
            Statement,
            Option<Expression>,
            Option<Expression>,
            Statement,
        ),
    ) -> Self {
        Self::ForStmt {
            stmt: Box::new(stmt),
            condition: Box::new(condition),
            increment: Box::new(increment),
            body: Box::new(body),
        }
    }
    fn build_if_stmt(
        (condition, if_case, else_case): (Expression, Statement, Option<Statement>),
    ) -> Self {
        Self::IfStmt {
            condition: Box::new(condition),
            if_case: Box::new(if_case),
            else_case: Box::new(else_case),
        }
    }
    fn build_print_stmt(expr: Expression) -> Self {
        Self::PrintStmt(expr)
    }
    fn build_return_stmt(value: Option<Expression>) -> Self {
        Self::ReturnStmt(value)
    }
    fn build_while_stmt((condition, body): (Expression, Statement)) -> Self {
        Self::WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }
    fn build_block(decls: Vec<Declaration>) -> Self {
        Self::Block { body: decls }
    }
}
#[derive(Debug, PartialEq, Clone)]
struct Function {
    name: String,
    parameters: Vec<String>,
    body: Box<Option<Statement>>,
}
impl Function {
    grammar_rule!(Self::build_function : new -> IDENTIFIER LEFT_PAREN (IDENTIFIER (COMMA IDENTIFIER)* )? RIGHT_PAREN ([Statement::block])?);
    fn build_function(
        (name, params, stmt): (&str, Option<(&str, Vec<&str>)>, Option<Statement>),
    ) -> Self {
        let params = if let Some((first_param, other_params)) = params {
            let mut params: Vec<String> = other_params.iter().map(|e| e.to_string()).collect();
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
}

impl Declaration {
    grammar_rule!(new -> ([Declaration::class_decl] | [Declaration::fun_decl] | [Declaration::var_decl] | [Declaration::statement]) );
    grammar_rule!(Self::build_class_decl : class_decl -> CLASS IDENTIFIER ( LESS IDENTIFIER )? LEFT_BRACE ([Function::new])* RIGHT_BRACE);
    grammar_rule!(Self::build_fun_decl : fun_decl -> FUN [Function::new] );
    grammar_rule!(Self::build_var_decl : var_decl -> VAR IDENTIFIER ( EQUAL [Expression::new] )? SEMICOLON );
    grammar_rule!(Self::build_statement : statement -> [Statement::new]);

    fn build_class_decl(data: (&str, Option<&str>, Vec<Function>)) -> Self {
        let (ident, parent_name, body) = data;
        Self::ClassDecl {
            name: ident.to_string(),
            parent_name: parent_name.map(|s| s.to_string()),
            body,
        }
    }
    fn build_fun_decl(data: Function) -> Self {
        Self::FunDecl(data)
    }
    fn build_var_decl(data: (&str, Option<Expression>)) -> Self {
        let (ident, defn) = data;
        Self::VarDecl {
            name: ident.to_string(),
            definition: defn,
        }
    }
    fn build_statement(data: Statement) -> Self {
        Self::Statement(Box::new(data))
    }
}
#[derive(Debug, PartialEq, Clone)]
enum MemberAccess {
    Field(String),
    Args { args: Vec<Expression> },
}

impl MemberAccess {
    grammar_rule!(new -> ([MemberAccess::field_name] | [MemberAccess::function_name]));
    grammar_rule!(Self::build_function_name : function_name ->  LEFT_PAREN ([Expression::new] ( COMMA [Expression::new] )* )? RIGHT_PAREN);
    grammar_rule!(Self::build_field_name : field_name -> IDENTIFIER);
    fn build_function_name(data: Option<(Expression, Vec<Expression>)>) -> Self {
        let args = data.map_or(Vec::new(), |(first, mut rest)| {
            rest.insert(0, first);
            rest
        });
        MemberAccess::Args { args }
    }
    fn build_field_name(name: &str) -> Self {
        MemberAccess::Field(name.to_string())
    }
}

impl Expression {
    grammar_rule!(new -> [Expression::assignment] );
    grammar_rule!(assignment -> ([Expression::member_assign] | [Expression::logic_or] ));
    fn member_assign<'a>(
        mut tokens: &'a [Token<'a>],
    ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
        let mut member_accesses = vec![];
        loop {
            match MemberAccess::new(tokens) {
                Ok((member_access, leftovers)) => match leftovers.get(0) {
                    Some(t) => {
                        if t.token == TokenType::DOT {
                            member_accesses.push(member_access);
                            tokens = &leftovers[1..];
                        } else {
                            //last one
                            //is it an identifier
                            match Expression::direct_assign(tokens) {
                                Ok((direct, leftovers)) => {
                                    //construct full assignment
                                    todo!()
                                }
                                Err(e) => return Err(e),
                            }
                        }
                    }
                    None => return Err(LoxSyntaxError::UnexpectedEof),
                },
                Err(e) => return Err(e),
            }
        }
    }
    grammar_rule!(Self::build_direct_assign : direct_assign -> IDENTIFIER EQUAL [Expression::assignment]); //private helper function that does not product Expression
    grammar_rule!(call_dot -> [Expression::call] DOT);
    grammar_rule!(Self::build_logic_or : logic_or -> [Expression::logic_and] ( OR [Expression::logic_and] )* );
    grammar_rule!(Self::build_logic_and : logic_and -> [Expression::equality] ( AND [Expression::equality] )* );
    grammar_rule!(Self::build_equality : equality -> [Expression::comparison] ( ( {BANG_EQUAL} | {EQUAL_EQUAL} ) [Expression::comparison] )* );
    grammar_rule!(Self::build_comparison : comparison -> [Expression::term] ( ( {GREATER} | {GREATER_EQUAL} | {LESS} | {LESS_EQUAL} ) [Expression::term] )* );
    grammar_rule!(Self::build_term : term -> [Expression::factor] ( ( {MINUS} | {PLUS} ) [Expression::factor] )* );
    grammar_rule!(Self::build_factor : factor -> [Expression::unary] ( ( {SLASH} | {STAR} ) [Expression::unary] )* );
    grammar_rule!(unary -> ( [Expression::recursive_unary] | [Expression::call]));
    grammar_rule!(Self::build_recursive_unary : recursive_unary -> ( {BANG} | {MINUS} ) [Expression::unary]);
    grammar_rule!(Self::build_call :
        call -> [Expression::primary] ( [MemberAccess::new] )* );
    grammar_rule!(primary ->
        ({TRUE:Expression::Bool(true)} |
         {FALSE:Expression::Bool(false)} |
         {NIL:Expression::Nil} |
         {THIS:Expression::This} |
         NUMBER |
         STRING |
         [Expression::identifier] |
         (LEFT_PAREN [Expression::new] RIGHT_PAREN) |
         [Expression::super_field_access] ));
    grammar_rule!(Self::build_ident : identifier -> IDENTIFIER);
    grammar_rule!(Self::build_super_field_access : super_field_access -> SUPER DOT IDENTIFIER);
    fn build_logic_or((first, rest): (Expression, Vec<Expression>)) -> Self {
        let mut result = first;
        for expr in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: BinaryOperator::Or,
                right: Box::new(expr),
            }
        }
        result
    }
    fn build_logic_and((first, rest): (Expression, Vec<Expression>)) -> Self {
        let mut result = first;
        for expr in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: BinaryOperator::And,
                right: Box::new(expr),
            }
        }
        result
    }
    fn build_equality((first, rest): (Expression, Vec<(TokenType, Expression)>)) -> Self {
        let token_type_to_op = |tt: TokenType| {
            if tt == TokenType::EQUAL_EQUAL {
                BinaryOperator::EqualEqual
            } else if tt == TokenType::BANG_EQUAL {
                BinaryOperator::NotEqual
            } else {
                unreachable!();
            }
        };
        let mut result = first;
        for (tt, expr) in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: token_type_to_op(tt),
                right: Box::new(expr),
            }
        }
        result
    }
    fn build_comparison((first, rest): (Expression, Vec<(TokenType, Expression)>)) -> Self {
        let token_type_to_op = |tt: TokenType| {
            if tt == TokenType::GREATER {
                BinaryOperator::Greater
            } else if tt == TokenType::GREATER_EQUAL {
                BinaryOperator::GreaterEqual
            } else if tt == TokenType::LESS {
                BinaryOperator::Less
            } else if tt == TokenType::LESS_EQUAL {
                BinaryOperator::LessEqual
            } else {
                unreachable!();
            }
        };
        let mut result = first;
        for (tt, expr) in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: token_type_to_op(tt),
                right: Box::new(expr),
            }
        }
        result
    }

    fn build_member_assign((call, ident, value): (Option<Expression>, &str, Expression)) -> Self {
        if let Some(member) = call {
            Self::MemberAssign {
                target: Box::new(member),
                value: Box::new(value),
            }
        } else {
            todo!()
        }
    }
    fn build_term((first, rest): (Expression, Vec<(TokenType, Expression)>)) -> Self {
        let token_type_to_op = |tt: TokenType| {
            if tt == TokenType::PLUS {
                BinaryOperator::Plus
            } else if tt == TokenType::MINUS {
                BinaryOperator::Minus
            } else {
                unreachable!();
            }
        };
        let mut result = first;
        for (tt, expr) in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: token_type_to_op(tt),
                right: Box::new(expr),
            }
        }
        result
    }
    fn build_factor((first, rest): (Expression, Vec<(TokenType, Expression)>)) -> Self {
        let token_type_to_op = |tt: TokenType| {
            if tt == TokenType::SLASH {
                BinaryOperator::Div
            } else if tt == TokenType::STAR {
                BinaryOperator::Mul
            } else {
                unreachable!();
            }
        };
        let mut result = first;
        for (tt, expr) in rest {
            result = Self::Binary {
                left: Box::new(result),
                operator: token_type_to_op(tt),
                right: Box::new(expr),
            }
        }
        result
    }
    fn build_call((base, path): (Expression, Vec<MemberAccess>)) -> Self {
        if path.is_empty() {
            //special case: collapse call to primary expr
            return base;
        }
        Self::Call {
            base: Box::new(base),
            path,
        }
    }
    fn build_recursive_unary((tt, expr): (TokenType, Expression)) -> Self {
        let token_type_to_op = |tt: TokenType| {
            if tt == TokenType::MINUS {
                UnaryOperator::Neg
            } else if tt == TokenType::BANG {
                UnaryOperator::Not
            } else {
                unreachable!();
            }
        };
        Self::Unary {
            operator: token_type_to_op(tt),
            right: Box::new(expr),
        }
    }
    fn build_ident(data: &str) -> Self {
        Self::Identifier(data.to_string())
    }
    fn build_super_field_access(ident: &str) -> Self {
        Self::SuperFieldAccess {
            field: ident.to_string(),
        }
    }
    fn build_direct_assign((ident, value): (&str, Expression)) -> Self {
        let target = Expression::Identifier(ident.to_string());
        Self::MemberAssign {
            target: Box::new(target),
            value: Box::new(value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
enum BinaryOperator {
    Minus,
    Plus,
    Div,
    Mul,
    Equal,
    EqualEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Or,
    And,
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
    match &args[1..] {
        &[] => run_prompt()?,
        [file] => run_file(file)?,
        _ => {
            println!("Usage: lox [script]");
            return Ok(());
        }
    }
    Ok(())
}

fn run_file(path: &str) -> Result<()> {
    //get all bytes of file
    let s = std::fs::read_to_string(path)?;
    //run on the string
    run(&s);
    Ok(())
}

fn run_prompt() -> Result<()> {
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
struct Token<'a> {
    token: TokenType<'a>,
    lexeme: &'a str,
    line: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[rustfmt::skip]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
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
    let scanner = Scanner::new(source);
    //get tokens from scanner
    for token in scanner {
        println!("{token:#?}");
    }
    //print tokens
    println!("running on {source}");
}

/// Apply a parser directly to a string, confirm that there are no leftovers
///
/// Assumes parser should not produce any leftovers
#[cfg(test)]
fn parse_str_with<F, T>(input: &str, parser: F) -> T
where
    F: for<'a> Fn(&'a [Token<'a>]) -> std::result::Result<(T, &'a [Token<'a>]), LoxSyntaxError<'a>>,
    T: std::fmt::Debug,
{
    let scanner = Scanner::new(input);
    let tokens = scanner.collect::<Vec<_>>();
    match parser(&tokens) {
        Ok((expr, leftovers)) => {
            if leftovers.is_empty() {
                expr
            } else {
                panic!("Managed to parse {input:?} into {expr:?}, but there are leftovers: {leftovers:?}");
            }
        }
        Err(err) => {
            panic!("Failed to parse {input:?} into expression: {err:?}")
        }
    }
}

#[cfg(test)]
mod tests_expr {

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
                    target: Box::new(Expression::Identifier("time".into())),
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
    use crate::Expression::*;
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
fn test_syntax_error_missing_paren() {
    let sample = "(1 2";
    let scanner = Scanner::new(sample);
    let tokens = scanner.collect::<Vec<_>>();
    let x = Expression::new(&tokens);
    assert_eq!(
        x,
        Err(LoxSyntaxError::UnexpectedToken {
            lexeme: "2",
            line: 0,
            message: "Unexpected token"
        })
    );
}

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

      class Breakfast {
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
    assert_eq!(x, Expression::Number(3.0),);
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
