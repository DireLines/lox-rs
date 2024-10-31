#![allow(clippy::type_complexity)]
use crate::scanner::{Scanner, Token, TokenType};

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
        pub fn $functionname<'a>(
            tokens: &'a [Token<'a>],
        ) -> std::result::Result<(Self, &[Token<'a>]), LoxSyntaxError> {
            let res = grammar_rule!(@munch tokens $($tail)*);
            match res {
                Ok((body,tokens))=>{
                    Ok(($ast_build_fn(body.done()),tokens))
                },
                Err(e)=>{
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
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(ident)),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ => {
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
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(Expression::Number(value))),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ => {
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
                    match grammar_rule!(@munch tokens $($tail)*) {
                        Ok((parsed_tail_ast,tokens)) => {
                            Ok::<_, LoxSyntaxError>(((parsed_tail_ast.prep(Expression::String(value.to_string()))),tokens))
                        },
                        Err(e) =>Err(e)
                    }
                }
                _ =>  {
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
                //consume tokens for subtree
                let res = grammar_rule!(@munch leftover_tokens $($tail)*);
                match res {
                    Ok((parsed_tail_ast,tokens))=>Ok((parsed_tail_ast.prep(parsed_subtree_ast.done()), tokens)),
                    Err(e)=>Err(e)
                }
            },
            Err(e)=>{
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


    // OPTIONAL GROUP ( a b )?
   (@munch $tokens:ident ($($subtree:tt)+)? $($tail:tt)*) => {{
    let tokens = $tokens;
    let subtree = grammar_rule!(@munch tokens $($subtree)+);
    match subtree {
    Ok((parsed_subtree_ast,leftover_tokens))=>{
        // consume tokens for subtree
        let res = grammar_rule!(@munch leftover_tokens $($tail)*);
        match res {
            Ok((parsed_tail_ast,tokens))=>Ok(((parsed_tail_ast.prep(Some(parsed_subtree_ast.done()))),tokens)),
            Err(e)=>Err(e)
        }
    },
    Err(_)=>{
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
                let tokens = &$tokens[1..];
                let subtree = grammar_rule!(@munch tokens $($tail)*);
                match subtree {
                    Ok((parsed_tail_ast,tokens)) => {
                        Ok::<_, LoxSyntaxError>((parsed_tail_ast.prep($rep),tokens))
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

   //generic token but capture which token it was
   (@munch $tokens:ident {$tt:ident} $($tail:tt)*) => {{
    match $tokens.get(0) {
        Some(first_token)=>{
            if first_token.token == TokenType::$tt {
                let tokens = &$tokens[1..];
                let subtree = grammar_rule!(@munch tokens $($tail)*);
                match subtree {
                    Ok((parsed_tail_ast,tokens)) => {
                        Ok::<_, LoxSyntaxError>((parsed_tail_ast.prep(TokenType::$tt),tokens))
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
pub struct Program {
    pub body: Vec<Declaration>,
}

impl Program {
    grammar_rule!(Self::build_program : new -> ([Declaration::new])*);
    fn build_program(body: Vec<Declaration>) -> Self {
        Self { body }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoxSyntaxError<'a> {
    UnexpectedEof,
    UnexpectedToken {
        lexeme: &'a str,
        line: usize,
        message: &'static str,
    },
}
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Number(f64),
    String(String),
    Identifier {
        name: String,
        resolution_depth: Option<usize>,
    },
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
        path: Vec<MemberAccess>,
        field: String,
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
pub enum Declaration {
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
pub enum Statement {
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
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<Option<Statement>>,
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
pub enum MemberAccess {
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

//needed because for member assignment we need the pre-AST parsed version of a direct assignment
struct DirectAssignComponents {
    identifier: String,
    value: Expression,
}

impl DirectAssignComponents {
    grammar_rule!(Self::build_direct_assign : direct_assign -> IDENTIFIER EQUAL [Expression::assignment]);
    fn build_direct_assign((ident, value): (&str, Expression)) -> Self {
        Self {
            identifier: ident.to_string(),
            value,
        }
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
                            match DirectAssignComponents::direct_assign(tokens) {
                                Ok((direct, leftovers)) => {
                                    //construct full assignment
                                    //construct full target out of member_accesses + ident
                                    //pack into Expression
                                    return Ok((
                                        Self::build_member_assign((
                                            member_accesses,
                                            &direct.identifier,
                                            direct.value,
                                        )),
                                        leftovers,
                                    ));
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
        call -> [Expression::primary] ( ([MemberAccess::function_name] | (DOT [MemberAccess::field_name])) )* );
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

    fn build_member_assign((path, ident, value): (Vec<MemberAccess>, &str, Expression)) -> Self {
        Self::MemberAssign {
            path,
            field: ident.to_string(),
            value: Box::new(value),
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
        Self::Identifier {
            name: data.to_string(),
            resolution_depth: None,
        }
    }
    fn build_super_field_access(ident: &str) -> Self {
        Self::SuperFieldAccess {
            field: ident.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
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

/// Apply a parser directly to a string, confirm that there are no leftovers
///
/// Assumes parser should not produce any leftovers
pub fn parse_str_with<F, T>(input: &str, parser: F) -> T
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
mod tests_expr;
