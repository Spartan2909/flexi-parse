//! A parser and interpreter for the Lox language from
//! [`Crafting Interpreters`].
//!
//! [Crafting Interpreters]: https://craftinginterpreters.com

use std::cell::Cell;

use flexi_parse::group;
use flexi_parse::group::Braces;
use flexi_parse::group::Group;
use flexi_parse::group::Parentheses;
use flexi_parse::parse_repeated;
use flexi_parse::parse_string;
use flexi_parse::peek2_any;
use flexi_parse::pretty_unwrap;
use flexi_parse::punctuated::Punctuated;
use flexi_parse::token::Ident;
use flexi_parse::token::LitFloat;
use flexi_parse::token::LitInt;
use flexi_parse::token::LitStrDoubleQuote as LitStr;
use flexi_parse::Parse;
use flexi_parse::ParseStream;
use flexi_parse::Parser;
use flexi_parse::Punct;
use flexi_parse::Result;

mod interpreter;
mod resolver;

mod error_codes {
    pub const INVALID_ASSIGN: u16 = 0;
    pub const TOO_MANY_ARGS: u16 = 1;
    pub const TYPE_ERROR: u16 = 2;
    pub const UNDEFINED_NAME: u16 = 3;
    pub const INCORRECT_ARITY: u16 = 4;
    pub const INVALID_INITIALISER: u16 = 5;
    pub const SHADOW: u16 = 6;
    pub const RETURN_OUTSIDE_FUNCTION: u16 = 7;
    pub const THIS_OUTSIDE_CLASS: u16 = 8;
    pub const CYCLICAL_INHERITANCE: u16 = 9;
    pub const INHERIT_FROM_VALUE: u16 = 10;
    pub const INVALID_SUPER: u16 = 11;
}

mod kw {
    use flexi_parse::keywords_prefixed;

    keywords_prefixed![
        "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return",
        "super", "this", "true", "var", "while"
    ];
}

#[derive(Debug, Clone, PartialEq)]
enum Binary {
    Mul(Expr, Punct!["*"], Expr),
    Div(Expr, Punct!["/"], Expr),

    Add(Expr, Punct!["+"], Expr),
    Sub(Expr, Punct!["-"], Expr),

    Equal(Expr, Punct!["=="], Expr),
    NotEqual(Expr, Punct!["!="], Expr),

    Greater(Expr, Punct![">"], Expr),
    GreaterEqual(Expr, Punct![">="], Expr),
    Less(Expr, Punct!["<"], Expr),
    LessEqual(Expr, Punct!["<="], Expr),
}

impl Binary {
    fn left(&self) -> &Expr {
        match self {
            Binary::Mul(left, _, _)
            | Binary::Div(left, _, _)
            | Binary::Add(left, _, _)
            | Binary::Sub(left, _, _)
            | Binary::Equal(left, _, _)
            | Binary::NotEqual(left, _, _)
            | Binary::Greater(left, _, _)
            | Binary::GreaterEqual(left, _, _)
            | Binary::Less(left, _, _)
            | Binary::LessEqual(left, _, _) => left,
        }
    }

    fn right(&self) -> &Expr {
        match self {
            Binary::Mul(_, _, right)
            | Binary::Div(_, _, right)
            | Binary::Add(_, _, right)
            | Binary::Sub(_, _, right)
            | Binary::Equal(_, _, right)
            | Binary::NotEqual(_, _, right)
            | Binary::Greater(_, _, right)
            | Binary::GreaterEqual(_, _, right)
            | Binary::Less(_, _, right)
            | Binary::LessEqual(_, _, right) => right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Literal {
    False(kw::keyword_false),
    Float(LitFloat),
    Int(LitInt),
    Nil(kw::keyword_nil),
    String(LitStr),
    True(kw::keyword_true),
}

#[derive(Debug, Clone, PartialEq)]
enum Logical {
    And(Expr, kw::keyword_and, Expr),
    Or(Expr, kw::keyword_or, Expr),
}

impl Logical {
    fn left(&self) -> &Expr {
        match self {
            Logical::And(left, _, _) | Logical::Or(left, _, _) => left,
        }
    }

    fn right(&self) -> &Expr {
        match self {
            Logical::And(_, _, right) | Logical::Or(_, _, right) => right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Unary {
    Neg(Punct!["-"], Expr),
    Not(Punct!["!"], Expr),
}

impl Unary {
    fn right(&self) -> &Expr {
        match self {
            Unary::Neg(_, right) | Unary::Not(_, right) => right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Assign {
        name: Ident,
        value: Box<Expr>,
        distance: Cell<Option<usize>>,
    },
    Binary(Box<Binary>),
    Call {
        callee: Box<Expr>,
        paren: Parentheses,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Ident,
    },
    Group(Box<Expr>),
    Literal(Literal),
    Logical(Box<Logical>),
    Set {
        object: Box<Expr>,
        name: Ident,
        value: Box<Expr>,
    },
    Super {
        keyword: kw::keyword_super,
        distance: Cell<Option<usize>>,
        dot: Punct!["."],
        method: Ident,
    },
    This {
        keyword: kw::keyword_this,
        distance: Cell<Option<usize>>,
    },
    Unary(Box<Unary>),
    Variable {
        name: Ident,
        distance: Cell<Option<usize>>,
    },
}

impl Expr {
    fn binary(binary: Binary) -> Self {
        Expr::Binary(Box::new(binary))
    }

    fn assignment(input: ParseStream) -> Result<Self> {
        let expr = Expr::or(input)?;

        if input.peek(Punct!["="]) {
            let equals: Punct!["="] = input.parse()?;
            let value = Box::new(Expr::assignment(input)?);

            if let Expr::Variable { name, .. } = expr {
                return Ok(Expr::Assign {
                    name,
                    value,
                    distance: Cell::new(None),
                });
            } else if let Expr::Get { object, name } = expr {
                return Ok(Expr::Set {
                    object,
                    name,
                    value,
                });
            }

            input.add_error(input.new_error(
                "Invalid assignment target".to_string(),
                &equals,
                error_codes::INVALID_ASSIGN,
            ))
        }

        Ok(expr)
    }

    fn or(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::and(input)?;

        while input.peek(kw::keyword_or) {
            expr = Expr::Logical(Box::new(Logical::Or(
                expr,
                input.parse()?,
                Expr::and(input)?,
            )));
        }

        Ok(expr)
    }

    fn and(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::equality(input)?;

        while input.peek(kw::keyword_and) {
            expr = Expr::Logical(Box::new(Logical::And(
                expr,
                input.parse()?,
                Expr::equality(input)?,
            )));
        }

        Ok(expr)
    }

    fn equality(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::comparison(input)?;

        loop {
            if input.peek(Punct!["=="]) {
                expr = Expr::binary(Binary::Equal(
                    expr,
                    input.parse()?,
                    Expr::comparison(input)?,
                ));
            } else if input.peek(Punct!["!="]) {
                expr = Expr::binary(Binary::NotEqual(
                    expr,
                    input.parse()?,
                    Expr::comparison(input)?,
                ));
            } else {
                break Ok(expr);
            }
        }
    }

    fn comparison(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::term(input)?;

        loop {
            if input.peek(Punct![">"]) {
                expr = Expr::binary(Binary::Greater(expr, input.parse()?, Expr::term(input)?));
            } else if input.peek(Punct![">="]) {
                expr = Expr::binary(Binary::GreaterEqual(
                    expr,
                    input.parse()?,
                    Expr::term(input)?,
                ));
            } else if input.peek(Punct!["<"]) {
                expr = Expr::binary(Binary::Less(expr, input.parse()?, Expr::term(input)?));
            } else if input.peek(Punct!["<="]) {
                expr = Expr::binary(Binary::LessEqual(expr, input.parse()?, Expr::term(input)?));
            } else {
                break Ok(expr);
            }
        }
    }

    fn term(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::factor(input)?;

        loop {
            if input.peek(Punct!["+"]) {
                expr = Expr::binary(Binary::Add(expr, input.parse()?, Expr::factor(input)?));
            } else if input.peek(Punct!["-"]) {
                expr = Expr::binary(Binary::Sub(expr, input.parse()?, Expr::factor(input)?));
            } else {
                break Ok(expr);
            }
        }
    }

    fn factor(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::unary(input)?;

        loop {
            if input.peek(Punct!["*"]) {
                expr = Expr::binary(Binary::Mul(expr, input.parse()?, Expr::unary(input)?));
            } else if input.peek(Punct!["/"]) {
                expr = Expr::binary(Binary::Div(expr, input.parse()?, Expr::unary(input)?));
            } else {
                break Ok(expr);
            }
        }
    }

    fn unary(input: ParseStream) -> Result<Self> {
        if input.peek(Punct!["-"]) {
            Ok(Expr::Unary(Box::new(Unary::Neg(
                input.parse()?,
                Expr::unary(input)?,
            ))))
        } else if input.peek(Punct!["!"]) {
            Ok(Expr::Unary(Box::new(Unary::Not(
                input.parse()?,
                Expr::unary(input)?,
            ))))
        } else {
            Expr::call(input)
        }
    }

    fn call(input: ParseStream) -> Result<Self> {
        let mut expr = Expr::primary(input)?;

        loop {
            if input.peek(Punct!["("]) {
                expr = Expr::finish_call(input, expr)?;
            } else if input.peek(Punct!["."]) {
                let _: Punct!["."] = input.parse()?;
                let name: Ident = input.parse()?;
                expr = Expr::Get {
                    object: Box::new(expr),
                    name,
                };
            } else {
                break Ok(expr);
            }
        }
    }

    fn finish_call(input: ParseStream<'_>, callee: Expr) -> Result<Self> {
        let content;
        let paren: Parentheses = group!(content in input);
        let arguments: Punctuated<Expr, Punct![","]> =
            Punctuated::parse_separated_trailing(&content)?;
        let arguments: Vec<_> = arguments.into_iter().collect();
        if arguments.len() >= 255 {
            input.add_error(input.new_error(
                "Can't have more than 254 arguments".to_string(),
                paren.0.clone(),
                error_codes::TOO_MANY_ARGS,
            ));
        }
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn primary(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead();
        if lookahead.peek(kw::keyword_false) {
            Ok(Expr::Literal(Literal::False(input.parse()?)))
        } else if lookahead.peek(kw::keyword_true) {
            Ok(Expr::Literal(Literal::True(input.parse()?)))
        } else if lookahead.peek(kw::keyword_nil) {
            Ok(Expr::Literal(Literal::Nil(input.parse()?)))
        } else if lookahead.peek(LitFloat) {
            Ok(Expr::Literal(Literal::Float(input.parse()?)))
        } else if lookahead.peek(LitInt) {
            Ok(Expr::Literal(Literal::Int(input.parse()?)))
        } else if lookahead.peek(LitStr) {
            Ok(Expr::Literal(Literal::String(input.parse()?)))
        } else if input.peek(kw::keyword_super) {
            Ok(Expr::Super {
                keyword: input.parse()?,
                distance: Cell::new(None),
                dot: input.parse()?,
                method: input.parse()?,
            })
        } else if input.peek(kw::keyword_this) {
            Ok(Expr::This {
                keyword: input.parse()?,
                distance: Cell::new(None),
            })
        } else if lookahead.peek(Ident) {
            Ok(Expr::Variable {
                name: kw::ident(input)?,
                distance: Cell::new(None),
            })
        } else if lookahead.peek(Punct!["("]) {
            let content;
            let _: Parentheses = group!(content in input);
            Ok(Expr::Group(Box::new(content.parse()?)))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        Expr::assignment(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Function {
    name: Ident,
    params: Vec<Ident>,
    body: Vec<Stmt>,
}

impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let mut contents: Group<Parentheses> = input.parse()?;
        contents.remove_whitespace();
        let Parentheses(span) = contents.delimiters();
        let tokens = contents.into_token_stream();
        let params: Vec<Ident> = if tokens.is_empty() {
            vec![]
        } else {
            let params: Punctuated<Ident, Punct![","]> =
                Punctuated::parse_separated.parse(tokens)?;
            params.into_iter().collect()
        };
        if params.len() >= 255 {
            input.add_error(input.new_error(
                "Can't have more than 254 parameters".to_string(),
                span,
                error_codes::TOO_MANY_ARGS,
            ));
        }
        let mut contents: Group<Braces> = input.parse()?;
        contents.remove_whitespace();
        let body = block.parse(contents.into_token_stream())?;
        Ok(Function { name, params, body })
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Stmt {
    Block(Vec<Stmt>),
    Class {
        name: Ident,
        superclass: Option<Ident>,
        superclass_distance: Cell<Option<usize>>,
        methods: Vec<Function>,
    },
    Expr(Expr),
    Function(Function),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print(Expr),
    Return {
        keyword: kw::keyword_return,
        value: Option<Expr>,
    },
    Variable {
        name: Ident,
        initialiser: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

fn block(input: ParseStream) -> Result<Vec<Stmt>> {
    let mut statements = vec![];

    while !input.is_empty() {
        statements.push(Stmt::declaration(input)?);
    }

    Ok(statements)
}

impl Stmt {
    fn declaration(input: ParseStream) -> Result<Self> {
        if input.peek(kw::keyword_class) {
            Stmt::class_declaration(input)
        } else if input.peek(kw::keyword_fun) {
            let _: kw::keyword_fun = input.parse()?;
            Ok(Stmt::Function(Function::parse(input)?))
        } else if input.peek(kw::keyword_var) {
            Stmt::var_declaration(input)
        } else {
            Stmt::statement(input)
        }
    }

    fn class_declaration(input: ParseStream) -> Result<Self> {
        let _: kw::keyword_class = input.parse()?;
        let name: Ident = input.parse()?;

        let superclass = if input.peek(Punct!["<"]) {
            let _: Punct!["<"] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };

        let content;
        let _: Braces = group!(content in input);
        let methods = parse_repeated(&content)?;

        Ok(Stmt::Class {
            name,
            superclass,
            superclass_distance: Cell::new(None),
            methods,
        })
    }

    fn var_declaration(input: ParseStream) -> Result<Self> {
        let _: kw::keyword_var = input.parse()?;

        let name = kw::ident(input)?;

        let initialiser = if input.peek(Punct!["="]) {
            let _: Punct!["="] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };

        let _: Punct![";"] = input.parse()?;
        Ok(Stmt::Variable { name, initialiser })
    }

    fn statement(input: ParseStream) -> Result<Self> {
        if input.peek(kw::keyword_if) {
            Stmt::if_statement(input)
        } else if input.peek(kw::keyword_for) {
            Stmt::for_statement(input)
        } else if input.peek(kw::keyword_print) {
            Stmt::print_statement(input)
        } else if input.peek(kw::keyword_return) {
            Stmt::return_statement(input)
        } else if input.peek(kw::keyword_while) {
            Stmt::while_statement(input)
        } else if input.peek(Punct!["{"]) {
            let content;
            let _: Braces = group!(content in input);
            Ok(Stmt::Block(block(&content)?))
        } else {
            Stmt::expression_statement(input)
        }
    }

    fn for_statement(input: ParseStream) -> Result<Self> {
        struct ForInner(Option<Stmt>, Expr, Option<Expr>);

        impl Parse for ForInner {
            fn parse(content: ParseStream) -> Result<Self> {
                let initialiser = if content.peek(Punct![";"]) {
                    let _: Punct![";"] = content.parse()?;
                    None
                } else if content.peek(kw::keyword_var) {
                    Some(Stmt::var_declaration(content)?)
                } else {
                    Some(Stmt::expression_statement(content)?)
                };

                let condition = if content.peek(Punct![";"]) {
                    Expr::Literal(Literal::True(kw::keyword_true::new(content)))
                } else {
                    Expr::parse(content)?
                };
                let _: Punct![";"] = content.parse()?;

                let increment = if content.is_empty() {
                    None
                } else {
                    Some(Expr::parse(content)?)
                };

                Ok(ForInner(initialiser, condition, increment))
            }
        }

        let _: kw::keyword_for = input.parse()?;

        let content;
        let _: Parentheses = group!(content in input);

        let initialiser = if content.peek(Punct![";"]) {
            let _: Punct![";"] = content.parse()?;
            None
        } else if content.peek(kw::keyword_var) {
            Some(Stmt::var_declaration(&content)?)
        } else {
            Some(Stmt::expression_statement(&content)?)
        };

        let condition = if content.peek(Punct![";"]) {
            Expr::Literal(Literal::True(kw::keyword_true::new(&content)))
        } else {
            content.parse()?
        };
        let _: Punct![";"] = content.parse()?;

        let increment = if content.is_empty() {
            None
        } else {
            Some(content.parse()?)
        };

        let mut body = Stmt::statement(input)?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }

        body = Stmt::While {
            condition,
            body: Box::new(body),
        };

        if let Some(initialiser) = initialiser {
            body = Stmt::Block(vec![initialiser, body]);
        }

        Ok(body)
    }

    fn if_statement(input: ParseStream) -> Result<Self> {
        let _: kw::keyword_if = input.parse()?;
        let content;
        let _: Parentheses = group!(content in input);
        let condition = content.parse()?;

        let then_branch = Box::new(Stmt::statement(input)?);
        let else_branch = if input.peek(kw::keyword_else) {
            Some(Box::new(Stmt::statement(input)?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(input: ParseStream) -> Result<Self> {
        let _: kw::keyword_print = input.parse()?;
        let value = input.parse()?;
        let _: Punct![";"] = input.parse()?;
        Ok(Self::Print(value))
    }

    fn return_statement(input: ParseStream) -> Result<Self> {
        let keyword: kw::keyword_return = input.parse()?;
        let value = if input.peek(Punct![";"]) {
            None
        } else {
            Some(input.parse()?)
        };
        let _: Punct![";"] = input.parse()?;
        Ok(Stmt::Return { keyword, value })
    }

    fn while_statement(input: ParseStream) -> Result<Self> {
        let _: kw::keyword_while = input.parse()?;
        let content;
        let _: Parentheses = group!(content in input);
        let condition = content.parse()?;
        let body = Box::new(Stmt::statement(input)?);

        Ok(Stmt::While { condition, body })
    }

    fn expression_statement(input: ParseStream) -> Result<Self> {
        let expr = input.parse()?;
        let _: Punct![";"] = input.parse()?;
        Ok(Self::Expr(expr))
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        Stmt::declaration(input)
    }
}

struct Ast(Vec<Stmt>);

impl Ast {
    fn synchronise(input: ParseStream) {
        input.synchronise(|input| {
            use kw::*;
            input.peek(Punct![";"]) && !input.peek2(Punct!["}"])
                || peek2_any!(
                    input,
                    keyword_class,
                    keyword_for,
                    keyword_fun,
                    keyword_if,
                    keyword_print,
                    keyword_return,
                    keyword_var,
                    keyword_while
                )
        });
    }
}

impl Parse for Ast {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut stmts = vec![];

        while !input.is_empty() {
            match input.parse() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    Ast::synchronise(input);
                    input.add_error(err);
                }
            }
        }

        if let Some(error) = input.get_error() {
            Err(error)
        } else {
            Ok(Ast(stmts))
        }
    }
}

fn main() {
    let ast: Ast = pretty_unwrap(parse_string(
        r#"
        var x = 5;
        var y = "hello";
        x = 6;
        y = 4.0;

        fun add(x, y) {
            return x + y;
        }

        print(add(x, y));

        class Cake {
            init(flavour) {
                this.flavour = flavour;
            }

            taste() {
                var adjective = "delicious";
                print "The " + this.flavour + " cake is " + adjective + "!";
            }
        }

        class ChocolateCake < Cake {
            init() {
                this.flavour = "Chocolate";
            }

            taste() {
                super.taste();
                print "Mmm, chocolatey!";
            }
        }

        var cake = ChocolateCake();
        cake.taste();
    "#
        .to_string(),
    ));
    pretty_unwrap(resolver::resolve(&ast));
    pretty_unwrap(interpreter::interpret(ast));
}
