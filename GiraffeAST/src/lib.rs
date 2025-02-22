use std::collections::HashMap;
use ordered_float::OrderedFloat;


#[derive(Debug, Clone)]
pub enum AstNode {
    Program { statements: Vec<Statement> },
    Statement(Statement),
    Expression(Expression),
}


#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    Assignment(Assignment),
    ExpressionStatement(Expression),
    Block(Vec<Statement>),
    ForInStatement(String, Expression, Vec<Statement>),
    TryHandleStatement(TryHandleStatement),
    FunctionCall(String, Vec<Expression>)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    List(Vec<Expression>),
    Dictionary(HashMap<Literal, Expression>),
    Tuple(Vec<Expression>),
    Null,
    MemberAccess(Box<Expression>, String),
    MethodCall(Box<Expression>, String, Vec<Expression>),
    Error(String),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct TryHandleStatement {
    pub try_body: Vec<Statement>,
    pub catch_body: Vec<Statement>,
    pub finally_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<VariableDeclaration>,
    pub return_type: String,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub data_type: String,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub elif: Option<Box<IfStatement>>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    Float(OrderedFloat<f64>),
    Boolean(bool),
    String(String),
    List(Vec<Literal>),
    Dictionary(Vec<(Literal, Literal)>),
    Tuple(Vec<Literal>),
    Null,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    COMMENT_MULTILINE,
    COMMENT_SINGLELINE,
    KEYWORD,
    IDENTIFIER,
    INTEGER,
    FLOAT,
    STRING,
    BOOLEAN,
    LIST,
    DICT,
    SET,
    TUPLE,
    NULL,
    OPTION,
    ERROR,
    FUNCTION,
    OPERATOR,
    SYMBOL,
    BRACKET,
    PUNCTUATION,
    PRINT,
    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

impl AstNode {
    pub fn build_program(statements: Vec<Statement>) -> Self {
        AstNode::Program { statements }
    }
}

impl Statement {
    pub fn function_declaration(name: String, parameters: Vec<VariableDeclaration>, return_type: String, body: Vec<Statement>) -> Self {
        Statement::FunctionDeclaration(FunctionDeclaration { name, parameters, return_type, body })
    }

    pub fn variable_declaration(name: String, data_type: String, value: Option<Expression>) -> Self {
        Statement::VariableDeclaration(VariableDeclaration { name, data_type, value })
    }

    pub fn if_statement(condition: Expression, body: Vec<Statement>, elif: Option<IfStatement>, else_body: Option<Vec<Statement>>) -> Self {
        Statement::IfStatement(IfStatement { 
            condition, 
            body, 
            elif: elif.map(Box::new),
            else_body 
        })
    }    

    pub fn while_statement(condition: Expression, body: Vec<Statement>) -> Self {
        Statement::WhileStatement(WhileStatement { condition, body })
    }

    pub fn print_statement(value: Expression) -> Self {
        Statement::PrintStatement(PrintStatement { value })
    }

    pub fn return_statement(value: Option<Expression>) -> Self {
        Statement::ReturnStatement(ReturnStatement { value })
    }

    pub fn assignment(name: String, value: Expression) -> Self {
        Statement::Assignment(Assignment { name, value })
    }

    pub fn for_in_statement(loop_var: String, collection: Expression, body: Vec<Statement>) -> Self {
        Statement::ForInStatement(loop_var, collection, body)
    }

    pub fn try_handle_statement(try_handle_statement: TryHandleStatement) -> Self {
        Statement::TryHandleStatement(try_handle_statement)
    }

    pub fn break_statement() -> Self {
        Statement::ExpressionStatement(Expression::Break)
    }

    pub fn continue_statement() -> Self {
        Statement::ExpressionStatement(Expression::Continue)
    }

}

impl Expression {
    pub fn literal(literal: Literal) -> Self {
        Expression::Literal(literal)
    }

    pub fn variable(name: String) -> Self {
        Expression::Variable(name)
    }

    pub fn binary_operation(left: Expression, op: Operator, right: Expression) -> Self {
        Expression::BinaryOperation(Box::new(left), op, Box::new(right))
    }

    pub fn function_call(name: String, args: Vec<Expression>) -> Self {
        Expression::FunctionCall(name, args)
    }

    pub fn list(elements: Vec<Expression>) -> Self {
        Expression::List(elements)
    }

    pub fn dictionary(pairs: HashMap<Literal, Expression>) -> Self {
        Expression::Dictionary(pairs)
    }

    pub fn null() -> Self {
        Expression::Null
    }
}

impl Operator {
    pub fn add() -> Self {
        Operator::Add
    }

    pub fn subtract() -> Self {
        Operator::Subtract
    }

    pub fn multiply() -> Self {
        Operator::Multiply
    }

    pub fn divide() -> Self {
        Operator::Divide
    }

    pub fn greater_than() -> Self {
        Operator::GreaterThan
    }

    pub fn less_than() -> Self {
        Operator::LessThan
    }

    pub fn equal() -> Self {
        Operator::Equal
    }

    pub fn not_equal() -> Self {
        Operator::NotEqual
    }

    pub fn and() -> Self {
        Operator::And
    }

    pub fn or() -> Self {
        Operator::Or
    }
}
