use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}


#[derive(Debug, Clone)]
pub enum AstNode {
    Program { statements: Vec<Statement> },
    Statement(Statement),
    Expression(Expression),
}


#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration, Position),
    VariableDeclaration(VariableDeclaration, Position),
    IfStatement(IfStatement, Position),
    WhileStatement(WhileStatement, Position),
    PrintStatement(PrintStatement, Position),
    ReturnStatement(ReturnStatement, Position),
    Assignment(Assignment, Position),
    ExpressionStatement(Expression, Position),
    Block(Vec<Statement>, Position),
    ForInStatement(String, Expression, Vec<Statement>, Position),
    TryHandleStatement(TryHandleStatement, Position),
    FunctionCall(String, Vec<Expression>, Position)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Literal(Literal, Position),
    Variable(String, Position),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>, Position),
    FunctionCall(String, Vec<Expression>, Position),
    List(Vec<Expression>, Position),
    Dictionary(Vec<(Literal, Expression)>, Position),
    Tuple(Vec<Expression>, Position),
    Null(Position),

    // Сейчас нет нужды в MemberAccess
    // MemberAccess(Box<Expression>, String), 
    
    MethodCall(Box<Expression>, String, Vec<Expression>, Position),
    Error(String, Position),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct TryHandleStatement {
    pub try_body: Vec<Statement>,
    pub handle_body: Vec<Statement>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    GreaterThanOrEqual,
    LessThanOrEqual,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    Float(OrderedFloat<f64>),
    Boolean(bool),
    String(String),
    List(Vec<Literal>),
    Dictionary(Vec<(Literal, Expression)>),
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
    pub fn function_declaration(name: String, parameters: Vec<VariableDeclaration>, return_type: String, body: Vec<Statement>, position: Position) -> Self {
        Statement::FunctionDeclaration(FunctionDeclaration { name, parameters, return_type, body }, position)
    }

    pub fn variable_declaration(name: String, data_type: String, value: Option<Expression>, position: Position) -> Self {
        Statement::VariableDeclaration(VariableDeclaration { name, data_type, value }, position)
    }

    pub fn if_statement(condition: Expression, body: Vec<Statement>, elif: Option<IfStatement>, else_body: Option<Vec<Statement>>, position: Position) -> Self {
        Statement::IfStatement(IfStatement { 
            condition, 
            body, 
            elif: elif.map(Box::new),
            else_body 
        }, position)
    }    

    pub fn while_statement(condition: Expression, body: Vec<Statement>, position: Position) -> Self {
        Statement::WhileStatement(WhileStatement { condition, body }, position)
    }

    pub fn print_statement(value: Expression, position: Position) -> Self {
        Statement::PrintStatement(PrintStatement { value }, position)
    }

    pub fn return_statement(value: Option<Expression>, position: Position) -> Self {
        Statement::ReturnStatement(ReturnStatement { value }, position)
    }

    pub fn assignment(name: String, value: Expression, position: Position) -> Self {
        Statement::Assignment(Assignment { name, value }, position)
    }

    pub fn for_in_statement(loop_var: String, collection: Expression, body: Vec<Statement>, position: Position) -> Self {
        Statement::ForInStatement(loop_var, collection, body, position)
    }

    pub fn try_handle_statement(try_handle_statement: TryHandleStatement, position: Position) -> Self {
        Statement::TryHandleStatement(try_handle_statement, position)
    }

    pub fn break_statement(position: Position) -> Self {
        Statement::ExpressionStatement(Expression::Break, position)
    }

    pub fn continue_statement(position: Position) -> Self {
        Statement::ExpressionStatement(Expression::Continue, position)
    }

}

impl Expression {
    pub fn literal(literal: Literal, position: Position) -> Self {
        Expression::Literal(literal, position)
    }

    pub fn variable(name: String, position: Position) -> Self {
        Expression::Variable(name, position)
    }

    pub fn binary_operation(left: Expression, op: Operator, right: Expression, position: Position) -> Self {
        Expression::BinaryOperation(Box::new(left), op, Box::new(right), position)
    }

    pub fn function_call(name: String, args: Vec<Expression>, position: Position) -> Self {
        Expression::FunctionCall(name, args, position)
    }

    pub fn list(elements: Vec<Expression>, position: Position) -> Self {
        Expression::List(elements, position)
    }

    pub fn dictionary<'a>(dictionary: &'a Vec<(Literal, Expression)>, key: &'a Literal) -> Option<&'a Expression> {
        for (k, v) in dictionary.iter() {
            if k == key {
                return Some(v);
            }
        }
        None
    }
    pub fn null(position: Position) -> Self {
        Expression::Null(position)
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

    pub fn greater_than_or_equal() -> Self {
        Operator::GreaterThanOrEqual
    }

    pub fn less_than_or_equal() -> Self {
        Operator::LessThanOrEqual
    }
}
