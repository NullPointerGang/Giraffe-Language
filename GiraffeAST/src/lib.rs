use ordered_float::OrderedFloat;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

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
    Block { body: Vec<Statement>, position: Position },
    ForInStatement(ForInStatement),
    TryHandleStatement(TryHandleStatement),
    FunctionCall(FunctionCallStatement),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Literal { value: Literal, position: Position },
    Variable { name: String, position: Position },
    BinaryOperation {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
        position: Position,
    },
    FunctionCall { name: String, args: Vec<Expression>, position: Position },
    List { elements: Vec<Expression>, position: Position },
    Dictionary { entries: Vec<(Literal, Expression)>, position: Position },
    Tuple { elements: Vec<Expression>, position: Position },
    Null(Position),
    MethodCall {
        object: Box<Expression>,
        method: String,
        args: Vec<Expression>,
        position: Position,
    },
    Error { message: String, position: Position },
    Break(Position),
    Continue(Position),
}

// Все структуры данных с позициями
#[derive(Debug, Clone)]
pub struct TryHandleStatement {
    pub try_body: Vec<Statement>,
    pub handle_body: Vec<Statement>,
    pub finally_body: Option<Vec<Statement>>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<VariableDeclaration>,
    pub return_type: String,
    pub body: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub data_type: String,
    pub value: Option<Expression>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub elif: Option<Box<IfStatement>>,
    pub else_body: Option<Vec<Statement>>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: String,
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct ForInStatement {
    pub loop_var: String,
    pub collection: Expression,
    pub body: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct FunctionCallStatement {
    pub name: String,
    pub args: Vec<Expression>,
    pub position: Position,
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

impl AstNode {
    pub fn build_program(statements: Vec<Statement>) -> Self {
        AstNode::Program { statements }
    }
}

impl Statement {
    pub fn position(&self) -> Position {
        match self {
            Statement::FunctionDeclaration(f) => f.position,
            Statement::VariableDeclaration(v) => v.position,
            Statement::IfStatement(i) => i.position,
            Statement::WhileStatement(w) => w.position,
            Statement::PrintStatement(p) => p.position,
            Statement::ReturnStatement(r) => r.position,
            Statement::Assignment(a) => a.position,
            Statement::ExpressionStatement(e) => e.position(),
            Statement::Block { position, .. } => *position,
            Statement::ForInStatement(f) => f.position,
            Statement::TryHandleStatement(t) => t.position,
            Statement::FunctionCall(f) => f.position,
        }
    }

    // Конструкторы
    pub fn function_declaration(
        name: String,
        parameters: Vec<VariableDeclaration>,
        return_type: String,
        body: Vec<Statement>,
        position: Position,
    ) -> Self {
        Statement::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            position,
        })
    }

    pub fn variable_declaration(
        name: String,
        data_type: String,
        value: Option<Expression>,
        position: Position,
    ) -> Self {
        Statement::VariableDeclaration(VariableDeclaration {
            name,
            data_type,
            value,
            position,
        })
    }

    pub fn if_statement(
        condition: Expression,
        body: Vec<Statement>,
        elif: Option<IfStatement>,
        else_body: Option<Vec<Statement>>,
        position: Position,
    ) -> Self {
        Statement::IfStatement(IfStatement {
            condition,
            body,
            elif: elif.map(Box::new),
            else_body,
            position,
        })
    }

    pub fn block(body: Vec<Statement>, position: Position) -> Self {
        Statement::Block { body, position }
    }

    pub fn break_statement(position: Position) -> Self {
        Statement::ExpressionStatement(Expression::Break(position))
    }

    pub fn continue_statement(position: Position) -> Self {
        Statement::ExpressionStatement(Expression::Continue(position))
    }

    pub fn try_handle_statement(
        try_body: Vec<Statement>,
        handle_body: Vec<Statement>,
        finally_body: Option<Vec<Statement>>,
        position: Position,
    ) -> Self {
        Statement::TryHandleStatement(TryHandleStatement {
            try_body,
            handle_body,
            finally_body,
            position,
        })
    }

    pub fn for_in_statement(
        loop_var: String,
        collection: Expression,
        body: Vec<Statement>,
        position: Position,
    ) -> Self {
        Statement::ForInStatement(ForInStatement {
            loop_var,
            collection,
            body,
            position,
        })
    }

    pub fn while_statement(
        condition: Expression,
        body: Vec<Statement>,
        position: Position,
    ) -> Self {
        Statement::WhileStatement(WhileStatement {
            condition,
            body,
            position,
        })
    }

    pub fn return_statement(
        value: Option<Expression>,
        position: Position,
    ) -> Self {
        Statement::ReturnStatement(ReturnStatement { value, position })
    }

    pub fn assignment_statement(
        name: String,
        value: Expression,
        position: Position,
    ) -> Self {
        Statement::Assignment(Assignment { name, value, position })
    }

    pub fn print_statement(value: Expression, position: Position) -> Self {
        Statement::PrintStatement(PrintStatement { value, position })
    }
}

impl Expression {
    pub fn position(&self) -> Position {
        match self {
            Expression::Literal { position, .. } => *position,
            Expression::Variable { position, .. } => *position,
            Expression::BinaryOperation { position, .. } => *position,
            Expression::FunctionCall { position, .. } => *position,
            Expression::List { position, .. } => *position,
            Expression::Dictionary { position, .. } => *position,
            Expression::Tuple { position, .. } => *position,
            Expression::Null(pos) => *pos,
            Expression::MethodCall { position, .. } => *position,
            Expression::Error { position, .. } => *position,
            Expression::Break(pos) => *pos,
            Expression::Continue(pos) => *pos,
        }
    }

    // Конструкторы
    pub fn literal(value: Literal, position: Position) -> Self {
        Expression::Literal { value, position }
    }

    pub fn variable(name: String, position: Position) -> Self {
        Expression::Variable { name, position }
    }

    pub fn binary_operation(
        left: Expression,
        op: Operator,
        right: Expression,
        position: Position,
    ) -> Self {
        Expression::BinaryOperation {
            left: Box::new(left),
            op,
            right: Box::new(right),
            position,
        }
    }

    pub fn function_call(name: String, args: Vec<Expression>, position: Position) -> Self {
        Expression::FunctionCall { name, args, position }
    }

    pub fn null(position: Position) -> Self {
        Expression::Null(position)
    }
}
