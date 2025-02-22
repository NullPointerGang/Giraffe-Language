use ordered_float::OrderedFloat;
use GiraffeAST::*;
use std::collections::{self, HashMap};

#[derive(Clone, Debug)]
struct Context {
    variables: HashMap<String, Literal>,
    functions: HashMap<String, FunctionDeclaration>,
}

impl Context {
    fn new() -> Self {
        Context {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn set_variable(&mut self, name: String, value: Literal) {
        self.variables.insert(name, value);
    }

    fn get_variable(&self, name: &str) -> Option<&Literal> {
        self.variables.get(name)
    }

    fn set_function(&mut self, name: String, func: FunctionDeclaration) {
        self.functions.insert(name, func);
    }

    fn get_function(&self, name: &str) -> Option<&FunctionDeclaration> {
        self.functions.get(name)
    }
}

pub struct Interpreter {
    pub global_context: Context,
    pub call_stack: Vec<Context>, // стек локальных контекстов
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_context: Context::new(),
            call_stack: Vec::new(),
        }
    }

    /// Возвращает текущий контекст – либо верхушку стека (если функция выполняется),
    /// либо глобальный контекст, если стек пуст.
    fn current_context(&mut self) -> &mut Context {
        if let Some(ctx) = self.call_stack.last_mut() {
            ctx
        } else {
            &mut self.global_context
        }
    }

    pub fn evaluate_expression(&mut self, expr: Expression) -> Literal {
        match expr {
            Expression::Variable(name) => {
                // Сначала ищем в текущем контексте (если функция выполняется)
                if let Some(value) = self.current_context().get_variable(&name) {
                    value.clone()
                } else if let Some(value) = self.global_context.get_variable(&name) {
                    value.clone()
                } else {
                    println!("Error: Variable '{}' is not defined.", name);
                    Literal::Null
                }
            },
            Expression::FunctionCall(name, args) => {
                let evaluated_args: Vec<Literal> = args
                    .into_iter()
                    .map(|arg| self.evaluate_expression(arg))
                    .collect();
                self.call_function(name, evaluated_args)
            },
            Expression::BinaryOperation(left, op, right) => {
                let left_val = self.evaluate_expression(*left);
                let right_val = self.evaluate_expression(*right);
                self.apply_operator(op, left_val, right_val)
            },
            Expression::List(elements) => {
                let mut result = String::new();
                for element in elements {
                    let evaluated_value = self.evaluate_expression(element);
                    match evaluated_value {
                        Literal::String(s) => result.push_str(&s),
                        Literal::Integer(i) => result.push_str(&i.to_string()),
                        Literal::Float(f) => result.push_str(&f.to_string()),
                        Literal::Boolean(b) => result.push_str(&b.to_string()),
                        Literal::Null => result.push_str("null"),
                        Literal::List(v) => result.push_str(&format!("[{}]", v.iter().map(|item| format!("{:?}", item)).collect::<Vec<String>>().join(", "))),
                        Literal::Dictionary(d) => {
                            result.push_str(&format!("{{{}}}", d.iter().map(|(k, v)| format!("{:?}: {:?}", k, v)).collect::<Vec<String>>().join(", ")))
                        }
                        Literal::Tuple(t) => result.push_str(&format!("({})", t.iter().map(|item| format!("{:?}", item)).collect::<Vec<String>>().join(", "))),
                    }
                }
                Literal::String(result)
            }            
            Expression::Dictionary(elements) => {
                let result = elements.into_iter()
                .map(|(key, value)| {
                    (self.evaluate_expression(GiraffeAST::Expression::Literal(key)), self.evaluate_expression(value))
                })
                .collect();
                println!("evaluate_expression {:?}", result);
                Literal::Dictionary(result)
            },
            Expression::Tuple(elements) => {
                let mut result = Vec::new();
                for element in elements {
                    let evaluated_value = self.evaluate_expression(element);
                    result.push(evaluated_value);
                }
                Literal::Tuple(result)
            },
            Expression::Literal(literal) => literal,
            other => {
                println!("Unhandled Expression variant: {:?}", other);
                Literal::Null
            }
        }
    }

    /// Функция: создание нового контекста, пуш в стек, выполнение тела и pop после завершения.
    pub fn call_function(&mut self, name: String, args: Vec<Literal>) -> Literal {
        if let Some(func_decl) = self.global_context.get_function(&name).cloned() {
            // Создаём контекст для вызова функции и связываем параметры с аргументами
            let mut func_context = Context::new();
            for (param, arg) in func_decl.parameters.iter().zip(args.iter()) {
                func_context.set_variable(param.name.clone(), arg.clone());
            }
            // Пушим контекст функции в стек вызовов
            self.call_stack.push(func_context);

            let mut return_value = Literal::Null;
            for statement in func_decl.body {
                match statement {
                    Statement::ReturnStatement(return_stmt) => {
                        if let Some(return_expr) = return_stmt.value {
                            return_value = self.evaluate_expression(return_expr);
                            break;
                        }
                    }
                    _ => {
                        self.execute_statement(statement);
                    }
                }
            }
            // По завершении функции удаляем контекст из стека
            self.call_stack.pop();
            return_value
        } else {
            println!("Error: Function '{}' is not defined.", name);
            Literal::Null
        }
    }

    pub fn apply_operator(&self, op: Operator, left: Literal, right: Literal) -> Literal {
        match op {
            Operator::Add => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l + r),
                (Literal::Integer(l), Literal::Float(r)) => Literal::Float(OrderedFloat::from(l as f64 + *r)),
                (Literal::Float(l), Literal::Integer(r)) => Literal::Float(l + r as f64),
                (Literal::Float(l), Literal::Float(r)) => Literal::Float(l + r),
                (Literal::String(mut l), Literal::String(r)) => {
                    l.push_str(&r);
                    Literal::String(l)
                },
                (Literal::Integer(l), Literal::String(r)) => {
                    let result = l.to_string() + &r;
                    Literal::String(result)
                },
                (Literal::String(l), Literal::Integer(r)) => {
                    let result = l + &r.to_string();
                    Literal::String(result)
                },
                _ => {
                    println!(
                        "Error: Unsupported types for operator '+' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::Subtract => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l - r),
                (Literal::Integer(l), Literal::Float(r)) => Literal::Float(OrderedFloat::from(l as f64 - *r)),
                (Literal::Float(l), Literal::Integer(r)) => Literal::Float(l - r as f64),
                (Literal::Float(l), Literal::Float(r)) => Literal::Float(l - r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '-' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::Multiply => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l * r),
                (Literal::Integer(l), Literal::Float(r)) => Literal::Float(OrderedFloat::from(l as f64 * *r)),
                (Literal::Float(l), Literal::Integer(r)) => Literal::Float(l * r as f64),
                (Literal::Float(l), Literal::Float(r)) => Literal::Float(l * r),
                (Literal::String(l), Literal::Integer(r)) => {
                    let mut result = String::new();
                    for _ in 0..r {
                        result.push_str(&l);
                    }
                    Literal::String(result)
                },
                _ => {
                    println!(
                        "Error: Unsupported types for operator '*' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::Divide => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l / r),
                (Literal::Integer(l), Literal::Float(r)) => Literal::Float(OrderedFloat(l as f64 / *r)),
                (Literal::Float(l), Literal::Integer(r)) => Literal::Float(l / r as f64),
                (Literal::Float(l), Literal::Float(r)) => Literal::Float(l / r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '/' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::GreaterThan => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Boolean(l > r),
                (Literal::Float(l), Literal::Float(r)) => Literal::Boolean(l > r),
                (Literal::String(l), Literal::String(r)) => Literal::Boolean(l > r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '>' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::LessThan => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Boolean(l < r),
                (Literal::Float(l), Literal::Float(r)) => Literal::Boolean(l < r),
                (Literal::String(l), Literal::String(r)) => Literal::Boolean(l < r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '<' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::And => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Boolean(l), Literal::Boolean(r)) => Literal::Boolean(l && r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator 'and' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::Or => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Boolean(l), Literal::Boolean(r)) => Literal::Boolean(l || r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator 'or' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::Equal => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Boolean(l == r),
                (Literal::Float(l), Literal::Float(r)) => Literal::Boolean(l == r),
                (Literal::String(l), Literal::String(r)) => Literal::Boolean(l == r),
                (Literal::Boolean(l), Literal::Boolean(r)) => Literal::Boolean(l == r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '==' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            Operator::NotEqual => match (left.clone(), right.clone()) {
                (Literal::Null, _) | (_, Literal::Null) => Literal::Null,
                (Literal::Integer(l), Literal::Integer(r)) => Literal::Boolean(l != r),
                (Literal::Float(l), Literal::Float(r)) => Literal::Boolean(l != r),
                (Literal::String(l), Literal::String(r)) => Literal::Boolean(l != r),
                (Literal::Boolean(l), Literal::Boolean(r)) => Literal::Boolean(l != r),
                _ => {
                    println!(
                        "Error: Unsupported types for operator '!=' (left: {:?}, right: {:?})",
                        left, right
                    );
                    Literal::Null
                }
            },
            _ => {
                println!("Unhandled Operator variant: {:?}", op);
                Literal::Null
            }
        }
    }

    pub fn execute_statement(&mut self, stmt: Statement) -> Literal {
        // println!("execute_statement {:?}", stmt);
        match stmt {
            Statement::FunctionDeclaration(func_decl) => {
                // Функция регистрируется в текущем (или глобальном) контексте
                self.current_context().set_function(func_decl.name.clone(), func_decl);
                Literal::Null
            }
            Statement::VariableDeclaration(var_decl) => {
                let value = if let Some(expr) = var_decl.value {
                    self.evaluate_expression(expr)
                } else {
                    Literal::Null
                };
                self.current_context().set_variable(var_decl.name.clone(), value);
                Literal::Null
            }
            Statement::IfStatement(if_stmt) => {
                let condition_value = self.evaluate_expression(if_stmt.condition);
                if self.is_truthy(condition_value) {
                    for statement in if_stmt.body {
                        self.execute_statement(statement);
                    }
                } else if let Some(else_body) = if_stmt.else_body {
                    for statement in else_body {
                        self.execute_statement(statement);
                    }
                }
                Literal::Null
            }
            Statement::WhileStatement(while_stmt) => {
                let condition = while_stmt.condition.clone();
                let value = self.evaluate_expression(condition);
                while self.is_truthy(value.clone()) {
                    for statement in while_stmt.body.clone() {
                        self.execute_statement(statement);
                    }
                }
                Literal::Null
            }
            Statement::PrintStatement(print_stmt) => {
                let value = self.evaluate_expression(print_stmt.value);
                // println!("Call stack: {:?}", self.call_stack.clone());
                // println!("Current context: {:?}", self.current_context().clone());
                println!("{:?}", value);
                Literal::Null
            }
            Statement::ReturnStatement(return_stmt) => {
                if let Some(expr) = return_stmt.value {
                    return self.evaluate_expression(expr);
                }
                Literal::Null
            }
            Statement::Assignment(assign) => {
                let value = self.evaluate_expression(assign.value);
                // Сначала пытаемся обновить в текущем контексте,
                // если переменная не найдена – пробуем в глобальном
                if self.current_context().get_variable(&assign.name).is_some() {
                    self.current_context().set_variable(assign.name, value);
                } else if self.global_context.get_variable(&assign.name).is_some() {
                    self.global_context.set_variable(assign.name, value);
                } else {
                    println!("Error: Variable '{}' is not defined.", assign.name);
                }
                Literal::Null
            }
            Statement::ExpressionStatement(expr) => {
                self.evaluate_expression(expr);
                Literal::Null
            }
            Statement::ForInStatement(loop_var, collection , body) => {
                let collection_value = self.evaluate_expression(collection);
                println!("Collection Value: {:?}", collection_value);
                match collection_value {
                    Literal::String(s) => {
                        println!("String in For-In");
                        for char in s.chars() {
                            self.current_context().set_variable(loop_var.clone(), Literal::String(char.to_string()));
                            for statement in body.clone() {
                                self.execute_statement(statement);
                            }
                        }
                    }
                    Literal::List(l) => {
                        println!("List in For-In");
                        for item in l {
                            self.current_context().set_variable(loop_var.clone(), item);
                            for statement in body.clone() {
                                self.execute_statement(statement);
                            }
                        }
                    }
                    Literal::Dictionary(d) => {
                        println!("Dictionary in For-In");
                        for (key, value) in d {
                            self.current_context().set_variable(loop_var.clone(), value);
                            for statement in body.clone() {
                                self.execute_statement(statement);
                            }
                        }
                    }
                    _ => {
                        println!("Error: Cannot iterate over non-string and non-list values.");
                    }
                }
                Literal::Null
            }
            _ => {
                println!("Unhandled Statement variant: {:?}", stmt);
                Literal::Null
            }
        }
    }

    fn is_truthy(&self, value: Literal) -> bool {
        match value {
            Literal::Boolean(b) => b,
            Literal::Null => false,
            _ => true,
        }
    }

    pub fn interpret_program(&mut self, node: AstNode) -> Literal {
        match node {
            AstNode::Program { statements } => {
                let mut result = Literal::Null;
                for statement in statements {
                    result = self.execute_statement(statement);
                }
                result
            }
            AstNode::Statement(statement) => self.execute_statement(statement),
            AstNode::Expression(expression) => self.evaluate_expression(expression),
        }
    }
}
