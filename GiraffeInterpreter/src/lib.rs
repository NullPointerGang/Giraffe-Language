use ordered_float::OrderedFloat;
use GiraffeAST::*;
use std::collections::HashMap;
use std::fmt::{self, format};
use colored::*;
use std::fs;


pub fn print_error(title: &str, details: &[(&str, String)], filename: &str, position: &Position) {
    let absolute_path = match fs::canonicalize(filename) {
        Ok(path) => path.display().to_string(),
        Err(_) => filename.to_string(),
    };

    eprintln!("{}", format!("error: {}", title).red().bold());
    
    eprintln!(" --> {}:{}:{}", absolute_path, position.line, position.column);
    eprintln!("  |");
    
    for (label, value) in details {
        eprintln!("{} {}", label.green().bold(), value);
    }

    eprintln!("  |");
    eprintln!("  |                 ^");
    eprintln!();
}


pub struct LocalLiteral(pub GiraffeAST::Literal, pub Position);


impl fmt::Display for LocalLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            GiraffeAST::Literal::Null => write!(f, "null"),
            GiraffeAST::Literal::Integer(i) => write!(f, "{}", i),
            GiraffeAST::Literal::Float(fl) => write!(f, "{}", fl),
            GiraffeAST::Literal::Boolean(b) => write!(f, "{}", b),
            GiraffeAST::Literal::String(s) => write!(f, "{}", s),
            GiraffeAST::Literal::List(lst) => {
                let elements: Vec<String> = lst
                    .iter()
                    .map(|item| format!("{}", LocalLiteral(item.clone(), self.1.clone())))
                    .collect();
                write!(f, r#"["{}"]"#, elements.join(", "))
            },
            GiraffeAST::Literal::Dictionary(dict) => {
                let elements: Vec<String> = dict
                    .iter()
                    .map(|(key, value_expr)| {
                        let value_str = match value_expr {
                            GiraffeAST::Expression::Literal(lit, pos) => {
                                format!("{}", LocalLiteral(lit.clone(), pos.clone()))
                            }
                            _ => format!("{:?}", value_expr),
                        };
                        format!(r#""{}": "{}""#, LocalLiteral(key.clone(), self.1.clone()), value_str)
                    })
                    .collect();
                write!(f, "{{{}}}", elements.join(", "))
            },
            GiraffeAST::Literal::Tuple(tpl) => {
                let elements: Vec<String> = tpl
                    .iter()
                    .map(|item| format!("{}", LocalLiteral(item.clone(), self.1.clone())))
                    .collect();
                write!(f, "({})", elements.join(", "))
            },
        }
    }
}

#[derive(Debug)]
pub enum InterpreterError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    RuntimeError(String),
    Break,              // Спец. сигнал для break
    Continue,           // Спец. сигнал для continue
    Return(Literal),    // Спец. сигнал для return

    // TODO: Добавить другие виды ошибок (например, синтаксические, типовые и тд.)
}

#[derive(Debug)]
pub enum InterpreterResult<T> {
    Ok(T),
    Err(InterpreterError),
}

pub trait StateStore: Clone {
    fn set_variable(&mut self, name: &str, value: Literal);
    fn get_variable(&self, name: &str) -> Option<Literal>;
    fn set_function(&mut self, name: &str, function: Function);
    fn get_function(&self, name: &str) -> Option<&Function>;
    fn snapshot(&self) -> Self;
}

#[derive(Clone, Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Context {
    variables: HashMap<String, Literal>,
    functions: HashMap<String, Function>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

impl StateStore for Context {
    // Переменные
    fn set_variable(&mut self, name: &str, value: Literal) {
        self.variables.insert(name.to_string(), value);
    }

    fn get_variable(&self, name: &str) -> Option<Literal> {
        self.variables.get(name).cloned()
    }

    // Функции
    fn set_function(&mut self, name: &str, function: Function) {
        self.functions.insert(name.to_string(), function);
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    fn snapshot(&self) -> Self {
        self.clone() // Для простоты используем clone. В будущем сделать более эффективный механизм!
    }
}


/// Интерпретатор, параметризованный типом хранилища состояния.
/// Благодаря обобщённости можно в будущем использовать более умное хранилище или снапшоты.
pub struct Interpreter<S: StateStore> {
    global_state: S,
    call_stack: Vec<S>,
    break_signal: bool,
    continue_signal: bool,
}

impl<S: StateStore + std::fmt::Debug> Interpreter<S> {
    pub fn new(global_state: S) -> Self {
        Interpreter {
            call_stack: Vec::new(),
            global_state,
            break_signal: false,
            continue_signal: false,
        }
    }

    // Выполнение выражения
    pub fn execute_expression(&mut self, expr: Expression) -> InterpreterResult<Literal> {
        match expr {
            Expression::Literal(literal, _pos) => {
                // Здесь позиция не влияет на вычисление, поэтому просто возвращаем литерал.
                InterpreterResult::Ok(literal)
            },
            Expression::Variable(name, _pos) => {
                match self.global_state.get_variable(&name) {
                    Some(value) => InterpreterResult::Ok(value),
                    None => InterpreterResult::Err(InterpreterError::UndefinedVariable(name)),
                }
            },
            Expression::List(list, _pos) => {
                let mut evaluated_list = Vec::new();
                for item in list {
                    match self.execute_expression(item) {
                        InterpreterResult::Ok(value) => evaluated_list.push(value),
                        InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    }
                }
                InterpreterResult::Ok(Literal::List(evaluated_list))
            },
            Expression::BinaryOperation(left_expr, op, right_expr, _pos) => {
                let left = match self.execute_expression(*left_expr) {
                    InterpreterResult::Ok(val) => val,
                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                };
                let right = match self.execute_expression(*right_expr) {
                    InterpreterResult::Ok(val) => val,
                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                };
                InterpreterBinaryOperation::execute(left, op, right)
            },
            Expression::Break => {
                InterpreterResult::Err(InterpreterError::Break)
            },
            Expression::Continue => {
                InterpreterResult::Err(InterpreterError::Continue)
            },
            Expression::Null(_pos) => {
                InterpreterResult::Ok(Literal::Null)
            },
            Expression::FunctionCall(func_name, func_arguments, _pos) => {
                let mut evaluated_arguments = Vec::new();
                for arg in func_arguments {
                    match self.execute_expression(arg) {
                        InterpreterResult::Ok(value) => evaluated_arguments.push(value),
                        InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    }
                }
                match self.global_state.clone().get_function(&func_name) {
                    Some(func) => {
                        let mut local_context = self.global_state.snapshot();
                        for (param, arg_value) in func.params.iter().zip(evaluated_arguments.into_iter()) {
                            local_context.set_variable(param, arg_value);
                        }
                        let saved_state = self.global_state.snapshot();
                        let func_body = self.global_state.get_function(&func_name).map(|f| f.body.clone()); 
                        self.global_state = local_context;
            
                        let mut return_value: Option<Literal> = None;
                        if let Some(stmt) = func_body {
                            // Передаем позицию (хотя здесь она может быть несущественна)
                            match self.execute_statement(GiraffeAST::Statement::Block(stmt, _pos.clone())) {
                                InterpreterResult::Ok(()) => {},
                                InterpreterResult::Err(InterpreterError::Return(val)) => {
                                    return_value = Some(val);
                                },
                                InterpreterResult::Err(e) => {
                                    self.global_state = saved_state;
                                    return InterpreterResult::Err(e);
                                }
                            }
                        }
                        self.global_state = saved_state;
                        match return_value {
                            Some(value) => InterpreterResult::Ok(value),
                            None => InterpreterResult::Ok(Literal::Null),
                        }
                    },
                    None => InterpreterResult::Err(
                        InterpreterError::UndefinedFunction(format!("in expression {}", func_name))
                    ),
                }
            },
            Expression::Dictionary(dictionary_expression, _pos) => {
                let mut evaluated_dictionary = Vec::new();
                for (key, value) in dictionary_expression {
                    // Здесь ключ — это литерал. Оборачиваем его в Expression::Literal, добавляя позицию,
                    // чтобы использовать execute_expression. Можно использовать _pos как fallback.
                    let evaluated_key = match self.execute_expression(
                        GiraffeAST::Expression::Literal(key, _pos.clone())
                    ) {
                        InterpreterResult::Ok(val) => val,
                        InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    };
        
                    let evaluated_value = match self.execute_expression(value) {
                        InterpreterResult::Ok(val) => val,
                        InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    };
        
                    evaluated_dictionary.push((evaluated_key, GiraffeAST::Expression::Literal(evaluated_value, _pos.clone())));
                }
                InterpreterResult::Ok(Literal::Dictionary(evaluated_dictionary))
            },
            Expression::MethodCall(access, name, args, _pos) => {
                let maybe_var_name = match *access {
                    Expression::Variable(ref var_name, _) => Some(var_name.clone()),
                    _ => None,
                };
        
                let object_literal = match self.execute_expression(*access) {
                    InterpreterResult::Ok(val) => val,
                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                };
        
                let mut evaluated_arguments = Vec::new();
                for arg in args {
                    match self.execute_expression(arg) {
                        InterpreterResult::Ok(val) => evaluated_arguments.push(val),
                        InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    }
                }
        
                let result = InterpreterObjectMethod::execute(object_literal, name, evaluated_arguments);
        
                if let (Some(var_name), InterpreterResult::Ok(ref updated_object)) = (maybe_var_name, &result) {
                    self.global_state.set_variable(&var_name, updated_object.clone());
                }
        
                result
            },
            _ => InterpreterResult::Err(
                InterpreterError::RuntimeError(format!("Unsupported expression: {:?}", expr))
            ),
        }
    }
    

    // Выполнение инструкций
    pub fn execute_statement(&mut self, statement: Statement) -> InterpreterResult<()> {
        match statement {
            Statement::PrintStatement(print_statement, pos) => {
                let value = print_statement.value.clone();
                match self.execute_expression(value) {
                    InterpreterResult::Ok(Literal::List(lst)) => {
                        let mut output = String::new();
                        for item in lst {
                            if let Literal::String(s) = item {
                                output.push_str(&s);
                            } else {
                                output.push_str(&LocalLiteral(item.clone(), pos.clone()).to_string());
                            }
                        }
                        println!("{}", output);
                        InterpreterResult::Ok(())
                    },
                    InterpreterResult::Ok(result) => {
                        println!("{:?}", result);
                        InterpreterResult::Ok(())
                    },
                    InterpreterResult::Err(e) => {
                        print_error(
                            &format!("Error in print statement: {:?}", e),
                            &[(&format!("{:?}", print_statement.value).to_string(), "".to_string())],
                            "",
                            &pos,
                        );                        
                        InterpreterResult::Err(e)
                    }
                }
            },     
            Statement::VariableDeclaration(var_decl, pos) => {
                let var_name = var_decl.name;
                let var_value = var_decl.value;

                match self.execute_expression(var_value.expect(&format!("Variable value should not be None {}", var_name))) {
                    InterpreterResult::Ok(value) => {
                        self.global_state.set_variable(&var_name, value);
                        InterpreterResult::Ok(())
                    }
                    InterpreterResult::Err(e) => InterpreterResult::Err(e),
                }                
            },
            Statement::FunctionCall(name, args, pos) => {
                if let Some(func) = self.global_state.get_function(&name) {
                    let func = func.clone();
    
                    let local_state = self.global_state.snapshot();
                    let mut local_context = Context::new();
                    
                    for stmt in func.body.clone() {
                        match self.execute_statement(stmt.clone()) {
                            InterpreterResult::Ok(_) => {}
                            InterpreterResult::Err(e) => {
                                println!("Error in function '{}': {:?}", name, e);
                                return InterpreterResult::Err(e);
                            }
                        }
                    }
                    
                    for (param, arg) in func.params.iter().zip(args.iter()) {
                        if let InterpreterResult::Ok(value) = self.execute_expression(arg.clone()) {
                            local_context.set_variable(&param.clone(), value.clone());
                        }
                    }
    
                    for (var_name, var_value) in local_context.variables {
                        self.global_state.set_variable(&var_name, var_value);
                    }
    
                    self.global_state = local_state;
                    InterpreterResult::Ok(())
                } 
                else {
                    InterpreterResult::Err(InterpreterError::UndefinedFunction(name))
                }
            },

            Statement::IfStatement(if_stmt, pos) => {
                let condition_result = self.execute_expression(if_stmt.condition);
                match condition_result {
                    InterpreterResult::Ok(Literal::Boolean(true)) => {
                        for stmt in &if_stmt.body {
                            match self.execute_statement(stmt.clone()) {
                                InterpreterResult::Ok(()) => {},
                                InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                            }
                        }
                        return InterpreterResult::Ok(());
                    },
                    InterpreterResult::Ok(Literal::Boolean(false)) => {
                        if let Some(elif) = if_stmt.elif {
                            match self.execute_statement(Statement::IfStatement(*elif, pos)) {
                                InterpreterResult::Ok(()) => {},
                                InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                            }
                        } else if let Some(else_body) = if_stmt.else_body {
                            for stmt in else_body {
                                match self.execute_statement(stmt) {
                                    InterpreterResult::Ok(()) => {},
                                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                                }
                            }
                        }
                        return InterpreterResult::Ok(());
                    },
                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                    _ => return InterpreterResult::Err(InterpreterError::RuntimeError("Condition must evaluate to boolean".into())),
                }
            },
            Statement::Assignment(assignment_statement, pos) => {
                let name = assignment_statement.name;
                let value = assignment_statement.value;
                match self.execute_expression(value) {
                    InterpreterResult::Ok(value) => {
                        self.global_state.set_variable(&name, value);
                        InterpreterResult::Ok(())
                    }
                    InterpreterResult::Err(e) => InterpreterResult::Err(e),
                }
            },
            Statement::WhileStatement(while_statement, pos) => {
                let mut condition = self.execute_expression(while_statement.condition.clone());
                println!("{:#?}",while_statement);
                while let InterpreterResult::Ok(Literal::Boolean(true)) = condition {
                    for stmt in &while_statement.body {
                        match self.execute_statement(stmt.clone()) {
                            InterpreterResult::Ok(()) => {},
                            InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                        }

                        if self.is_break_signal() {
                            return InterpreterResult::Ok(());
                        } else if self.is_continue_signal() {
                            break;
                        }
                    }
                    condition = self.execute_expression(while_statement.condition.clone());
                }
                InterpreterResult::Ok(())
            },

            Statement::ForInStatement(loop_var, collection, body, pos) => {
                match self.execute_expression(collection) {
                    InterpreterResult::Ok(Literal::List(list)) => {
                        for item in list {
                            self.global_state.set_variable(&loop_var, item.clone());
                            for stmt in &body {
                                match self.execute_statement(stmt.clone()) {
                                    InterpreterResult::Ok(_) => {},
                                    InterpreterResult::Err(e) => {
                                        if self.is_break_signal() {
                                            self.reset_signals();
                                            return InterpreterResult::Ok(());
                                        } else if self.is_continue_signal() {
                                            self.reset_signals();
                                            break;
                                        } else {
                                            return InterpreterResult::Err(e);
                                        }
                                    }
                                }
                            }
                        }
                    },
                    InterpreterResult::Ok(Literal::String(s)) => {
                        let char_list: Vec<Literal> = s.chars()
                            .map(|c| Literal::String(c.to_string()))
                            .collect();
                        for item in char_list {
                            self.global_state.set_variable(&loop_var, item.clone());
                            for stmt in &body {
                                match self.execute_statement(stmt.clone()) {
                                    InterpreterResult::Ok(_) => {},
                                    InterpreterResult::Err(e) => {
                                        if self.is_break_signal() {
                                            self.reset_signals();
                                            return InterpreterResult::Ok(());
                                        } else if self.is_continue_signal() {
                                            self.reset_signals();
                                            break;
                                        } else {
                                            return InterpreterResult::Err(e);
                                        }
                                    }
                                }
                            }
                        }
                    },
                    InterpreterResult::Ok(Literal::Dictionary(dict)) => {
                        for (key, value_expr) in dict {
                            let value = match value_expr {
                                // Если значение – литерал, то извлекаем его с позицией, но позиция здесь не важна для вычисления
                                Expression::Literal(lit, _lit_pos) => lit.clone(),
                                _ => match self.execute_expression(value_expr) {
                                    InterpreterResult::Ok(val) => val,
                                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                                },
                            };
                            // При создании кортежа (tuple) нам нужно передать позицию.
                            // Здесь можно использовать позицию словаря, если более точную позицию нет.
                            let pair = Literal::Tuple(vec![key.clone(), value]);
                            self.global_state.set_variable(&loop_var, pair);
                            for stmt in &body {
                                match self.execute_statement(stmt.clone()) {
                                    InterpreterResult::Ok(_) => {},
                                    InterpreterResult::Err(e) => {
                                        if self.is_break_signal() {
                                            self.reset_signals();
                                            return InterpreterResult::Ok(());
                                        } else if self.is_continue_signal() {
                                            self.reset_signals();
                                            break;
                                        } else {
                                            return InterpreterResult::Err(e);
                                        }
                                    }
                                }
                            }
                        }
                    },                    
                    InterpreterResult::Ok(_) => {
                        return InterpreterResult::Err(
                            InterpreterError::RuntimeError("Collection must be a list, string, or dictionary".into())
                        )
                    },
                    InterpreterResult::Err(e) => return InterpreterResult::Err(e),
                }
                InterpreterResult::Ok(())
            },

            Statement::ReturnStatement(return_statement, pos) => {
                let value_expr = return_statement.value.expect("Return statement must have a value");
                match self.execute_expression(value_expr) {
                    InterpreterResult::Ok(val) => InterpreterResult::Err(InterpreterError::Return(val)),
                    InterpreterResult::Err(e) => InterpreterResult::Err(e),
                }
            },            
            
            Statement::TryHandleStatement(try_handle_statement, pos) => {
                let try_body = try_handle_statement.try_body;
                let handle_body = try_handle_statement.handle_body;
                let error_body = try_handle_statement.finally_body;

                let result = {
                    let mut execution_result = InterpreterResult::Ok(());
                    for stmt in handle_body {
                        let _ = self.execute_statement(stmt.clone());
                    }
                    
                    for stmt in try_body {
                        execution_result = self.execute_statement(stmt.clone());
                        if let InterpreterResult::Err(_) = &execution_result {
                            break;
                        }
                    }
                    execution_result
                };

                match result {
                    InterpreterResult::Err(e) => {
                        if let Some(finally_statements) = error_body {
                            for stmt in finally_statements {
                                let _ = self.execute_statement(stmt.clone());
                            }
                        }
                        InterpreterResult::Err(e)
                    }
                    InterpreterResult::Ok(_) => {
                        if let Some(finally_statements) = error_body {
                            for stmt in finally_statements {
                                let _ = self.execute_statement(stmt.clone());
                            }
                        }
                        result
                    }
                }
            },
            Statement::Block(block_statements, pos) => {
                let mut block_result = InterpreterResult::Ok(());
                for stmt in block_statements {
                    block_result = self.execute_statement(stmt.clone());
                    if let InterpreterResult::Err(_) = &block_result {
                        break;
                    }
                }
                block_result
            },
            Statement::FunctionDeclaration(func_decl, pos) => {
                self.global_state.set_function(
                    &func_decl.name,
                    Function {
                        params: func_decl.parameters.iter().map(|param| param.name.clone()).collect(),
                        body: func_decl.body.clone(),
                    },
                );
                InterpreterResult::Ok(())
            },    
            Statement::ExpressionStatement(expression_statement, pos) => {
                let _ = self.execute_expression(expression_statement);
                InterpreterResult::Ok(())
            }
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported statement {:?}", statement))),
        }
    }    
    
    pub fn set_break_signal(&mut self) {
        self.break_signal = true;
    }

    pub fn set_continue_signal(&mut self) {
        self.continue_signal = true;
    }

    pub fn is_break_signal(&self) -> bool {
        self.break_signal
    }

    pub fn is_continue_signal(&self) -> bool {
        self.continue_signal
    }

    pub fn reset_signals(&mut self) {
        self.break_signal = false;
        self.continue_signal = false;
    }
}

pub struct InterpreterObjectMethod;

impl InterpreterObjectMethod {
    pub fn execute(object_literal: Literal, name: String, evaluated_arguments: Vec<Literal>) -> InterpreterResult<Literal> {
        match object_literal {
            Literal::List(ref list) => {
                match name.as_str() {
                    "append" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'append' ожидает один аргумент".into())
                            );
                        }
                        let mut new_list = list.clone();
                        new_list.push(evaluated_arguments[0].clone());
                        InterpreterResult::Ok(Literal::List(new_list))
                    },
                    "length" => {
                        InterpreterResult::Ok(Literal::Integer(list.len() as i64))
                    },
                    "get" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'get' ожидает один аргумент".into())
                            );
                        }
                        let index = match &evaluated_arguments[0] {
                            Literal::Integer(i) => *i as usize,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс должен быть целым числом".into())
                            ),
                        };
                        if index >= list.len() {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс выходит за пределы списка".into())
                            );
                        }
                        println!("INER: {:?}", {list[index].clone()});
                        InterpreterResult::Ok(list[index].clone())
                    },
                    "set" => {
                        if evaluated_arguments.len() != 2 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'set' ожидает два аргумента".into())
                            );
                        }
                        let index = match &evaluated_arguments[0] {
                            Literal::Integer(i) => *i as usize,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс должен быть целым числом".into())
                            ),
                        };
                        if index >= list.len() {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс выходит за пределы списка".into())
                            );
                        }
                        let mut new_list = list.clone();
                        new_list[index] = evaluated_arguments[1].clone();
                        InterpreterResult::Ok(Literal::List(new_list))
                    },
                    "remove" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'remove' ожидает один аргумент".into())
                            );
                        }
                        let index = match &evaluated_arguments[0] {
                            Literal::Integer(i) => *i as usize,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс должен быть целым числом".into())
                            ),
                        };
                        if index >= list.len() {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Индекс выходит за пределы списка".into())
                            );
                        }
                        let mut new_list = list.clone();
                        new_list.remove(index);
                        InterpreterResult::Ok(Literal::List(new_list))
                    },
                    "len" => {
                        InterpreterResult::Ok(Literal::Integer(list.len() as i64))
                    },
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для списка", name))
                    ),
                }
            },
            Literal::Dictionary(ref dict) => {
                match name.as_str() {
                    "keys" => {
                        let keys = dict.iter().map(|(key, _)| key.clone()).collect();
                        InterpreterResult::Ok(Literal::List(keys))
                    },
                    "values" => {
                        let values = dict.iter().map(|(_, expr)| {
                            match expr {
                                Expression::Literal(lit, _) => lit.clone(),
                                _ => Literal::Null,
                            }
                        }).collect();
                        InterpreterResult::Ok(Literal::List(values))
                    },
                    "get" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'get' ожидает один аргумент".into())
                            );
                        }
                        let key = &evaluated_arguments[0];
                        for (dict_key, dict_value) in dict {
                            if dict_key == key {
                                if let Expression::Literal(literal, _) = dict_value {
                                    return InterpreterResult::Ok(literal.clone());
                                } else {
                                    return InterpreterResult::Err(InterpreterError::RuntimeError("Dictionary value is not a literal".into()));
                                }                                        
                            }
                        }
                        InterpreterResult::Ok(Literal::Null)
                    },
                    "set" => {
                        if evaluated_arguments.len() != 2 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'set' ожидает два аргумента".into())
                            );
                        }
                        let key = &evaluated_arguments[0];
                        let value = &evaluated_arguments[1];
                        let mut new_dict = dict.clone();
                        if let Some((_, expr)) = new_dict.iter_mut().find(|(k, _)| k == key) {
                            *expr = Expression::Literal(value.clone());
                        } else {
                            new_dict.push((key.clone(), Expression::Literal(value.clone())));
                        }                                
                        InterpreterResult::Ok(Literal::Dictionary(new_dict))
                    },
                    "remove" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'remove' ожидает один аргумент".into())
                            );
                        }
                        let key = &evaluated_arguments[0];
                        let mut new_dict = dict.clone();
                        new_dict.retain(|(k, _)| k != key);
                        InterpreterResult::Ok(Literal::Dictionary(new_dict))
                    },
                    "length" => {
                        InterpreterResult::Ok(Literal::Integer(dict.len() as i64))
                    },
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для словаря", name))
                    ),
                }
            },
            Literal::String(ref s) => {
                match name.as_str() {
                    "length" => InterpreterResult::Ok(Literal::Integer(s.len() as i64)),
                    "toUpperCase" => InterpreterResult::Ok(Literal::String(s.to_uppercase())),
                    "toLowerCase" => InterpreterResult::Ok(Literal::String(s.to_lowercase())),
                    "split" => {
                        if evaluated_arguments.len() != 1 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'split' ожидает один аргумент".into())
                            );
                        }
                        let separator = match &evaluated_arguments[0] {
                            Literal::String(sep) => sep,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Сепаратор должен быть строкой".into())
                            ),
                        };
                        let parts: Vec<Literal> = s.split(separator).map(|part| Literal::String(part.to_string())).collect();
                        InterpreterResult::Ok(Literal::List(parts))
                    },
                    "replace" => {
                        if evaluated_arguments.len() != 2 {
                            return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Метод 'replace' ожидает два аргумента".into())
                            );
                        }
                        let old = match &evaluated_arguments[0] {
                            Literal::String(sep) => sep,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Старый сепаратор должен быть строкой".into())
                            ),
                        };
                        let new = match &evaluated_arguments[1] {
                            Literal::String(sep) => sep,
                            _ => return InterpreterResult::Err(
                                InterpreterError::RuntimeError("Новый сепаратор должен быть строкой".into())
                            ),
                        };
                        InterpreterResult::Ok(Literal::String(s.replace(old, new)))
                    },
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для строки", name))
                    ),
                }
            },
            Literal::Integer(num) => {
                match name.as_str() {
                    "toString" => InterpreterResult::Ok(Literal::String(num.to_string())),
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для числа", name))
                    ),
                }
            },
            Literal::Float(num) => {
                match name.as_str() {
                    "toString" => InterpreterResult::Ok(Literal::String(num.to_string())),
                    "toInt" => InterpreterResult::Ok(Literal::Integer(num.into_inner() as i64)),
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для числа", name))
                    ),
                }
            },
            Literal::Boolean(_) => {
                match name.as_str() {
                    "toString" => InterpreterResult::Ok(Literal::String(match object_literal {
                        Literal::Boolean(true) => "true".to_string(),
                        Literal::Boolean(false) => "false".to_string(),
                        _ => unreachable!(),
                    })),
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для логического значения", name))
                    ),
                }
            },
            Literal::Null => {
                match name.as_str() {
                    "toString" => InterpreterResult::Ok(Literal::String("null".to_string())),
                    _ => InterpreterResult::Err(
                        InterpreterError::RuntimeError(format!("Метод '{}' не определён для null", name))
                    ),
                }
            }
            _ => InterpreterResult::Err(
                InterpreterError::RuntimeError(format!("Нельзя вызвать метод на типе: {:?}", object_literal))
            ),
        }
    }
}

pub struct InterpreterBinaryOperation;

impl InterpreterBinaryOperation {
    pub fn execute(left: Literal, operator: Operator, right: Literal) -> InterpreterResult<Literal> {
        match operator {
            Operator::Add => InterpreterBinaryOperation::add(left, right),
            Operator::Subtract => InterpreterBinaryOperation::subtract(left, right),
            Operator::Multiply => InterpreterBinaryOperation::multiply(left, right),
            Operator::Divide => InterpreterBinaryOperation::divide(left, right),
            Operator::GreaterThan => InterpreterBinaryOperation::greater_than(left, right),
            Operator::LessThan => InterpreterBinaryOperation::less_than(left, right),
            Operator::Equal => InterpreterBinaryOperation::equal(left, right),
            Operator::NotEqual => InterpreterBinaryOperation::not_equal(left, right),
            Operator::And => InterpreterBinaryOperation::and(left, right),
            Operator::Or => InterpreterBinaryOperation::or(left, right),
            Operator::GreaterThanOrEqual => InterpreterBinaryOperation::greater_than_or_equal(left, right),
            Operator::LessThanOrEqual => InterpreterBinaryOperation::less_than_or_equal(left, right),
        }
    }
    
    fn add(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Integer(l + r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 + r.0))),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l as f64 + r.0))),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 + r as f64))),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported addition types: {:?} + {:?}", left, right))),
        }
    }

    fn subtract(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Integer(l - r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 - r.0))),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l as f64 - r.0))),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 - r as f64))),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported subtraction types: {:?} - {:?}", left, right))),
        }
    }

    fn multiply(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Integer(l * r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 * r.0))),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l as f64 * r.0))),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 * r as f64))),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported multiplication types: {:?} * {:?}", left, right))),
        }
    }

    fn divide(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => {
                if r == 0 {
                    InterpreterResult::Err(InterpreterError::RuntimeError("Division by zero".into()))
                } else {
                    InterpreterResult::Ok(Literal::Integer(l / r))
                }
            }
            (Literal::Float(l), Literal::Float(r)) => {
                if r.0 == 0.0 {
                    InterpreterResult::Err(InterpreterError::RuntimeError("Division by zero".into()))
                } else {
                    InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 / r.0)))
                }
            }
            (Literal::Integer(l), Literal::Float(r)) => {
                if r.0 == 0.0 {
                    InterpreterResult::Err(InterpreterError::RuntimeError("Division by zero".into()))
                } else {
                    InterpreterResult::Ok(Literal::Float(OrderedFloat(l as f64 / r.0)))
                }
            }
            (Literal::Float(l), Literal::Integer(r)) => {
                if r == 0 {
                    InterpreterResult::Err(InterpreterError::RuntimeError("Division by zero".into()))
                } else {
                    InterpreterResult::Ok(Literal::Float(OrderedFloat(l.0 / r as f64)))
                }
            }
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported division types: {:?} / {:?}", left, right))),
        }
    }

    fn greater_than(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l > r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 > r.0)),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l as f64 > r.0)),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 > r as f64)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported greater than types: {:?} > {:?}", left, right))),
        }
    }

    fn less_than(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l < r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 < r.0)),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean((l as f64) < r.0)),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 < r as f64)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported less than types: {:?} < {:?}", left, right))),
        }
    }

    fn equal(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        InterpreterResult::Ok(Literal::Boolean(left == right))
    }

    fn not_equal(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        InterpreterResult::Ok(Literal::Boolean(left != right))
    }

    fn and(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Boolean(l), Literal::Boolean(r)) => InterpreterResult::Ok(Literal::Boolean(l && r)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported AND operation types: {:?} && {:?}", left, right))),
        }
    }

    fn or(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Boolean(l), Literal::Boolean(r)) => InterpreterResult::Ok(Literal::Boolean(l || r)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported OR operation types: {:?} || {:?}", left, right))),
        }
    }

    fn greater_than_or_equal(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l >= r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 >= r.0)),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l as f64 >= r.0)),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 >= r as f64)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported greater than or equal types: {:?} >= {:?}", left, right))),
        }
    }

    fn less_than_or_equal(left: Literal, right: Literal) -> InterpreterResult<Literal> {
        match (left.clone(), right.clone()) {
            (Literal::Integer(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l <= r)),
            (Literal::Float(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 <= r.0)),
            (Literal::Integer(l), Literal::Float(r)) => InterpreterResult::Ok(Literal::Boolean(l as f64 <= r.0)),
            (Literal::Float(l), Literal::Integer(r)) => InterpreterResult::Ok(Literal::Boolean(l.0 <= r as f64)),
            _ => InterpreterResult::Err(InterpreterError::RuntimeError(format!("Unsupported less than or equal types: {:?} <= {:?}", left, right))),
        }
    }
}
