use std::env;
use std::fs;
use std::process;
use colored::*;
use GiraffeLexer::Lexer;
use GiraffeParser::Parser;
use GiraffeInterpreter::{Interpreter, Context, InterpreterResult, Function, StateStore};
use GiraffeError::print_error;
use GiraffeAST::{AstNode, Statement};


pub fn interpret(ast_node: AstNode, filename: String) {
    let mut global_state = Context::new();

    if let AstNode::Program { statements } = &ast_node {
        for statement in statements {
            if let Statement::FunctionDeclaration(func_decl) = statement {
                global_state.set_function(&func_decl.name, Function {
                    params: func_decl.parameters.iter().map(|p| p.name.clone()).collect(),
                    body: func_decl.body.clone(),
                });
            }
        }
    }

    let mut interpreter = Interpreter::new(global_state.clone(), filename.clone());

    if let AstNode::Program { statements } = ast_node {
        for statement in statements {
            if !matches!(statement, Statement::FunctionDeclaration(_)) {
                interpreter.execute_statement(statement.clone());            
            }
        }
    }

    if let Some(main_func) = global_state.get_function("main") {
        for statement in &main_func.body {
            interpreter.execute_statement(statement.clone());        
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("{}", "Использование: <filename>".red().bold());
        process::exit(1);
    }

    let filename = &args[1];
    let source_code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            print_error(
                "Ошибка при чтении файла",
                &[("Детали", err.to_string())],
                filename,
                0,
                0,
            );
            process::exit(1);
        }
    };

    let tokens_result = Lexer::new().tokenize(&source_code);

    match tokens_result {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            let program = parser.parse_program();

            if let Ok(program) = program {
                interpret(program, (*filename).to_string());
            } else {
                let error_token = parser.get_current_token();
                print_error(
                    "Ошибка при парсинге программы",
                    &[
                        ("Найденное значение", error_token.value.clone())
                    ],
                    filename,
                    error_token.position.line,
                    error_token.position.column,
                );
                println!(
                    "{}",
                    format!("Детали: {:?}", program.err().unwrap()).on_red().bold()
                );
                process::exit(1);
            }
        },
        Err(err) => {
            
            process::exit(1);
        }
    }
}
