use std::env;
use std::fs;
use colored::*;
use std::process;
use GiraffeLexer::Lexer;
use GiraffeParser::Parser;
use GiraffeInterpreter::interpret;


fn print_error(title: &str, details: &[(&str, String)], filename: &str, line: usize, column: usize) {
    let absolute_path = match fs::canonicalize(filename) {
        Ok(path) => path.display().to_string(),
        Err(_) => filename.to_string(),
    };

    eprintln!("{}", format!("error: {}", title).red().bold());
    
    eprintln!(" --> {}:{}:{}", absolute_path, line, column);
    eprintln!("  |");
    
    for (label, value) in details {
        eprintln!("{} {}", label.green().bold(), value);
    }

    eprintln!("  |");
    eprintln!("  |                 ^");
    eprintln!();
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
                interpret(program);
            } else {
                let error_token = parser.get_current_token();
                print_error(
                    "Ошибка при парсинге программы",
                    &[
                        ("Найденное значение", error_token.value.clone())
                    ],
                    filename,
                    error_token.line,
                    error_token.column,
                );
                process::exit(1);
            }
        },
        Err(err) => {
            print_error(
                "Ошибка токенизации",
                &[("Детали", format!("{:?}", err))],
                filename,
                0,
                0,
            );
            process::exit(1);
        }
    }
}
