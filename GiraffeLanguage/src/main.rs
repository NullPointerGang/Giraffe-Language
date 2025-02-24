use std::env;
use std::fs;
use std::process;
use GiraffeLexer::Lexer;
use GiraffeParser::Parser;
use GiraffeInterpreter::interpret;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let source_code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Ошибка при чтении файла {}: {}", filename, err);
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
                eprintln!("Ошибка при парсинге программы!");
                process::exit(1);
            }
        },
        Err(err) => {
            eprintln!("Ошибка токенизации: {:?}", err);
            process::exit(1);
        }
    }
}
