use GiraffeLexer::Lexer;
use GiraffeParser::Parser;


fn main() {
    let source_code = r#"
        try {
            print!("Попытка выполнения")
        }
        handle {
            print!("Обработка исключения")
        }
        finally {
            print!("Блок finally")
        }
    "#;

    let tokens = Lexer::new().tokenize(source_code);

    println!("Токены: {:?}", tokens);

    let mut parser = Parser::new(tokens.expect("Ошибка токенизации"));

    match parser.parse_program() {
        Ok(ast) => {
            println!("AST дерево: {:?}", ast);
        }
        Err(e) => {
            eprintln!("Ошибка парсинга: {}", e);
        }
    }
}
