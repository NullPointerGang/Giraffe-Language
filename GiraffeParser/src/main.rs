use GiraffeLexer::Lexer;
use GiraffeParser::Parser;


fn main() {
    let source_code = r#"
            if (a > b) {
                result = sum(a, b)
                print!(result)
            }
            elif (a == b) {
                print!("a и b равны")
            }
            else {
                result = sum(a, 2)
                print!(result.to_str())  // Выводит сумму a и 2
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
