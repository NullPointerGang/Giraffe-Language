use GiraffeLexer::Lexer;
use GiraffeParser::Parser;


fn main() {
    let source_code = r#"
        while (true) {
            var x = 5
            var y = 10
            var z = x + y
            if (z > 15) {
                break
            }
            else {
                continue
            }
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
