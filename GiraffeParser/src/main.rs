use GiraffeLexer::Lexer;
use GiraffeParser::Parser;


fn main() {
    let source_code = r#"
    const PI: float = 3.14
    var radius: int = 5
    if (PI && radius >= 0) {
        var area: float = PI * radius * radius
        print!("Area of the circle: " + area)
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
