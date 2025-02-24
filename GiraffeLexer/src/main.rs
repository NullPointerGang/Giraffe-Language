use GiraffeLexer::Lexer;

fn main() {
    let code = r#"
    const PI: float = 3.14
    var radius: int = 5
    if (PI && radius > 0) {
        var area: float = PI * radius * radius
        print!("Area of the circle: " + area)
    }
    "#;

    let lexer = Lexer::new();
    match lexer.tokenize(code) {
        Ok(tokens) => {
            for token in tokens {
                println!("Token({:?}, {})", token.token_type, token.value);
            }
        }
        Err(e) => println!("Ошибка: {}", e),
    }
}
