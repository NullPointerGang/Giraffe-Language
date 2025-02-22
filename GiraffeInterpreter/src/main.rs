use GiraffeLexer::Lexer;
use GiraffeParser::Parser;
use GiraffeInterpreter::Interpreter;


fn main() {
    let source_code = r#"
        const PI: float = 3.14159

        func sum(a: int, b: int): int {
            return a + b
        }

        func main(){
            var a: int = 5
            var b: int = 10
            var result: int

            var users = {"user1": "1", "user2": "2", "user3": "3"}

            if (a != b) {
                result = sum(a, b)
                print!(result)
            }
            elif (a == b) {
                print!("a и b равны")
            }
            else {
                result = sum(a, 2)
                print!(result)
            }

            var ops: int = 0
            
            for (user in users) {
                print!(user)
            }

            var flag: bool = true
            if (flag and ops > 1) {
                print!("Флаг установлен, и операций больше одного!")
            }

            var area: float = PI * (a * a)
            print!(area)

            var dictionary = {"key1": "value1", "key2": "value2"}
            print!(dictionary)
        }
        print!(main())
    "#;

    let tokens = Lexer::new().tokenize(source_code);

    println!("Tokens: {:?}", tokens);

    let mut parser = Parser::new(tokens.expect("REASON"));
    let program = parser.parse_program();
    println!("AST: {:?}", program);

    let mut interpreter = Interpreter::new();

    interpreter.interpret_program(program.expect("REASON"));
}
