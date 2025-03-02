use GiraffeLexer::Lexer;
use GiraffeParser::Parser;


fn main() {
    let source_code = r#"
        func bubbleSort(arr: list) {
            print!("Starting bubble sort on array: ", arr)
            var swapped = true
            while (swapped) {
                swapped = false
                var i = 0
                while (i < arr.len() - 1) {
                    if (arr.get(i) > arr.get(i+1)) {
                        var temp = arr.get(i)  
                        print!("Swapping: ", arr.get(i), " with ", arr.get(i+1))
                        arr.set(i, arr.get(i+1))
                        arr.set(i+1, temp)
                        swapped = true
                    }
                    i = i + 1
                }
                print!("Array after pass: ", arr)
            }
            print!("Sorted array: ", arr)
            return arr
        }

        func benchmarkSort() {
            var arr = [999, 100, 9998, 997, 9934336]
            print!("Unsorted array: ", arr)
            var sorted = bubbleSort(arr)
            print!("Sorted array: ", sorted)
        }

        func main() {
            benchmarkSort()
        }
    "#;

    let tokens = Lexer::new().tokenize(source_code);

    println!("Токены: {:?}", tokens);

    let mut parser = Parser::new(tokens.expect("Ошибка токенизации"));

    match parser.parse_program() {
        Ok(ast) => {
            println!("AST дерево: {:#?}", ast);
        }
        Err(e) => {
            eprintln!("Ошибка парсинга: {}", e);
        }
    }
}
