use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Комментарии
    COMMENT_MULTILINE,
    COMMENT_SINGLELINE,

    // Ключевые слова
    KEYWORD,

    // Идентификаторы
    IDENTIFIER,

    // Примитивные типы данных
    INTEGER,
    FLOAT,
    STRING,
    BOOLEAN,

    // Аннотации типов
    ANNOTATION_TYPE,

    // Сложные типы данных
    LIST,
    DICT,
    SET,
    TUPLE,

    // Специальные типы данных
    NULL,
    OPTION,
    ERROR,
    FUNCTION,

    // Операторы и символы
    OPERATOR,
    SYMBOL,
    BRACKET,
    PUNCTUATION,

    // Встроенные конструкции
    PRINT,

    // Специальные токены
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, value: &str, line: usize, column: usize) -> Self {
        Token {
            token_type,
            value: value.to_string(),
            line,
            column,
        }
    }
}

pub struct Lexer {
    keywords: HashMap<String, TokenType>,
    // Список шаблонов: каждый элемент — пара (регулярное выражение, Some(тип_токена)) или (regex, None) для игнорируемых символов.
    regex_patterns: Vec<(Regex, Option<TokenType>)>,
}

impl Lexer {
    pub fn new() -> Self {
        let mut keywords = HashMap::new();
        for kw in &[
            "const", "func", "var", "if", "elif", "else", "while", "for",
            "continue", "break", "return", "is", "in", "and", "or", "not",
            "try", "handle", "finally", "get"
        ] {
            keywords.insert(kw.to_string(), TokenType::KEYWORD);
        }

        let patterns: Vec<(&str, Option<TokenType>)> = vec![
            // Специальный токен print!
            (r"(print!)", Some(TokenType::PRINT)),
            
            // Однострочные комментарии
            (r"//[^\n]*", Some(TokenType::COMMENT_SINGLELINE)),
            // Многострочные комментарии
            (r"/\*[\s\S]*?\*/", Some(TokenType::COMMENT_MULTILINE)),

            // Примитивные типы (int, str, bool) — тип будем помечать как INTEGER
            (r"\bint\b", Some(TokenType::ANNOTATION_TYPE)),
            (r"\bstr\b", Some(TokenType::ANNOTATION_TYPE)),
            (r"\bbool\b", Some(TokenType::ANNOTATION_TYPE)),

            // Сложные типы (list, dict, set, tuple)
            (r"\blist\b", Some(TokenType::ANNOTATION_TYPE)),
            (r"\bdict\b", Some(TokenType::ANNOTATION_TYPE)),
            (r"\btuple\b", Some(TokenType::ANNOTATION_TYPE)),

            // Ключевые слова
            (r"\b(?:const|func|var|if|elif|else|while|for|continue|break|return|is|in|and|or|not|try|handle|finally|global)\b", Some(TokenType::KEYWORD)),

            // Логические значения
            (r"\b(?:true|false)\b", Some(TokenType::BOOLEAN)),

            // Null/None
            (r"\b(null|None)\b", Some(TokenType::NULL)),

            // Идентификаторы (имена переменных или функций)
            (r"[a-zA-Z_][a-zA-Z0-9_]*", Some(TokenType::IDENTIFIER)),

            // Числа с плавающей точкой (float)
            (r"\b\d+\.\d+\b", Some(TokenType::FLOAT)),

            // Целые числа
            (r"\b\d+\b", Some(TokenType::INTEGER)),

            // Строки (с двойными кавычками)
            (r#""([^"\\]|\\.)*""#, Some(TokenType::STRING)),

            // Операторы
            (r"==|!=|&&|\|\||[+\-*/%=<>!&|^]=?", Some(TokenType::OPERATOR)),

            // Скобки (включая круглые, фигурные и квадратные)
            (r"[\(\)\{\}\[\]]", Some(TokenType::BRACKET)),

            // Пунктуация
            (r"[;,:.]", Some(TokenType::PUNCTUATION)),

            // Пробельные символы — пропускаем (None означает, что токен не создаётся)
            (r"\s+", None),

            // Любой одиночный символ, который не подошёл под предыдущие шаблоны — ошибка
            (r".", Some(TokenType::ERROR)),
        ];

        // Компилируем шаблоны с добавлением "^" в начало
        let regex_patterns = patterns
            .into_iter()
            .map(|(pat, token_type)| {
                let anchored = format!("^{}", pat);
                (Regex::new(&anchored).unwrap(), token_type)
            })
            .collect();

        Lexer {
            keywords,
            regex_patterns,
        }
    }

    pub fn tokenize(&self, code: &str) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut position = 0;
        let code_len = code.len();
        
        let mut line = 1;
        let mut column = 1;

        while position < code_len {
            let slice = &code[position..];
            let mut matched = false;

            for (regex, token_type_opt) in &self.regex_patterns {
                if let Some(mat) = regex.find(slice) {
                    if mat.start() != 0 {
                        continue;
                    }
                    let value = &slice[mat.start()..mat.end()];
                    
                    if let Some(token_type) = token_type_opt {
                        tokens.push(Token::new(token_type.clone(), value, line, column));
                    }

                    for ch in value.chars() {
                        if ch == '\n' {
                            line += 1;
                            column = 1;
                        } else {
                            column += 1;
                        }
                    }

                    position += mat.end();
                    matched = true;
                    break;
                }
            }

            if !matched {
                let err_char = code[position..].chars().next().unwrap();
                return Err(format!(
                    "Нераспознанный символ на позиции {} (line {}, column {}): '{}'",
                    position, line, column, err_char
                ));
            }
        }

        // Добавляем токен EOF с текущими координатами
        tokens.push(Token::new(TokenType::EOF, "EOF", line, column));
        Ok(tokens)
    }
}