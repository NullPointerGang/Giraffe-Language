use GiraffeAST::*;
use GiraffeLexer::{Token, TokenType};
use ordered_float::OrderedFloat;


/// Структура парсера, содержащая список токенов и текущую позицию в них.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Создаёт новый парсер на основе вектора токенов.
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
        }
    }

    /// Возвращает текущий токен.
    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Переходит к следующему токену.
    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            
            self.pos += 1;
        }
    }    

    /// Проверяет, достигнут ли конец входного потока.
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::EOF
    }

    /// Если текущий токен соответствует ожидаемому типу (и значению, если задано), то возвращает его и переходит к следующему.
    fn expect(&mut self, token_type: TokenType, value: Option<&str>) -> Result<Token, String> {
        let token = self.current();
        if token.token_type != token_type {
            return Err(format!(
                "Ожидался токен {:?}, найден {:?}",
                token_type, token.token_type
            ));
        }
        if let Some(val) = value {
            if token.value != val {
                return Err(format!(
                    "Ожидалось значение токена '{}', найдено '{}'",
                    val, token.value
                ));
            }
        }
        let tok = token.clone();
        self.advance();
        Ok(tok)
    }

    /// Если следующий токен соответствует ожидаемому типу и значению, то переходит к нему и возвращает true.
    fn match_token(&mut self, token_type: TokenType, value: Option<&str>) -> bool {
        if self.is_at_end() {
            return false;
        }
        let token = self.current();
        if token.token_type == token_type {
            if let Some(val) = value {
                if token.value == val {
                    self.advance();
                    return true;
                } else {
                    return false;
                }
            }
            self.advance();
            return true;
        }
        false
    }

    /// Проверяет, соответствует ли следующий токен (без сдвига) указанному типу и значению.
    fn peek_token_is(&self, token_type: TokenType, value: Option<&str>) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        let token = &self.tokens[self.pos + 1];
        if token.token_type != token_type {
            return false;
        }
        if let Some(val) = value {
            token.value == val
        } else {
            true
        }
    }

    /// Проверяет, соответствует ли текущий токен указанному типу и (опционально) значению.
    fn check(&self, token_type: TokenType, value: Option<&str>) -> bool {
        if self.is_at_end() {
            return false;
        }
        let token = self.current();
        if token.token_type != token_type {
            return false;
        }
        if let Some(val) = value {
            token.value == val
        } else {
            true
        }
    }

    /// Главная функция разбора программы: последовательность операторов до EOF.
    pub fn parse_program(&mut self) -> Result<AstNode, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            self.skip_comments();
            if self.is_at_end() {
                break;
            }
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        Ok(AstNode::build_program(statements))
    }    

    /// Пропускает коментарии
    fn skip_comments(&mut self) {
        while !self.is_at_end() && 
              (self.current().token_type == TokenType::COMMENT_SINGLELINE ||
               self.current().token_type == TokenType::COMMENT_MULTILINE) {
            self.advance();
        }
    }    

    /// Разбирает все ключевые слова и конструкции
    fn parse_statement(&mut self) -> Result<Statement, String> {
        let token = self.current().clone();
        let position = token.position;
        match token.token_type {
            TokenType::KEYWORD => {
                match token.value.as_str() {
                    "func" => self.parse_function_declaration(position),
                    "var" | "const" => self.parse_variable_declaration(position),
                    "if" => self.parse_if_statement(position),
                    "while" => self.parse_while_statement(position),
                    "return" => self.parse_return_statement(position),
                    "for" => self.parse_for_statement(position),
                    "try" => self.parse_try_handle_statement(position),
                    "break" => self.parse_break_statement(position),
                    "continue" => self.parse_continue_statement(position),
                    _ => Err(format!("Неожиданное ключевое слово: {}", token.value)),
                }
            }
            TokenType::PRINT => self.parse_print_statement(position),
            TokenType::IDENTIFIER => {
                if self.peek_token_is(TokenType::OPERATOR, Some("=")) {
                    self.parse_assignment(position)
                } else {
                    let expr = self.parse_expression(position)?;
                    Ok(Statement::ExpressionStatement(expr))
                }
            }            
            _ => Err(format!("Неожиданный токен в операторе: {:?}", token)),
        }
    }        

    /// Разбирает оператор break.
    fn parse_break_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("break"))?;
        Ok(Statement::break_statement(position))
    }

    /// Разбирает оператор continue.
    fn parse_continue_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("continue"))?;
        Ok(Statement::continue_statement(position))
    }


    fn parse_try_handle_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("try"))?;
        let try_body = self.parse_block()?;
        
        self.expect(TokenType::KEYWORD, Some("handle"))?;
        let handle_body = self.parse_block()?;
        
        let finally_body = if self.check(TokenType::KEYWORD, Some("finally")) {
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::try_handle_statement(try_body, handle_body, finally_body, position))
    }
    

    /// Разбирает цикл for
    fn parse_for_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("for"))?;
        self.expect(TokenType::BRACKET, Some("("))?;
        
        if self.check(TokenType::IDENTIFIER, None) && self.peek_token_is(TokenType::KEYWORD, Some("in")) {
            let loop_var_token = self.expect(TokenType::IDENTIFIER, None)?;
            let loop_var = loop_var_token.value;
            self.expect(TokenType::KEYWORD, Some("in"))?;
            let collection_expr = self.parse_expression(position)?;
            self.expect(TokenType::BRACKET, Some(")"))?;
            let body = self.parse_block()?;
            
            return Ok(Statement::for_in_statement(loop_var, collection_expr, body, position));
        } else {
            let initializer = if self.check(TokenType::PUNCTUATION, Some(";")) {
                None
            } else {
                if self.check(TokenType::KEYWORD, Some("var")) || self.check(TokenType::KEYWORD, Some("const")) {
                    Some(self.parse_variable_declaration(position)?)
                } else {
                    Some(Statement::ExpressionStatement(self.parse_expression(position)?))
                }
            };
            self.expect(TokenType::PUNCTUATION, Some(";"))?;
            
            let condition = if self.check(TokenType::PUNCTUATION, Some(";")) {
                None
            } else {
                Some(self.parse_expression(position)?)
            };
            self.expect(TokenType::PUNCTUATION, Some(";"))?;
            
            let update = if self.check(TokenType::BRACKET, Some(")")) {
                None
            } else {
                Some(self.parse_expression(position)?)
            };
            self.expect(TokenType::BRACKET, Some(")"))?;
            
            let mut body = self.parse_block()?;
            if let Some(update_expr) = update {
                body.push(Statement::ExpressionStatement(update_expr));
            }
            
            let loop_condition = condition.unwrap_or(Expression::Literal {
                value: Literal::Boolean(true),
                position,
            });
            let while_stmt = Statement::while_statement(loop_condition, body, position);
            
            if let Some(init_stmt) = initializer {
                Ok(Statement::Block {
                    body: vec![init_stmt, while_stmt],
                    position,
                })
            } else {
                Ok(while_stmt)
            }
        }
    }


    /// Разбирает объявление функции:
    /// func имя(параметры): возвращаемый_тип { тело }
    fn parse_function_declaration(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("func"))?;
        let name_token = self.expect(TokenType::IDENTIFIER, None)?;
        let name = name_token.value;
        self.expect(TokenType::BRACKET, Some("("))?;
        let mut parameters = Vec::new();
        if !self.check(TokenType::BRACKET, Some(")")) {
            loop {
                let param_name_token = self.expect(TokenType::IDENTIFIER, None)?;
                let param_name = param_name_token.value;
                self.expect(TokenType::PUNCTUATION, Some(":"))?;
                
                let type_token = self.current().clone();

                if type_token.token_type != TokenType::IDENTIFIER
                    && type_token.token_type != TokenType::KEYWORD
                    && type_token.token_type != TokenType::INTEGER
                    && type_token.token_type != TokenType::LIST
                    && type_token.token_type != TokenType::FLOAT
                    && type_token.token_type != TokenType::TUPLE
                    && type_token.token_type != TokenType::STRING
                    && type_token.token_type != TokenType::BOOLEAN
                    && type_token.token_type != TokenType::DICT
                    && type_token.token_type != TokenType::ANNOTATION_TYPE
                {
                    return Err(format!("Ожидался тип переменной, найден: {:?}", type_token));
                }

                let data_type = type_token.value.clone();
                self.advance();
                
                let var_decl = VariableDeclaration {
                    name: param_name,
                    data_type,
                    value: None,
                    position,
                };
                parameters.push(var_decl);
                
                if self.check(TokenType::BRACKET, Some(")")) {
                    break;
                }
                self.expect(TokenType::PUNCTUATION, Some(","))?;
            }
        }
        self.expect(TokenType::BRACKET, Some(")"))?;
        
        let mut return_type = "void".to_string();
        if self.match_token(TokenType::PUNCTUATION, Some(":")) {
            let ret_type_token = self.current().clone();
            if ret_type_token.token_type != TokenType::IDENTIFIER
                && ret_type_token.token_type != TokenType::ANNOTATION_TYPE
                && ret_type_token.token_type != TokenType::INTEGER
                && ret_type_token.token_type != TokenType::LIST
                && ret_type_token.token_type != TokenType::KEYWORD
            {
                return Err(format!(
                    "Ожидался тип возвращаемого значения, найден: {:?}",
                    ret_type_token
                ));
            }

            return_type = ret_type_token.value.clone();
            self.advance();
        }
        
        let body = self.parse_block()?;
        Ok(Statement::function_declaration(name, parameters, return_type, body, position))
    }    
    
    /// Разбирает объявление переменной:
    /// синтаксис: var/const имя [ ":" тип ] [ "=" выражение ]
    fn parse_variable_declaration(&mut self, position: Position) -> Result<Statement, String> {
        let keyword_token = self.current().clone();
        if keyword_token.value != "var" && keyword_token.value != "const" {
            return Err(format!(
                "Ожидалась переменная или константа, найдено: {}",
                keyword_token.value
            ));
        }
        self.advance();
        
        let name_token = self.expect(TokenType::IDENTIFIER, None)?;
        let name = name_token.value;
        
        let mut data_type: Option<String> = None;
        if self.check(TokenType::PUNCTUATION, Some(":")) {
            self.advance();
            let type_token = self.current().clone();
            if type_token.token_type != TokenType::IDENTIFIER
                && type_token.token_type != TokenType::KEYWORD
                && type_token.token_type != TokenType::INTEGER
                && type_token.token_type != TokenType::LIST
                && type_token.token_type != TokenType::FLOAT
                && type_token.token_type != TokenType::TUPLE
                && type_token.token_type != TokenType::STRING
                && type_token.token_type != TokenType::BOOLEAN
                && type_token.token_type != TokenType::DICT
                && type_token.token_type != TokenType::ANNOTATION_TYPE
            {
                return Err(format!("Ожидался тип переменной, найден: {:?}", type_token));
            }
            data_type = Some(type_token.value.clone());
            self.advance();
        }

        let mut value = None;
        if self.match_token(TokenType::OPERATOR, Some("=")) {
            let expr = self.parse_expression(position)?;
            value = Some(expr);

            if data_type.is_none() {
                data_type = Some(self.infer_type_from_expression(value.as_ref().unwrap()));
            }
        }
        else if data_type.is_none() {
            return Err("Тип переменной не указан и отсутствует инициализатор для автоопределения типа".to_string());
        }
        
        Ok(Statement::variable_declaration(name, data_type.unwrap(), value, position))
    }

    /// Вспомогательная функция для определения типа выражения.
    fn infer_type_from_expression(&self, expr: &Expression) -> String {
        match expr {
            Expression::Literal { value: Literal::Integer(_), .. } => "int".to_string(),
            Expression::Literal { value: Literal::Float(_), .. } => "float".to_string(),
            Expression::Literal { value: Literal::String(_), .. } => "string".to_string(),
            Expression::Literal { value: Literal::Boolean(_), .. } => "bool".to_string(),
            Expression::Literal { value: Literal::Null, .. } => "null".to_string(),
            Expression::List { .. } => "list".to_string(),
            Expression::Tuple { .. } => "tuple".to_string(),
            Expression::Dictionary { .. } => "dict".to_string(),
            Expression::FunctionCall { .. } => "auto".to_string(),
            _ => "auto".to_string(),
        }
    }


    /// Разбирает условный оператор if с опциональными elif и else. 
    fn parse_if_statement(&mut self, position: Position) -> Result<Statement, String> {
        let keyword = self.current().value.clone();
        if keyword != "if" && keyword != "elif" {
            return Err(format!("Ожидался 'if' или 'elif', найдено: {}", keyword));
        }
        self.advance();
    
        self.expect(TokenType::BRACKET, Some("("))?;
        let condition = self.parse_expression(position)?;
        self.expect(TokenType::BRACKET, Some(")"))?;
        let body = self.parse_block()?;
    
        let mut elif_branch: Option<Box<IfStatement>> = None;
        let mut else_body: Option<Vec<Statement>> = None;

        if self.check(TokenType::KEYWORD, Some("elif")) {
            if let Statement::IfStatement(if_stmt) = self.parse_if_statement(position)? {
                elif_branch = Some(Box::new(if_stmt));
            } else {
                return Err("Ожидался if для elif".to_string());
            }
        } else if self.check(TokenType::KEYWORD, Some("else")) {
            self.expect(TokenType::KEYWORD, Some("else"))?;
            else_body = Some(self.parse_block()?);
        }
    
        Ok(Statement::if_statement(condition, body, elif_branch.map(|b| *b), else_body, position))
    }

    /// Разбирает цикл while:
    /// while (условие) { тело }
    fn parse_while_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("while"))?;
        self.expect(TokenType::BRACKET, Some("("))?;
        let condition = self.parse_expression(position)?;
        self.expect(TokenType::BRACKET, Some(")"))?;
        let body = self.parse_block()?;
        Ok(Statement::while_statement(condition, body, position))
    }

    /// Разбирает оператор печати:
    /// print!(выражение)
    fn parse_print_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::PRINT, Some("print!"))?;
        self.expect(TokenType::BRACKET, Some("("))?;
    
        let mut args = Vec::new();

        if !self.check(TokenType::BRACKET, Some(")")) {
            loop {
                let arg = self.parse_expression(position)?;
                args.push(arg);
                if self.check(TokenType::PUNCTUATION, Some(",")) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
    
        self.expect(TokenType::BRACKET, Some(")"))?;
        Ok(Statement::print_statement(GiraffeAST::Expression::List{elements: args, position}, position))
    }

    /// Разбирает оператор возврата:
    /// return [выражение]
    fn parse_return_statement(&mut self, position: Position) -> Result<Statement, String> {
        self.expect(TokenType::KEYWORD, Some("return"))?;
        // Если следующего токена нет или он завершает блок – возвращаем без значения
        let value = if !self.check(TokenType::BRACKET, Some("}")) && !self.check(TokenType::EOF, None) {
            Some(self.parse_expression(position)?)
        } else {
            None
        };
        Ok(Statement::return_statement(value, position))
    }

    /// Разбирает оператор присваивания:
    /// идентификатор = выражение
    fn parse_assignment(&mut self, position: Position) -> Result<Statement, String> {
        let name_token = self.expect(TokenType::IDENTIFIER, None)?;
        let name = name_token.value;
        self.expect(TokenType::OPERATOR, Some("="))?;
        let value = self.parse_expression(position)?;
        Ok(Statement::assignment_statement(name, value, position))
    }

    /// Разбирает блок операторов, заключённых в фигурные скобки.
    fn parse_block(&mut self) -> Result<Vec<Statement>, String> {
        self.expect(TokenType::BRACKET, Some("{"))?;
        let mut statements = Vec::new();
        while !self.check(TokenType::BRACKET, Some("}")) && !self.is_at_end() {
            self.skip_comments();
            if self.check(TokenType::BRACKET, Some("}")) {
                break;
            }
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        self.expect(TokenType::BRACKET, Some("}"))?;
        Ok(statements)
    }    

    // =========================================================================
    // Функции разбора выражений с учётом приоритетов операторов.
    // =========================================================================

    fn parse_expression(&mut self, position: Position) -> Result<Expression, String> {
        self.parse_logical_or(position)
    }

    fn parse_logical_or(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and(position)?;
        while self.check_operator("||") || self.check_keyword("or") {
            let op = Operator::Or;
            self.advance();
            let right = self.parse_logical_and(position)?;
            expr = Expression::binary_operation(expr, op, right, position);
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_equality(position)?;
        while self.check_operator("&&") || self.check_keyword("and") {
            let op = Operator::And;
            self.advance();
            let right = self.parse_equality(position)?;
            expr = Expression::binary_operation(expr, op, right, position);
        }
        Ok(expr)
    }

    fn parse_equality(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_comparison(position)?;
        while self.check_operator("==") || self.check_operator("!=") {
            let op_token = self.current().clone();
            let op = match op_token.value.as_str() {
                "==" => Operator::Equal,
                "!=" => Operator::NotEqual,
                _ => return Err(format!("Неизвестный оператор равенства: {}", op_token.value)),
            };
            self.advance();
            let right = self.parse_comparison(position)?;
            expr = Expression::binary_operation(expr, op, right, position);
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_term(position)?;
        while self.check_operator(">") || self.check_operator("<") || self.check_operator(">=") || self.check_operator("<=") {
        let op_token = self.current().clone();
        let op = match op_token.value.as_str() {
            ">"  => Operator::GreaterThan,
            "<"  => Operator::LessThan,
            ">=" => Operator::GreaterThanOrEqual,
            "<=" => Operator::LessThanOrEqual,
            _ => return Err(format!("Неизвестный оператор сравнения: {}", op_token.value)),
        };
        self.advance();
        let right = self.parse_term(position)?;
        expr = Expression::binary_operation(expr, op, right, position);
    }  
        Ok(expr)
    }

    fn parse_term(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_factor(position)?;
        while self.check_operator("+") || self.check_operator("-") {
            let op_token = self.current().clone();
            let op = match op_token.value.as_str() {
                "+" => Operator::Add,
                "-" => Operator::Subtract,
                _ => return Err(format!("Неизвестный оператор: {}", op_token.value)),
            };
            self.advance();
            let right = self.parse_factor(position)?;
            expr = Expression::binary_operation(expr, op, right, position);
        }
        Ok(expr)
    }

    fn parse_factor(&mut self, position: Position) -> Result<Expression, String> {
        let mut expr = self.parse_unary(position)?;
        while self.check_operator("*") || self.check_operator("/") {
            let op_token = self.current().clone();
            let op = match op_token.value.as_str() {
                "*" => Operator::Multiply,
                "/" => Operator::Divide,
                _ => return Err(format!("Неизвестный оператор: {}", op_token.value)),
            };
            self.advance();
            let right = self.parse_unary(position)?;
            expr = Expression::binary_operation(expr, op, right, position);
        }
        Ok(expr)
    }

    /// Обрабатывает унарный минус.
    fn parse_unary(&mut self, position: Position) -> Result<Expression, String> {
        if self.check_operator("-") {
            self.advance();
            let expr = self.parse_unary(position)?;
            // Представляем унарный минус как операцию: 0 - expr
            return Ok(Expression::binary_operation(
                Expression::literal(Literal::Integer(0), position),
                Operator::Subtract,
                expr,
                position,
            ));
        }
        self.parse_primary(position)
    }

    /// Разбирает первичные выражения: литералы, идентификаторы (с возможным вызовом функции),
    /// группировку в круглых скобках, списки и словари.
    fn parse_primary(&mut self, position: Position) -> Result<Expression, String> {
        let token = self.current().clone();
        
        let mut expr = match token.token_type {
            TokenType::INTEGER => {
                let value: i64 = token.value.parse().map_err(|_| {
                    format!("Неверный формат целого числа: {}", token.value)
                })?;
                self.advance();
                Expression::Literal {
                    value: Literal::Integer(value),
                    position,
                }
            }
            TokenType::FLOAT => {
                let value: f64 = token.value.parse().map_err(|_| {
                    format!("Неверный формат числа с плавающей точкой: {}", token.value)
                })?;
                self.advance();
                Expression::Literal{
                    value: Literal::Float(OrderedFloat::from(value)), 
                    position
                }

            }
            TokenType::STRING => {
                let trimmed = token.value.trim_matches('"').to_string();
                self.advance();
                Expression::Literal{
                    value: Literal::String(trimmed), 
                    position
                }
            }
            TokenType::BOOLEAN => {
                let value = token.value == "true";
                self.advance();
                Expression::Literal{
                    value: Literal::Boolean(value), 
                    position
                }
            }
            TokenType::NULL => {
                self.advance();
                Expression::Literal{
                    value: Literal::Null, 
                    position
                }
            }
            TokenType::IDENTIFIER => {
                let ident = token.value.clone();
                self.advance();
                if self.check(TokenType::BRACKET, Some("(")) {
                    self.advance();
                    let mut args = Vec::new();
                    if !self.check(TokenType::BRACKET, Some(")")) {
                        loop {
                            let arg = self.parse_expression(position)?;
                            args.push(arg);
                            if self.check(TokenType::PUNCTUATION, Some(",")) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(TokenType::BRACKET, Some(")"))?;
                    Expression::FunctionCall{
                        name: ident, 
                        args, 
                        position}
                } else {
                    Expression::Variable{
                        name: ident, 
                        position
                    }
                }
            }
            TokenType::BRACKET => {
                if token.value == "(" {
                    self.advance();
                    let expr = self.parse_expression(position)?;
                    self.expect(TokenType::BRACKET, Some(")"))?;
                    expr
                } else if token.value == "[" {
                    self.parse_list(position)?
                } else if token.value == "{" {
                    self.parse_dictionary(position)?
                } else {
                    return Err(format!("Неожиданный скобочный символ: {}", token.value));
                }
            }
            _ => return Err(format!("Неожиданный токен в выражении: {:?}", token)),
        };

        expr = self.parse_postfix(expr, position)?;
        Ok(expr)
    }

    /// Обрабатывает цепочку точечных обращений: вызовы методов или доступ к полям.
    fn parse_postfix(&mut self, mut expr: Expression, position: Position) -> Result<Expression, String> {
        loop {
            if self.check_operator(".") {
                self.advance();
                let member_token = self.expect(TokenType::IDENTIFIER, None)?;
                let member_name = member_token.value;
                
                if self.check(TokenType::BRACKET, Some("(")) {
                    self.advance(); // пропускаем '('
                    let mut args = Vec::new();
                    if !self.check(TokenType::BRACKET, Some(")")) {
                        loop {
                            let arg = self.parse_expression(position)?;
                            args.push(arg);
                            if self.check(TokenType::PUNCTUATION, Some(",")) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(TokenType::BRACKET, Some(")"))?;
                    expr = Expression::MethodCall{
                        object: Box::new(expr), 
                        method: member_name, 
                        args, 
                        position
                    };
                } 
                // Сейчас нет нужды в MemberAccess
                // else {
                //     expr = Expression::MemberAccess(Box::new(expr), member_name);
                // }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Разбирает литерал списка: [ expr1, expr2, … ]
    fn parse_list(&mut self, position: Position) -> Result<Expression, String> {
        self.expect(TokenType::BRACKET, Some("["))?;
        let mut elements = Vec::new();
        if !self.check(TokenType::BRACKET, Some("]")) {
            loop {
                let elem = self.parse_expression(position)?;
                elements.push(elem);
                if self.check(TokenType::PUNCTUATION, Some(",")) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(TokenType::BRACKET, Some("]"))?;
        Ok(Expression::List{
            elements,
            position
        })
    }

    /// Разбирает литерал словаря: { key1: value1, key2: value2, … }
    fn parse_dictionary(&mut self, position: Position) -> Result<Expression, String> {
        self.expect(TokenType::BRACKET, Some("{"))?;
        let mut pairs = Vec::new();
        if !self.check(TokenType::BRACKET, Some("}")) {
            loop {
                let key = match self.parse_expression(position)? {
                    Expression::Literal{
                        value: lit, 
                        position
                    } => lit,
                    _ => return Err("Dictionary keys must be literals".to_string()),
                };
                self.expect(TokenType::PUNCTUATION, Some(":"))?;
                let value = self.parse_expression(position)?;
                pairs.push((key, value));
                if self.check(TokenType::PUNCTUATION, Some(",")) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(TokenType::BRACKET, Some("}"))?;
        Ok(Expression::Dictionary{
            entries: pairs, 
            position
        })
    }    

    // =========================================================================
    // Вспомогательные функции для проверки операторов и ключевых слов
    // =========================================================================

    fn check_operator(&self, op: &str) -> bool {
        if self.is_at_end() {
            return false;
        }
        let token = self.current();
        if op == "." {
            (token.token_type == TokenType::OPERATOR || token.token_type == TokenType::PUNCTUATION) && token.value == op
        } else {
            token.token_type == TokenType::OPERATOR && token.value == op
        }
    }    

    fn check_keyword(&self, kw: &str) -> bool {
        if self.is_at_end() {
            return false;
        }
        let token = self.current();
        token.token_type == TokenType::KEYWORD && token.value == kw
    }

    // =========================================================================
    // Вспомогательные функции для составления ошибок
    // =========================================================================

    pub fn get_current_position(&self) -> usize {
        return self.pos;
    }

    pub fn get_current_token(&self) -> Token {
        return self.tokens[self.pos].clone();
    }
}