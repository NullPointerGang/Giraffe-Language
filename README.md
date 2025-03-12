# Giraffe-Language

![Giraffe](images/Giraffe.png)

**Giraffe-Language** — это интерпритуруемый язык программирования, ориентированный на создание скриптов для автоматизации процессов и разработки удобных интерфейсов для сложных проектов. 


## Текущий статус

На данный момент проект на паузе. Версия 1.2.0 является финальной, но сейчас идет разработка версии 2.0.0.

### Графичиская схема роботы:

![Giraffe Sheme](images/Giraffe_Sheme.jpg)

### Описание схемы:

#### Лаксер (Токенайзер)
Разбивает исходный код на токены, представляя его в виде более простых элементов для дальнейшей обработки


#### Парсер
Обрабатывает токены, строит абстрактное синтаксическое дерево (AST) и отправляет данные в хранилище для дальнейшей работы


#### Интерпритатор
Проходит по AST, работает с хранилищем переменных (`env` или `global`), а также обновляет состояние интерпретатора, включая указатель на текущую инструкцию и другие параметры


#### Хранилищ `env`/`state`
Хранилище перемененных состоит из 
- имя переменной
- тип перемнной 
- значение 

В хранилище переменные добавляются при инициализации если переменная определена, если нет то при ее определении


#### Хранилище `state`
Содержит информацию необходимую для интерпретатора в текущий момент. 

**Описание `state`:**

- Очередь вызовов 
- Указатель инструкции (номер или строка в AST дереве исполняемого узла)
- Регистр перемененных - временные переменные только для интерпретатора 

####  ***Опционально для `state`:***

- Последняя ошибка
- Состояние роботы интерпретатора
- Отладочный режим


## Синтаксис:
```js
/*
Многострочный коментарий

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
*/


// Однострочный коментарий  


const PI: float = 3.14159  // Константа с типом данных float

// Функция для сложения двух чисел
func sum(a: int, b: int): int {
    return a + b
}

func main(){
    var a: int = 5
    var b: int = 10
    var result: int
    var users = [{"username": "user1"}, {"username": "user2"}, {"username": "user3"}]  // Список пользователей
    
    // Использование оператора сравнения
    if (a > b) {
        result = sum(a, b)
        print!(result.to_str())  // Выводит сумму чисел
    }
    elif (a == b) {
        print!("a и b равны")
    }
    else {
        result = sum(a, 2)
        print!(result.to_str())  // Выводит сумму a и 2
    }
    
    // Пример работы с while
    var ops: int = 0
    while (ops < 3) {
        ops = ops + 1
        print!(ops.to_str())  // Печатает количество операций
    }
    
    // Пример работы с for
    for (user in users) {
        print!(user.to_str())  // Выводит {"username": "user1"}, {"username": "user2"}, {"username": "user3"}
    }

    // Пример работы с операторами логического И и ИЛИ
    var flag: bool = true
    if (flag && ops > 1) {
        print!("Флаг установлен, и операций больше одного!")
    }

    // Пример использования типа данных float и операторов
    var area: float = PI * (a * a)
    print!(area.to_str())  // Выводит площадь круга с радиусом a

    // Пример работы с комплексными типами данных
    var dictionary = {"key1": "value1", "key2": "value2"}
    print!(dictionary.to_str())  // Выводит словарь

    // Пример работы с try/handle
    try {
        var unknown_var = 10 / 0
    } handle {
        print!("Ошибка деления на ноль!")
    }
}
```

## Лицензия

Этот проект лицензирован под GNU LICENSE - подробности см. в файле [LICENSE](LICENSE).
