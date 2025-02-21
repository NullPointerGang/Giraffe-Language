use::GiraffeVariablesStorage::{VariablesStorage, DataValue, DataType};

fn main() {
    let mut env = VariablesStorage::new();

    env.define("pi".to_string(), DataType::Float, DataValue::Float(3.14));
    env.define("name".to_string(), DataType::Str, DataValue::Str("Giraffe".to_string()));
    env.define("is_active".to_string(), DataType::Bool, DataValue::Bool(true));

    if let Some(var) = env.get("pi") {
        println!("pi = {:?}", var.value); // pi = Float(3.14)
    }

    if let Some(var) = env.get("name") {
        println!("name = {:?}", var.value); // name = Str("Giraffe")
    }

    if let Some(var) = env.get("is_active") {
        println!("is_active = {:?}", var.value); // is_active = Bool(true)
    }

    if let Err(e) = env.set("pi", DataValue::Float(3.14159)) {
        println!("{}", e);
    }

    if let Some(var) = env.get("pi") {
        println!("pi после обновления = {:?}", var.value); // pi после обновления = Float(3.14159)
    }

    if let Some(var) = env.remove("is_active") {
        println!("Удалена переменная: {:?}", var); // Удалена переменная: Variable { name: "is_active", data_type: Bool, value: Bool(true) }
    }
    
    if env.get("is_active").is_none() {
        println!("Переменная 'is_active' удалена."); // Переменная 'is_active' удалена.
    }
}