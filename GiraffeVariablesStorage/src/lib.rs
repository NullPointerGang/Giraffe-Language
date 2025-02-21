use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum DataType {
    Int,
    Str,
    Float,
    Bool,
    List,
    Dict,
    Set,
    Tuple,
    Null,
    Option,
    Error,
    Function,
}

#[derive(Debug, Clone)]
pub enum DataValue {
    Int(i64),
    Str(String),
    Float(f64),
    Bool(bool),
    List(Vec<DataValue>),
    Dict(HashMap<String, DataValue>),
    Set(Vec<DataValue>),
    Tuple(Vec<DataValue>),
    Null,
    Option(Box<DataValue>),
    Error(String),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub data_type: DataType,
    pub value: DataValue,
}

pub struct VariablesStorage {
    variables: HashMap<String, Variable>,
}

impl VariablesStorage {
    pub fn new() -> Self {
        VariablesStorage {
            variables: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, data_type: DataType, value: DataValue) {
        let var = Variable {
            name: name.clone(),
            data_type,
            value,
        };
        self.variables.insert(name, var);
    }

    pub fn get(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }

    pub fn set(&mut self, name: &str, value: DataValue) -> Result<(), String> {
        if let Some(var) = self.variables.get_mut(name) {
            var.value = value;
            Ok(())
        } else {
            Err(format!("Переменная '{}' не найдена", name))
        }
    }

    pub fn remove(&mut self, name: &str) -> Option<Variable> {
        self.variables.remove(name)
    }
}
