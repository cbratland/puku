use super::{BasicType, Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub name: Option<String>,
    pub r#type: Type,
}

impl TypeSymbol {
    // variable with type
    pub fn var(name: String, typ: Type) -> Self {
        TypeSymbol {
            name: Some(name),
            r#type: typ,
        }
    }

    // just type
    pub fn typ(typ: Type) -> Self {
        TypeSymbol {
            name: None,
            r#type: typ,
        }
    }

    // function with type
    pub fn func(name: String, typ: Type) -> Self {
        Self::var(name, typ)
    }
}

#[derive(Debug)]
pub struct SymbolTable<T: Clone> {
    scopes: Vec<HashMap<String, T>>,
}

impl SymbolTable<TypeSymbol> {
    pub fn default() -> Self {
        // add builtin types
        let mut symbols = HashMap::new();
        for symbol in &[
            ("bool", Type::Basic(BasicType::Bool)),
            ("i8", Type::Basic(BasicType::Int8)),
            ("i16", Type::Basic(BasicType::Int16)),
            ("i32", Type::Basic(BasicType::Int32)),
            ("i64", Type::Basic(BasicType::Int64)),
            ("u8", Type::Basic(BasicType::UInt8)),
            ("u16", Type::Basic(BasicType::UInt16)),
            ("u32", Type::Basic(BasicType::UInt32)),
            ("u64", Type::Basic(BasicType::UInt64)),
            ("f32", Type::Basic(BasicType::Float32)),
            ("f64", Type::Basic(BasicType::Float64)),
        ] {
            symbols.insert(symbol.0.to_string(), TypeSymbol::typ(symbol.1));
        }

        SymbolTable {
            scopes: vec![symbols],
        }
    }
}

impl<T: Clone> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable { scopes: Vec::new() }
    }

    pub fn get(&self, key: &str) -> Option<T> {
        for table in self.scopes.iter().rev() {
            if let Some(entry) = table.get(key) {
                return Some(entry.clone());
            }
        }
        None
    }

    pub fn insert(&mut self, key: &str, entry: T) {
        self.current().insert(String::from(key), entry);
    }

    pub fn remove(&mut self, key: &str) {
        self.current().remove(key);
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current(&mut self) -> &mut HashMap<String, T> {
        let index = self.scopes.len() - 1;
        &mut self.scopes[index]
    }
}
