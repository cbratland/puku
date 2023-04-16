use super::{BasicType, Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Option<String>,
    pub r#type: Type,
}

impl Symbol {
    pub fn var(name: String, typ: Type) -> Self {
        Symbol {
            name: Some(name),
            r#type: typ,
        }
    }

    pub fn typ(typ: Type) -> Self {
        Symbol {
            name: None,
            r#type: typ,
        }
    }

    pub fn func(name: String, typ: Type) -> Self {
        Self::var(name, typ)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
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
        ] {
            symbols.insert(symbol.0.to_string(), Symbol::typ(symbol.1));
        }

        SymbolTable {
            scopes: vec![symbols],
        }
    }

    pub fn get(&self, key: &str) -> Option<Symbol> {
        for table in self.scopes.iter().rev() {
            if let Some(entry) = table.get(key) {
                return Some(entry.clone());
            }
        }
        None
    }

    pub fn insert(&mut self, key: &str, entry: Symbol) {
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

    fn current(&mut self) -> &mut HashMap<String, Symbol> {
        let index = self.scopes.len() - 1;
        &mut self.scopes[index]
    }
}
