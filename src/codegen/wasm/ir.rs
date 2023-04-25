// wasm ir

#[repr(u8)]
pub enum Section {
    Custom = 0,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
    DataCount,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Valtype {
    I32 = 0x7F,
    I64,
    F32,
    F64,
    V128,
}

pub struct Type {
    pub params: Vec<Valtype>,
    pub returns: Vec<Valtype>,
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum ExternalKind {
    Function = 0,
    Table,
    Memory,
    Global,
}

pub struct GlobalType {
    pub valtype: Valtype,
    pub mutable: bool,
}

pub enum ImportKind {
    Function(u32),
    Table(Table),
    Memory(Limits),
    Global(GlobalType),
}

impl ImportKind {
    pub fn raw_value(&self) -> u8 {
        match self {
            Self::Function(_) => 0,
            Self::Table(_) => 1,
            Self::Memory(_) => 2,
            Self::Global(_) => 3,
        }
    }
}

pub struct Import {
    pub module_name: String,
    pub name: String,
    pub kind: ImportKind,
}

pub struct Code {
    pub locals: Vec<(Valtype, u32)>, // variable type, count
    pub body: Vec<u8>,
}

pub struct Function {
    pub type_index: u8,
    pub code: Code,
}

pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum RefType {
    Func = 0x70,
    Extern = 0x6F,
}

pub struct Table {
    pub limits: Limits,
    pub reftype: RefType,
}

pub struct Memory {
    pub limits: Limits,
}

pub enum InitExpression {
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    GlobalGet(u32),
}

pub struct Global {
    pub global_type: GlobalType,
    pub init: InitExpression,
}

pub struct Export {
    pub name: String,
    pub kind: ExternalKind,
    pub index: u32,
}

pub struct Element {
    pub table_offset: u32,
    pub offset: InitExpression,
    pub func_indexes: Vec<u32>,
}

pub struct Module {
    pub types: Option<Vec<Type>>,
    pub imports: Option<Vec<Import>>,
    pub functions: Option<Vec<Function>>,
    pub tables: Option<Vec<Table>>,
    pub memories: Option<Vec<Memory>>,
    pub globals: Option<Vec<Global>>,
    pub exports: Option<Vec<Export>>,
    pub elements: Option<Vec<Element>>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            types: None,
            imports: None,
            functions: None,
            tables: None,
            // wasm v1 only supports one memory
            memories: Some(vec![Memory {
                limits: Limits { min: 0, max: None },
            }]),
            globals: None,
            exports: None,
            elements: None,
        }
    }
}

#[repr(u8)]
pub enum Opcode {
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    Return = 0x0F,
    Call = 0x10,
    CallIndirect = 0x11,
    Drop = 0x1A,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,

    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,

    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LeS = 0x4C,
    I32LeU = 0x4D,

    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LeS = 0x57,
    I64LeU = 0x58,

    F32Eq = 0x5B,
    F32Ne = 0x5C,
    F32Le = 0x5F,

    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Le = 0x65,

    I32Add = 0x6A,
    I32Sub = 0x6B,
    I32Mul = 0x6C,
    I32DivS = 0x6D,
    I32DivU = 0x6E,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,

    I64Add = 0x7C,
    I64Sub = 0x7D,
    I64Mul = 0x7E,
    I64DivS = 0x7F,
    I64DivU = 0x80,

    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,

    F64Add = 0xA0,
    F64Sub = 0xA1,
    F64Mul = 0xA2,
    F64Div = 0xA3,
}
