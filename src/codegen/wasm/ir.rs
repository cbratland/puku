// wasm ir

#[repr(u8)]
#[derive(Debug)]
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
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
    V128 = 0x7B,
}

#[derive(Debug)]
pub struct Type {
    pub params: Vec<Valtype>,
    pub returns: Vec<Valtype>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum ExternalKind {
    Function = 0,
    Table,
    Memory,
    Global,
}

#[derive(Debug)]
pub struct GlobalType {
    pub valtype: Valtype,
    pub mutable: bool,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Import {
    pub module_name: String,
    pub name: String,
    pub kind: ImportKind,
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<(Valtype, u32)>, // variable type, count
    pub body: Vec<u8>,
}

#[derive(Debug)]
pub struct Function {
    pub type_index: u8,
    pub code: Code,
}

#[derive(Debug)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum RefType {
    Func = 0x70,
    Extern = 0x6F,
}

#[derive(Debug)]
pub struct Table {
    pub limits: Limits,
    pub reftype: RefType,
}

#[derive(Debug)]
pub struct Memory {
    pub limits: Limits,
}

#[derive(Debug)]
pub enum InitExpression {
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    GlobalGet(u32),
}

#[derive(Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub init: InitExpression,
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub kind: ExternalKind,
    pub index: u32,
}

#[derive(Debug)]
pub struct Element {
    pub table_offset: u32,
    pub offset: InitExpression,
    pub func_indexes: Vec<u32>,
}

#[derive(Debug)]
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

    MemoryGrow = 0x40,

    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,

    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4A,
    I32GtU = 0x4B,
    I32LeS = 0x4C,
    I32LeU = 0x4D,
    I32GeS = 0x4E,
    I32GeU = 0x4F,

    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5A,

    F32Eq = 0x5B,
    F32Ne = 0x5C,
    F32Lt = 0x5D,
    F32Gt = 0x5E,
    F32Le = 0x5F,
    F32Ge = 0x60,

    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,

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
    I64And = 0x83,
    I64Or = 0x84,

    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,

    F64Add = 0xA0,
    F64Sub = 0xA1,
    F64Mul = 0xA2,
    F64Div = 0xA3,
}
