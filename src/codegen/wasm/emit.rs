use super::ir::*;
use std::io::{Seek, Write};

use super::leb;

// errors
#[derive(Debug)]
pub enum EmitError {
    WriteError(std::io::Error),
}

impl From<std::io::Error> for EmitError {
    fn from(err: std::io::Error) -> Self {
        Self::WriteError(err)
    }
}

impl std::error::Error for EmitError {}

impl std::fmt::Display for EmitError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "EmitError") // todo
    }
}

type Result<T> = std::result::Result<T, EmitError>;

// emits a Module (wasm ir) to a buffer
pub fn emit<W: std::io::Write + std::io::Seek>(
    module: Module,
    buffer: &mut std::io::BufWriter<W>,
) -> Result<()> {
    let magic: &[u8; 4] = &[0x00, 0x61, 0x73, 0x6D]; // \0asm
    let version: &[u8; 4] = &[0x01, 0x00, 0x00, 0x00]; // v1

    let function_type = 0x60u8;

    buffer.write_all(magic)?;
    buffer.write_all(version)?;

    // type section
    if let Some(types) = module.types {
        if !types.is_empty() {
            // println!("writing type section");
            buffer.write(&[Section::Type as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, types.len() as u64)?;
            for type_obj in types {
                buffer.write(&[function_type])?;
                leb128::write::unsigned(buffer, type_obj.params.len() as u64)?;
                for param in type_obj.params {
                    buffer.write(&[param as u8])?;
                }
                leb128::write::unsigned(buffer, type_obj.returns.len() as u64)?;
                for ret in type_obj.returns {
                    buffer.write(&[ret as u8])?;
                }
            }

            write_header(buffer, header_offset)?;
        }
    }

    // import section
    if let Some(imports) = module.imports {
        if !imports.is_empty() {
            // println!("writing import section");
            buffer.write_all(&[Section::Import as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, imports.len() as u64)?;
            for import in imports {
                leb128::write::unsigned(buffer, import.module_name.len() as u64)?;
                buffer.write_all(import.module_name.as_bytes())?;
                leb128::write::unsigned(buffer, import.name.len() as u64)?;
                buffer.write_all(import.name.as_bytes())?;

                buffer.write_all(&[import.kind.raw_value()])?;
                match &import.kind {
                    ImportKind::Function(type_index) => {
                        leb128::write::unsigned(buffer, *type_index as u64)?;
                    }
                    ImportKind::Global(global_type) => {
                        leb128::write::unsigned(buffer, global_type.valtype as u64)?;
                        buffer.write_all(&[if global_type.mutable { 1 } else { 0 }])?;
                    }
                    ImportKind::Table(table) => {
                        leb128::write::unsigned(buffer, table.reftype as u64)?;
                        emit_limits(buffer, &table.limits)?;
                    }
                    ImportKind::Memory(limits) => {
                        emit_limits(buffer, limits)?;
                    }
                };
            }

            write_header(buffer, header_offset)?;
        }
    }

    // function section
    if let Some(functions) = &module.functions {
        if !functions.is_empty() {
            // println!("writing function section");
            buffer.write_all(&[Section::Function as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, functions.len() as u64)?;
            for func in functions {
                buffer.write_all(&[func.type_index])?;
            }

            write_header(buffer, header_offset)?;
        }
    }

    // memory section
    if let Some(memories) = module.memories {
        if !memories.is_empty() {
            // println!("writing memory section");
            buffer.write_all(&[Section::Memory as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, memories.len() as u64)?;
            for memory in memories {
                emit_limits(buffer, &memory.limits)?;
            }

            write_header(buffer, header_offset)?;
        }
    }

    // global section
    if let Some(globals) = module.globals {
        if !globals.is_empty() {
            // println!("writing global section");
            buffer.write_all(&[Section::Global as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, globals.len() as u64)?;
            for global in globals {
                buffer.write_all(&[global.global_type.valtype as u8])?;
                buffer.write_all(&[if global.global_type.mutable { 1 } else { 0 }])?;
                emit_init(buffer, &global.init)?;
            }

            write_header(buffer, header_offset)?;
        }
    }

    // export section
    if let Some(exports) = module.exports {
        if !exports.is_empty() {
            // println!("writing export section");
            buffer.write_all(&[Section::Export as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, exports.len() as u64)?;
            for export in exports {
                leb128::write::unsigned(buffer, export.name.len() as u64)?;
                buffer.write_all(export.name.as_bytes())?;
                buffer.write_all(&[export.kind as u8])?;
                leb128::write::unsigned(buffer, export.index as u64)?;
            }

            write_header(buffer, header_offset)?;
        }
    }

    // code section
    if let Some(functions) = module.functions {
        if !functions.is_empty() {
            // println!("writing code section");
            buffer.write(&[Section::Code as u8])?;

            let header_offset = reserve_header(buffer)?;

            leb128::write::unsigned(buffer, functions.len() as u64)?;
            for func in functions {
                let locals_len = func.code.locals.len();
                let func_len = func.code.body.len() + 2 + locals_len;
                leb128::write::unsigned(buffer, func_len as u64)?;
                leb128::write::unsigned(buffer, locals_len as u64)?;
                for local in func.code.locals {
                    buffer.write(&[local as u8])?;
                }
                buffer.write_all(&func.code.body)?;
                buffer.write(&[Opcode::End as u8])?;
            }

            write_header(buffer, header_offset)?;
        }
    }

    Ok(())
}

fn emit_init<W: std::io::Write>(
    buffer: &mut std::io::BufWriter<W>,
    init_expr: &InitExpression,
) -> Result<()> {
    match init_expr {
        InitExpression::I32Const(val) => {
            buffer.write(&[Opcode::I32Const as u8])?;
            leb128::write::signed(buffer, *val as i64)?;
        }
        InitExpression::I64Const(val) => {
            buffer.write(&[Opcode::I64Const as u8])?;
            leb128::write::signed(buffer, *val)?;
        }
        InitExpression::F32Const(val) => {
            buffer.write(&[Opcode::F32Const as u8])?;
            buffer.write_all(&(*val as u32).to_be_bytes())?;
        }
        InitExpression::F64Const(val) => {
            buffer.write(&[Opcode::F32Const as u8])?;
            buffer.write_all(&(*val as u64).to_be_bytes())?;
        }
        InitExpression::GlobalGet(val) => {
            buffer.write(&[Opcode::GlobalGet as u8])?;
            leb128::write::unsigned(buffer, *val as u64)?;
        }
    };
    buffer.write(&[Opcode::End as u8])?;
    Ok(())
}

fn emit_limits<W: std::io::Write>(
    buffer: &mut std::io::BufWriter<W>,
    limits: &Limits,
) -> Result<()> {
    leb128::write::unsigned(buffer, if limits.max.is_some() { 1 } else { 0 })?;
    leb128::write::unsigned(buffer, limits.min as u64)?;
    if let Some(max) = limits.max {
        leb128::write::unsigned(buffer, max as u64)?;
    }
    Ok(())
}

fn reserve_header<W: std::io::Write + std::io::Seek>(
    buffer: &mut std::io::BufWriter<W>,
) -> Result<u64> {
    let pos = buffer.stream_position()?;
    buffer.seek(std::io::SeekFrom::Current(5))?;
    Ok(pos)
}

fn write_header<W: std::io::Write + std::io::Seek>(
    buffer: &mut std::io::BufWriter<W>,
    offset: u64,
) -> Result<()> {
    let curr = buffer.stream_position()?;
    buffer.seek(std::io::SeekFrom::Start(offset))?;
    let section_size = (curr - offset - 5) as u32;
    leb::write_unsigned_fixed(buffer, 5, section_size as u64)?;
    buffer.seek(std::io::SeekFrom::Start(curr))?;
    Ok(())
}
