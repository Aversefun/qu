//! The actual compilation.

use crate::{
    codegen::register_alloc::{PhysicalRegister, allocate_single},
    ir::{
        Primitive,
        owned::{
            Block, ExternalFunctionSignature, InternalFunctionSignature, ModuleAnnotation,
            NoProdInstruction, StructDef,
        },
    },
};

use super::{CodegenPacket, ResultPacket};
use std::sync::mpmc::{Receiver, Sender};

pub fn compile_thread(
    new_block_chan: Receiver<CodegenPacket>,
    result_chan: Sender<ResultPacket>,
    annotations: Vec<ModuleAnnotation>,
    structs: Vec<StructDef>,
    extern_funcs: Vec<ExternalFunctionSignature>,
    intern_funcs: Vec<InternalFunctionSignature>,
    verbose: bool,
    codegen_opts: Vec<(String, Option<String>)>,
    input_name: String,
) -> impl FnOnce() {
    move || {
        let codegen_opts = codegen_opts
            .iter()
            .map(|v| (v.0.as_str(), v.1.as_ref().map(String::as_str)))
            .collect::<Vec<_>>();
        for packet in new_block_chan.iter() {
            result_chan
                .send(compile(
                    packet,
                    annotations.as_ref(),
                    structs.as_ref(),
                    extern_funcs.as_ref(),
                    intern_funcs.as_ref(),
                    verbose,
                    codegen_opts.as_ref(),
                    &input_name,
                ))
                .unwrap()
        }
    }
}

pub fn mangle_name(
    sig: &InternalFunctionSignature,
    block: Option<&str>,
    input_name: &str,
) -> String {
    let InternalFunctionSignature {
        annotations: _,
        name,
        params,
        result,
    } = sig;
    let input_name = input_name.replace(|c: char| !c.is_alphanumeric(), "_");
    format!(
        "_f{input_name}__{name}__{}__{}{}",
        params.len(),
        if let Some(res) = result {
            let s = format!("_{res:?}");
            s[..(5.min(s.len()))].to_string()
        } else {
            "Void".to_string()
        },
        if let Some(block_name) = block {
            format!("__b{block_name}")
        } else {
            String::new()
        }
    )
}

macro_rules! normal_reg {
    ($start:literal) => {
        PhysicalRegister::PartOf {
            id: $start,
            size: 8,
            part_of: &[($start)+1, ($start)+2, ($start)+3, ($start)+4],
        },
        PhysicalRegister::PartOf {
            id: ($start)+1,
            size: 4,
            part_of: &[($start), ($start)+2, ($start)+3, ($start)+4],
        },
        PhysicalRegister::PartOf {
            id: ($start)+2,
            size: 2,
            part_of: &[($start), ($start)+1, ($start)+3, ($start)+4],
        },
        PhysicalRegister::PartOf {
            id: ($start)+3,
            size: 1,
            part_of: &[($start), ($start)+1, ($start)+2],
        },
        PhysicalRegister::PartOf {
            id: ($start)+4,
            size: 1,
            part_of: &[($start), ($start)+1, ($start)+2],
        }
    };
}

const REGISTERS: &[PhysicalRegister] = &[
    normal_reg!(0) // Acc
];

pub fn compile<'a>(
    packet: CodegenPacket,
    annotations: &'a [ModuleAnnotation],
    structs: &'a [StructDef],
    extern_funcs: &'a [ExternalFunctionSignature],
    intern_funcs: &'a [InternalFunctionSignature],
    verbose: bool,
    codegen_opts: &'a [(&'a str, Option<&'a str>)],
    input_name: &'a str,
) -> ResultPacket {
    let CodegenPacket {
        sig,
        func_locals,
        block,
        is_entry,
        i,
    } = packet;

    let mut result = String::new();

    let Block {
        name: block_name,
        instructions,
    } = block;

    if is_entry {
        let InternalFunctionSignature {
            annotations,
            name,
            params: _,
            result: _,
        } = sig;
        if let Some(export) = annotations.iter().find(|v| v.is_export()) {
            let (_, name) = export.unwrap_export();
            result.push_str(&format!("{name}:"));
        } else {
            result.push_str(&mangle_name(&sig, None, input_name));
            result.push(':');
            result.push('\n');
        }
    }

    result.push_str(&mangle_name(&sig, Some(&block_name), input_name));
    result.push(':');
    result.push('\n');

    let mut var_sizes = func_locals
        .iter()
        .map(|v| {
            (
                v.0,
                v.1,
                v.1.get_size()
                    .unwrap_or(Primitive::Uptr.get_size().unwrap()),
            )
        })
        .collect::<Vec<_>>();

    for instr in instructions {
        match instr {
            NoProdInstruction::VarDef(name, ty) => var_sizes.push((
                name,
                ty,
                ty.get_size().unwrap_or(Primitive::Uptr.get_size().unwrap()),
            )),
            _ => {}
        }
        allocate_single(registers, vars)
    }
}
