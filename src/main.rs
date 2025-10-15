//! New Rust Project
#![warn(
    missing_docs,
    clippy::missing_docs_in_private_items,
    clippy::pedantic,
    clippy::all,
    clippy::ignore_without_reason,
    clippy::alloc_instead_of_core,
    clippy::as_underscore,
    clippy::assertions_on_result_states
)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::cast_possible_wrap,
    clippy::too_many_lines,
    reason = "unwanted"
)]

use std::{fmt::Write as _, fs::File};

use qu::{
    codegen::{Codegen, x86_64_linux_hardfloat::CodegenX64LinuxHardFloat},
    ir::{FunctionDef, Location, ModuleItem, parse::Parse as _},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let tokens = qu::ir::parse::tokenizer::read_tokens(
        &mut File::open("example.qir")?,
        &Location {
            line: Some(1),
            column: Some(1),
            index: Some(0),
            file: Some("example.qir".into()),
        },
    )?;

    std::fs::write("example.tokens", format!("{tokens:#?}"))?;

    let mut i = 0usize;

    let mut parsed_str = String::new();
    let mut items = Vec::new();

    while i < tokens.len() {
        let (v, len) = ModuleItem::debug_output((), &tokens[i..]).unwrap();
        writeln!(parsed_str, "{v:#?}")?;
        i += len;
        items.push(v);
    }

    std::fs::write("example.parsed", parsed_str)?;

    let mut codegen = CodegenX64LinuxHardFloat::default();

    let funcs = items
        .iter()
        .filter_map(|v| match v {
            ModuleItem::Func(def) => Some(def.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let extern_funcs = funcs
        .iter()
        .filter_map(|v| match v {
            qu::ir::FunctionDef::External {
                calling_conv: _,
                sig,
                external_name: _,
            } => Some(sig.clone().into()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let intern_funcs = funcs
        .iter()
        .filter_map(|v| match v {
            qu::ir::FunctionDef::Internal {
                sig,
                func_vars: _,
                code: _,
            } => Some(sig.clone().into()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let structs = items
        .iter()
        .filter_map(|v| match v {
            ModuleItem::StructDef(def) => Some(def.clone().into()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let annotations = items
        .iter()
        .filter_map(|v| match v {
            ModuleItem::ModuleAnnotation(anno) => Some(anno.clone().into()),
            _ => None,
        })
        .collect::<Vec<_>>();

    codegen
        .prepare(
            &annotations,
            &structs,
            &extern_funcs,
            &intern_funcs,
            1,
            true,
            &[],
            "example.qir",
        )
        .unwrap();
    
    let mut all_func_locals: Vec<Vec<(String, qu::ir::owned::Type)>> = Vec::new();
    let mut mapping = Vec::new();

    for fun in &funcs {
        if let FunctionDef::Internal {
            sig: _,
            func_vars,
            code: _,
        } = fun
        {
            all_func_locals.push(func_vars
                            .iter()
                            .map(|v| (v.0.clone().into(), v.1.clone().into()))
                            .collect::<Vec<_>>());

            mapping.push(all_func_locals.len()-1);
        } else {
            mapping.push(0);
        }
    }

    for (i, fun) in funcs.into_iter().enumerate() {
        if let FunctionDef::Internal {
            sig,
            func_vars: _,
            code,
        } = fun
        {
            let sig: qu::ir::owned::InternalFunctionSignature = sig.into();
            for block in code.into_owned() {
                let is_entry = block.name == sig.name;
                let func_locals = all_func_locals[mapping[i]].as_slice();

                codegen
                    .compile(
                        &sig,
                        func_locals,
                        block.into(),
                        is_entry,
                    )
                    .unwrap();
            }
        }
    }

    codegen.finish_compilation().unwrap();

    std::fs::write("example.asm", codegen.emit_asm().unwrap()).unwrap();

    Ok(())
}
