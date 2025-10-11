//! The actual compilation.

use crate::ir::owned::{ExternalFunctionSignature, ModuleAnnotation, StructDef};

use super::{CodegenPacket, ResultPacket};
use std::sync::mpmc::{Receiver, Sender};

pub fn compile_thread(
    new_block_chan: Receiver<CodegenPacket>,
    result_chan: Sender<ResultPacket>,
    annotations: Vec<ModuleAnnotation>,
    structs: Vec<StructDef>,
    extern_funcs: Vec<ExternalFunctionSignature>,
    verbose: bool,
    codegen_opts: Vec<(String, Option<String>)>,
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
                    verbose,
                    codegen_opts.as_ref(),
                ))
                .unwrap()
        }
    }
}

pub fn compile<'a>(
    packet: CodegenPacket,
    annotations: &'a [ModuleAnnotation],
    structs: &'a [StructDef],
    extern_funcs: &'a [ExternalFunctionSignature],
    verbose: bool,
    codegen_opts: &'a [(&'a str, Option<&'a str>)],
) -> ResultPacket {
}
