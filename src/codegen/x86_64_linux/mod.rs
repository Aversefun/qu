//! Codegen for x86-64 linux.

use crate::{codegen::Codegen, ir::{code::Block, ExternalFunctionSignature, InternalFunctionSignature, ModuleAnnotation, StructDef}, Str};

use std::{sync::{mpmc::{channel, Receiver, Sender}}, thread::{self, JoinHandle}};

mod compile;

#[derive(Debug, Clone, PartialEq)]
struct CodegenPacket<'a> {
    sig: InternalFunctionSignature<'a>,
    block: Block<'a>,
    is_entry: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct ResultPacket {
    func: String,
    asm: String,
}

#[derive(Debug)]
struct CodegenState<'a, 'b> {
    annotations: &'a [ModuleAnnotation<'a>],
    structs: &'a [StructDef<'a>],
    extern_funcs: &'a [ExternalFunctionSignature<'a>],
    verbose: bool,
    codegen_opts: &'a [&'a str],
    new_block_chan: Option<Sender<CodegenPacket<'b>>>,
    result_chan: Option<Receiver<ResultPacket>>,
    threads: Option<Vec<JoinHandle<()>>>,
}

#[derive(Debug, Default)]
pub struct CodegenX64Linux<'a, 'b> {
    state: Option<CodegenState<'a, 'b>>,
}

impl<'a, 'b> Codegen<'a> for CodegenX64Linux<'a, 'b> {
    const BIN_EXTENSION: &'static str = "";
    const CAN_OUTPUT_ASM: bool = true;
    fn prepare(
            &mut self,
            annotations: &'a [crate::ir::ModuleAnnotation],
            structs: &'a [crate::ir::StructDef],
            extern_funcs: &'a [crate::ir::ExternalFunctionSignature],
            codegen_units: usize,
            verbose: bool,
            codegen_opts: &'a [&'a str],
        ) -> Result<(), crate::errors::CodegenError<'a>> {
        if codegen_units == 0 {
            return Err(crate::errors::CodegenError::ZeroCodegenUnits);
        }

        let (threads, new_block_chan, result_chan) = if codegen_units > 1 {
            let mut threads = Vec::new();
            let (compile_send, compile_recv) = channel();
            let (result_send, result_recv) = channel();
            for _ in 0..codegen_units {
                threads.push(thread::spawn(compile::compile_thread(compile_recv.clone(), result_send.clone())));
            }
            (Some(threads), Some(compile_send), Some(result_recv))
        } else {
            (None, None, None)
        };

        self.state = Some(
            CodegenState { annotations, structs, extern_funcs, verbose, codegen_opts, new_block_chan, result_chan, threads }
        );

        Ok(())
    }
}