//! Codegen for x86-64 linux.

use crate::{
    codegen::Codegen,
    errors::CodegenError,
    impl_is, impl_unwrap,
    ir::owned::{
        Block, ExternalFunctionSignature, InternalFunctionSignature, ModuleAnnotation, StructDef,
    },
};

use std::{
    process::Command, sync::mpmc::{channel, Receiver, Sender}, thread::{self, JoinHandle}
};

mod compile;

fn format_block_name(func: impl AsRef<str>, block: impl AsRef<str>) -> String {
    let func = func.as_ref();
    let block = block.as_ref();
    format!("{func}_{block}")
}

fn format_block_name_cpac(packet: &CodegenPacket) -> String {
    format_block_name(&packet.sig.name, &packet.block.name)
}

fn format_block_name_rpac(packet: &OkResultPacket) -> String {
    format_block_name(&packet.func, &packet.block)
}

#[derive(Debug, Clone, PartialEq)]
struct CodegenPacket {
    sig: InternalFunctionSignature,
    block: Block,
    is_entry: bool,
    i: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct OkResultPacket {
    func: String,
    block: String,
    asm: String,
    i: usize,
}

type ResultPacket = Result<OkResultPacket, CodegenError<'static>>;

#[derive(Debug)]
enum CodegenType {
    Threaded {
        new_block_chan: Sender<CodegenPacket>,
        result_chan: Receiver<ResultPacket>,
        threads: Vec<JoinHandle<()>>,
    },
    SingleThreaded {
        results: Vec<OkResultPacket>,
    },
}

impl<'a> CodegenType {
    impl_unwrap!(
        CodegenType,
        Vec<OkResultPacket>,
        CodegenType::SingleThreaded,
        CodegenType::SingleThreaded { results } => results,
        single_threaded
    );
    impl_is!(CodegenType::SingleThreaded { results: _ }, single_threaded);

    impl_is!(
        CodegenType::Threaded {
            new_block_chan: _,
            result_chan: _,
            threads: _
        },
        threaded
    );
}

#[derive(Debug)]
struct CodegenState<'a> {
    annotations: &'a [ModuleAnnotation],
    structs: &'a [StructDef],
    extern_funcs: &'a [ExternalFunctionSignature],
    verbose: bool,
    codegen_opts: &'a [(&'a str, Option<&'a str>)],
    codegen_ty: CodegenType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LinkFlavor {
    Gnu,
    Darwin,
    MSVC,
}

#[derive(Debug, Default)]
pub struct CodegenX64Linux<'a> {
    state: Option<CodegenState<'a>>,
    next_i: usize,
}

impl<'a> Codegen<'a> for CodegenX64Linux<'a> {
    const BIN_EXTENSION: &'static str = "";
    const CAN_OUTPUT_ASM: bool = true;
    const CAN_OUTPUT_BIN: bool = true;
    const CODEGEN_OPTS: &'static [(&'static str, &'static str)] = &[
        (
            "asm=(path)",
            "sets the assembler to use - searches the path for it as needed",
        ),
        (
            "link=(path)",
            "sets the linker to use - searches the path for it as needed",
        ),
        (
            "link_flavor=(gnu|darwin|msvc)",
            "sets the flavor of the linker to use",
        ),
    ];
    const MAX_BIT_WIDTH: u8 = 64;
    fn prepare(
        &mut self,
        annotations: &'a [ModuleAnnotation],
        structs: &'a [StructDef],
        extern_funcs: &'a [ExternalFunctionSignature],
        codegen_units: usize,
        verbose: bool,
        codegen_opts: &'a [(&'a str, Option<&'a str>)],
    ) -> Result<(), crate::errors::CodegenError<'a>> {
        if codegen_units == 0 {
            return Err(crate::errors::CodegenError::ZeroCodegenUnits);
        }

        let thread_codegen_opts: Vec<_> = codegen_opts.iter().map(|v| (v.0.to_string(), v.1.map(ToString::to_string))).collect();

        let codegen_ty = if codegen_units > 1 {
            let mut threads = Vec::new();
            let (compile_send, compile_recv) = channel();
            let (result_send, result_recv) = channel();
            for _ in 0..codegen_units {
                threads.push(thread::spawn(compile::compile_thread(
                    compile_recv.clone(),
                    result_send.clone(),
                    annotations.to_vec(),
                    structs.to_vec(),
                    extern_funcs.to_vec(),
                    verbose,
                    thread_codegen_opts.clone(),
                )));
            }
            drop(result_send);
            drop(compile_recv);
            CodegenType::Threaded {
                new_block_chan: compile_send,
                result_chan: result_recv,
                threads,
            }
        } else {
            CodegenType::SingleThreaded {
                results: Vec::new(),
            }
        };

        self.state = Some(CodegenState {
            annotations,
            structs,
            extern_funcs,
            verbose,
            codegen_opts,
            codegen_ty,
        });

        Ok(())
    }
    fn compile(
        &mut self,
        sig: InternalFunctionSignature,
        block: Block,
        is_entry: bool,
    ) -> Result<(), crate::errors::CodegenError<'a>> {
        let state = self.state.as_mut().unwrap();
        let packet = CodegenPacket {
            sig,
            block,
            is_entry,
            i: self.next_i,
        };
        self.next_i += 1;
        match state.codegen_ty {
            CodegenType::Threaded {
                ref mut new_block_chan,
                result_chan: _,
                threads: _,
            } => new_block_chan.send(packet)?,
            CodegenType::SingleThreaded { ref mut results } => results.push(compile::compile(
                packet,
                state.annotations,
                state.structs,
                state.extern_funcs,
                state.verbose,
                state.codegen_opts,
            )?),
        }
        Ok(())
    }
    fn finish_compilation(&mut self) -> Result<(), CodegenError<'a>> {
        let mut state = self.state.take().unwrap();
        match state.codegen_ty {
            CodegenType::Threaded {
                new_block_chan: _,
                result_chan,
                threads,
            } => {
                for thread in threads {
                    let _ = thread.join();
                }
                let mut results = Vec::new();
                for result in result_chan.iter() {
                    results.push(result?);
                }
                results.sort_by(|v1, v2| v1.i.cmp(&v2.i));
                state.codegen_ty = CodegenType::SingleThreaded { results };
            }
            CodegenType::SingleThreaded { results: _ } => {}
        }
        self.state = Some(state);
        Ok(())
    }
    fn output_asm(&mut self) -> Result<String, CodegenError<'a>> {
        Ok(self
            .state
            .take()
            .unwrap()
            .codegen_ty
            .unwrap_single_threaded()
            .iter()
            .map(|v| {
                format!(
                    "{}: ; block {}/{}\n    {}\n\n",
                    format_block_name_rpac(v),
                    v.i,
                    self.next_i,
                    v.asm
                )
            })
            .collect())
    }
    fn output_bin(&mut self) -> Result<Vec<u8>, CodegenError<'a>> {
        let asm = self.output_asm()?;
        let mut assembler = None;
        let mut linker_flavor = None;
        let mut linker = None;
        for (opt, value) in self.state.unwrap().codegen_opts {
            match opt.to_lowercase().as_str() {
                "asm" => assembler = Some(value.ok_or(CodegenError::InvalidOption("asm".to_string(), "expected a value to be provided for assembler argument".to_string()))?),
                "link" => linker = Some(value.ok_or(CodegenError::InvalidOption("link".to_string(), "expected a value to be provided for linker argument".to_string()))?),
                "link_flavor" => linker_flavor = Some(match value.ok_or(CodegenError::InvalidOption("link_flavor".to_string(), "expected a value to be provided for linker flavor argument".to_string()))?.to_lowercase().as_str() {
                    "gnu" => LinkFlavor::Gnu,
                    "darwin" => LinkFlavor::Darwin,
                    "msvc" => LinkFlavor::MSVC,
                    _ => return Err(CodegenError::InvalidOption("link_flavor".to_string(), "invalid linker flavor, expected one of gnu,darwin,msvc".to_string()))
                }),
                v => return Err(CodegenError::UnknownOption(v.to_string()))
            }
        }

        if linker.is_some() ^ linker_flavor.is_some() {
            return Err(CodegenError::Generic("if one of link, link_flavor is passed both must be passed".to_string()));
        }

        let (linker, linker_flavor) = linker.map(|v| Ok((v, linker_flavor.unwrap()))).unwrap_or_else(|| {
            if Command::new("LINK.exe")
                .arg("/?")
                .output().map(|v| {
                    let stdout = str::from_utf8(&v.stdout);
                    v.status.exit_ok().is_ok() && stdout.is_ok() && stdout.unwrap().contains(&"Microsoft")
                }).unwrap_or(false) {
                return Ok(("LINK.exe", LinkFlavor::MSVC));
            }

            if Command::new("ld64")
                .arg("-v")
                .output().map(|v| {
                    let stdout = str::from_utf8(&v.stdout);
                    v.status.exit_ok().is_ok() && stdout.is_ok() && stdout.unwrap().contains(&"PROJECT:ld64")
                }).unwrap_or(false) {
                return Ok(("ld64", LinkFlavor::Darwin));
            }
            
            if Command::new("ld")
                .arg("--version")
                .output().map(|v| {
                    let stdout = str::from_utf8(&v.stdout);
                    v.status.exit_ok().is_ok() && stdout.is_ok() && stdout.unwrap().contains(&"GNU ld")
                }).unwrap_or(false) {
                return Ok(("ld", LinkFlavor::Gnu));
            }

            Err(CodegenError::Generic("cannot find linker; please specify manually".to_string()))
        })?;

        let assembler = assembler.map(|v| Ok(v)).unwrap_or_else(|| {
            if Command::new("ml64.exe")
                .arg("/?")
                .output().map(|v| {
                    let stdout = str::from_utf8(&v.stdout);
                    v.status.exit_ok().is_ok() && stdout.is_ok() && stdout.unwrap().contains(&"Microsoft")
                }).unwrap_or(false) {
                return Ok("ml64.exe");
            }

            if Command::new("as")
                .arg("--version")
                .output().map(|v| {
                    let stdout = str::from_utf8(&v.stdout);
                    v.status.exit_ok().is_ok() && stdout.is_ok() && stdout.unwrap().contains(&"GNU assembler")
                }).unwrap_or(false) {
                return Ok("as");
            }

            Err(CodegenError::Generic("cannot find assembler; please specify manually".to_string()))
        })?;

        
    }
}

impl From<std::sync::mpmc::SendError<CodegenPacket>> for CodegenError<'_> {
    fn from(_: std::sync::mpmc::SendError<CodegenPacket>) -> Self {
        Self::ThreadCommError(
            "failed to send codegen packet; all threads are likely dead".to_string(),
        )
    }
}
