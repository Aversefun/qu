//! The codegen itself.

use crate::errors::CodegenError;
use crate::ir::owned::{
    ExternalFunctionSignature, InternalFunctionSignature, ModuleAnnotation, StructDef,
    Block
};

use std::fmt::Debug;

/// A codegen.
pub trait Codegen<'a>: Default + Debug {
    /// The default binary file extension on this target.
    const BIN_EXTENSION: &'static str;
    /// Whether this codegen can output assembly.
    const CAN_OUTPUT_ASM: bool;
    /// Prepare internal state with annotations, structs, external functions,
    /// the number of codegen units, and other flags passed to the compiler.
    /// 
    /// If `codegen_units` is more than one, create that number of threads to
    /// compile right away.
    ///
    /// # Errors
    /// This function should NEVER leave the type this is implemented on in an
    /// invalid state or panic. This function should propogate all errors.
    fn prepare(
        &mut self,
        annotations: &'a [ModuleAnnotation],
        structs: &'a [StructDef],
        extern_funcs: &'a [ExternalFunctionSignature],
        codegen_units: usize,
        verbose: bool,
        codegen_opts: &'a [&'a str],
    ) -> Result<(), CodegenError<'a>>;
    /// Compile a single block. If the `codegen_units` parameter passed to
    /// [`prepare`](Codegen::prepare) is more than one, delegate a thread
    /// to compile this block.
    ///
    /// This function should only block if one codegen unit is being used.
    ///
    /// # Errors
    /// This function should NEVER leave the type this is implemented on in an
    /// invalid state or panic. This function should only return errors
    /// encountered when spawning the thread if more than one codegen unit is
    /// being used, and otherwise return any codegen errors.
    fn compile(
        &mut self,
        sig: InternalFunctionSignature,
        block: Block,
        is_entry: bool,
    ) -> Result<(), CodegenError<'a>>;
    /// Finish compilation, blocking on any codegen threads remaining. This
    /// should NOT assemble/link the output, and that step should instead be
    /// done if [`output_bin`](Codegen::output_bin) is called.
    ///
    /// This function should block and wait for any codegen units that are
    /// still active.
    ///
    /// # Errors
    /// This function should NEVER leave the type this is implemented on in an
    /// invalid state or panic. This function should propogate all errors.
    fn finish_compilation(&mut self) -> Result<(), CodegenError<'a>>;
    /// Produces a flat assembly output after codegen. The default
    /// implementation returns [`CodegenError::CannotCompileToAsm`].
    ///
    /// # Errors
    /// This function should NEVER leave the type this is implemented on in an
    /// invalid state or panic. This function should propogate all errors.
    fn output_asm(&mut self) -> Result<String, CodegenError<'a>> {
        Err(CodegenError::CannotCompileToAsm)
    }
    /// Produces a binary output after codegen. If needed, this may call
    /// external programs to assemble/link the output.
    ///
    /// # Errors
    /// This function should NEVER leave the type this is implemented on in an
    /// invalid state or panic. This function should propogate all errors.
    fn output_bin(&mut self) -> Result<Vec<u8>, CodegenError<'a>>;
}

pub mod x86_64_linux;
