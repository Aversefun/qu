//! The code itself.

use super::{ExtendedVarRef, FunctionCall, Type, Value, VarRef};
use crate::{List, Str};

/// A condition for a [`CmpBr`](NoProdInstruction::CmpBr).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(missing_docs, reason = "unneeded for conditions")]
pub enum Cond {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// Instructions that do produce a value.
#[derive(Clone, Debug, PartialEq)]
pub enum ProdInstruction<'a> {
    /// Addition.
    Add(Value<'a>, Value<'a>),
    /// Subtraction.
    Sub(Value<'a>, Value<'a>),
    /// Multiplication.
    Mul(Value<'a>, Value<'a>),
    /// Division.
    Div(Value<'a>, Value<'a>),
    /// Remainder.
    Rem(Value<'a>, Value<'a>),

    /// Bitwise and.
    And(Value<'a>, Value<'a>),
    /// Bitwise or.
    Or(Value<'a>, Value<'a>),
    /// Bitwise not.
    Not(Value<'a>),
    /// Bitwise xor.
    Xor(Value<'a>, Value<'a>),

    /// Dereference a pointer.
    DerefPtr(Value<'a>),
    /// Create a pointer to a memory variable.
    CreatePtr(Str<'a>),

    /// Returns the first variable that is not undefined.
    Phi(List<'a, VarRef<'a>>),

    /// A producing function call.
    Call(FunctionCall<'a>),

    /// A value.
    Value(Value<'a>),
}

/// Options provided to a inline assembly block.
#[derive(Clone, Debug, PartialEq)]
pub enum AssemblyOption<'a> {
    /// A codegen-specific option.
    CodegenSpecific(Str<'a>),
    /// This assembly doesn't return.
    NoReturn,
    /// Flags are not used.
    NoFlags,
    /// Pure means that it is completely deterministic and only depends on direct
    /// inputs. See the Rust reference.
    Pure,
    /// This assembly doesn't interact with memory outside of the stack.
    NoMem,
    /// This assembly only reads from memory except for the stack.
    ReadOnly,
    /// This assembly doesn't interact with the stack.
    NoStack,
}

/// A value for an inline assembly block.
#[derive(Clone, Debug, PartialEq)]
pub enum AssemblyOpt<'a> {
    /// Put the value from a variable into a register.
    VarToReg(VarRef<'a>, Str<'a>),
    /// Put the value from a register into a variable (or drop it and mark it as clobbered).
    RegToVar(Str<'a>, ExtendedVarRef<'a>),
    /// Provide an option.
    Option(AssemblyOption<'a>),
}

/// Instructions that don't produce a value.
#[derive(Clone, Debug, PartialEq)]
pub enum NoProdInstruction<'a> {
    /// A non-producing function call.
    Call(FunctionCall<'a>),
    /// A comparison with branch.
    CmpBr {
        v0: Value<'a>,
        cond: Cond,
        v1: Value<'a>,
        b_true: Str<'a>,
        b_false: Str<'a>
    },
    /// An unconditional jump.
    Jmp(Str<'a>),
    /// Inline assembly.
    InlineAssembly {
        /// The target to run this on.
        target: Str<'a>,
        /// The target options to provide.
        target_opts: Option<Str<'a>>,
        /// The assembly itself.
        asm: Str<'a>,
        /// The options.
        opts: List<'a, AssemblyOpt<'a>>,
    },
    /// A variable definition.
    VarDef(Str<'a>, Type<'a>),
    /// Assign a value to a variable.
    Assign(ExtendedVarRef<'a>, ProdInstruction<'a>),
    /// Return a value.
    Return(Option<Value<'a>>),
}

/// A single block.
#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    /// The name of the block.
    pub name: Str<'a>,
    /// The instructions.
    pub instructions: List<'a, NoProdInstruction<'a>>,
}
