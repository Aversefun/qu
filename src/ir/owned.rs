//! Owned types - primarily used for codegen threads.

use crate::{ir::code::Cond, List, OwnedVersion};

use super::{CallingConvention, FunctionHint, Primitive, RuntimeCheck, StructAnnotation};

/// Implements `From<$borrowed<'a>> for $owned`.
///
/// Uses .into() on them, it should call .to_owned or .into_owned.
macro_rules! impl_from_borrowed {
    ($owned:ty, $borrowed:ty, struct { $($field:ident),* $(,)? }) => {
        impl<'a> From<$borrowed> for $owned {
            fn from(b: $borrowed) -> Self {
                Self {
                    $(
                        $field: b.$field.into(),
                    )*
                }
            }
        }
    };

    ($owned:ty, $borrowed:ty, enum { $(
                $variant:ident
                $( ( $($tuple_field:ident),* $(,)? ) )?
                $( { $($named_field:ident),* $(,)? } )?
                $(,)?
            )* }) => {
        impl<'a> From<$borrowed> for $owned {
            fn from(b: $borrowed) -> Self {
                match b {
                    $(
                        <$borrowed>::$variant
                        $( ( $($tuple_field),* ) )?
                        $( { $($named_field),* } )? => {
                            <$owned>::$variant
                            $( ( $( $tuple_field.into() ),* ) )?
                            $( { $( $named_field: $named_field.into() ),* } )?
                        }
                    ),*
                }
            }
        }
    };
}

/// A single function annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionAnnotation {
    /// Export this function.
    Export(CallingConvention, String),
    /// Used as an intermediate layer between [`FunctionDef::Internal`]
    /// and [`FunctionDef::External`].
    Extern(CallingConvention, String),
    /// Hints for optimizations.
    Hint(Vec<FunctionHint>),
}

impl_from_borrowed!(FunctionAnnotation, super::FunctionAnnotation<'a>, enum {
    Export(cc, s),
    Extern(cc, s),
    Hint(hints),
});

/// A reference to a specific variable version.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VarRef {
    /// The variable name. Cannot be `_` as that is mapped to [`ExtendedVarRef::Drop`].
    pub name: String,
    /// The variable version. If None, means that it's a memory variable.
    pub version: Option<u32>,
}

impl_from_borrowed!(VarRef, super::VarRef<'a>, struct {
    name,
    version
});

/// An extended variable reference. Used in assigning.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExtendedVarRef {
    /// A real variable.
    Real(VarRef),
    /// A field of a struct.
    Struct(VarRef, String),
    /// Keyword to drop the variable.
    Drop,
}

impl_from_borrowed!(ExtendedVarRef, super::ExtendedVarRef<'a>, enum {
    Real(vref),
    Struct(vref, field),
    Drop,
});

/// A constant value.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs, reason = "types are self explanatory")]
pub enum ConstValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Uptr(u64),
    /// Uptr value of &_
    DropAddress,

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Iptr(i64),

    F32(f32),
    F64(f64),

    CString(String),
    String(String),
}

impl_from_borrowed!(ConstValue, super::ConstValue<'a>, enum {
    U8(v),
    U16(v),
    U32(v),
    U64(v),
    U128(v),
    Uptr(v),
    DropAddress,

    I8(v),
    I16(v),
    I32(v),
    I64(v),
    I128(v),
    Iptr(v),

    F32(v),
    F64(v),

    CString(v),
    String(v),
});

/// A value.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// A constant value.
    Constant(ConstValue),
    /// A local variable.
    Variable(ExtendedVarRef),
    /// The default value for untaken branches. Used in phi.
    Undef,
}

impl_from_borrowed!(Value, super::Value<'a>, enum {
    // A constant value.
    Constant(v),
    // A local variable.
    Variable(v),
    // The default value for untaken branches. Used in phi.
    Undef,
});

/// A generic type.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// A primitive type.
    Primitive(Primitive),
    /// A struct.
    Struct(String),
    /// A list.
    List {
        /// The type of the items.
        ty: Box<Type>,
        /// The length of the list.
        length: Value,
    },
    /// A pointer.
    Pointer(Box<Type>),
    /// A value in memory (.data or .bss sections).
    Memory(Box<Type>),
}

impl From<Box<super::Type<'_>>> for Box<Type> {
    fn from(value: Box<super::Type<'_>>) -> Self {
        Box::new((*value).into())
    }
}

impl_from_borrowed!(Type, super::Type<'a>, enum {
    Primitive(v),
    Struct(v),
    List {
        ty,
        length
    },
    Pointer(v),
    Memory(v),
});

/// The return type of a function.
#[derive(Clone, Debug, PartialEq)]
pub enum RetType {
    /// A normal type.
    Normal(Type),
    /// Never returns.
    Never,
}

impl_from_borrowed!(RetType, super::RetType<'a>, enum {
    Normal(v),
    Never
});

impl From<List<'_, super::FunctionAnnotation<'_>>> for Vec<FunctionAnnotation> {
    fn from(value: List<'_, super::FunctionAnnotation<'_>>) -> Self {
        value.iter().map(|v| v.clone().into()).collect()
    }
}

/// An internal function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct InternalFunctionSignature {
    /// Annotations on the function.
    pub annotations: Vec<FunctionAnnotation>,
    /// The name of the function.
    pub name: String,
    /// The parameters of the function. String is the name of the parameter.
    pub params: Vec<(String, Type)>,
    /// The result of the function.
    pub result: Option<RetType>,
}

impl_from_borrowed!(InternalFunctionSignature, super::InternalFunctionSignature<'a>, struct {
    annotations,
    name,
    params,
    result
});

/// A function call.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    /// The function itself.
    pub func: String,
    /// The arguments.
    pub args: Vec<Value>,
}

/// A struct definition.
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    /// Struct annotations.
    pub annotations: Vec<StructAnnotation>,
    /// The name of the struct.
    pub name: String,
    /// The fields of the struct.
    pub fields: Vec<(String, Type)>,
}

/// A module-level annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum ModuleAnnotation {
    /// The version of Qu that this module is designed for. Follows semver rules.
    QuVer(OwnedVersion),
    /// Runtime checks to enable.
    RuntimeChecks(Vec<RuntimeCheck>),
    /// The function(s) to call when a runtime check is violated.
    CheckViolated(Vec<FunctionCall>),
}

/// An external function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternalFunctionSignature {
    /// Annotations on the function.
    pub annotations: Vec<FunctionAnnotation>,
    /// The name of the function.
    pub name: String,
    /// The parameters of the function. String is the name of the parameter.
    pub params: Vec<Type>,
    /// Whether the number of parameters is undefined.
    pub params_continue: bool,
    /// The result of the function.
    pub result: Option<RetType>,
}

/// Instructions that do produce a value.
#[derive(Clone, Debug, PartialEq)]
pub enum ProdInstruction {
    /// Addition.
    Add(Value, Value),
    /// Subtraction.
    Sub(Value, Value),
    /// Multiplication.
    Mul(Value, Value),
    /// Division.
    Div(Value, Value),
    /// Remainder.
    Rem(Value, Value),

    /// Bitwise and.
    And(Value, Value),
    /// Bitwise or.
    Or(Value, Value),
    /// Bitwise not.
    Not(Value),
    /// Bitwise xor.
    Xor(Value, Value),

    /// Dereference a pointer.
    DerefPtr(Value),
    /// Create a pointer to a memory variable.
    CreatePtr(String),

    /// Returns the first variable that is not undefined.
    Phi(Vec<ExtendedVarRef>),

    /// A producing function call.
    Call(FunctionCall),

    /// A value.
    Value(Value),
}

/// Options provided to a inline assembly block.
#[derive(Clone, Debug, PartialEq)]
pub enum AssemblyOption {
    /// A codegen-specific option.
    CodegenSpecific(String),
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
pub enum AssemblyOpt {
    /// Put the value from a variable into a register.
    VarToReg(VarRef, String),
    /// Put the value from a register into a variable (or drop it and mark it as clobbered).
    RegToVar(String, ExtendedVarRef),
    /// Provide an option.
    Option(AssemblyOption),
}

/// Instructions that don't produce a value.
#[derive(Clone, Debug, PartialEq)]
pub enum NoProdInstruction {
    /// A non-producing function call.
    Call(FunctionCall),
    /// A comparison with branch.
    CmpBr {
        /// The first value to compare.
        v0: Value,
        /// The condition to use to compare.
        cond: Cond,
        /// The second value to compare.
        v1: Value,
        /// The block to jump to if true.
        b_true: String,
        /// The block to jump to if false.
        b_false: String,
    },
    /// An unconditional jump.
    Jmp(String),
    /// Inline assembly.
    InlineAssembly {
        /// The target to run this on.
        target: String,
        /// The target options to provide.
        target_opts: Option<String>,
        /// The assembly itself.
        asm: String,
        /// The options.
        opts: Vec<AssemblyOpt>,
    },
    /// A variable definition.
    VarDef(String, Type),
    /// Assign a value to a variable.
    Assign(ExtendedVarRef, ProdInstruction),
    /// Return a value.
    Return(Option<Value>),
}

/// A single block.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// The name of the block.
    pub name: String,
    /// The instructions.
    pub instructions: Vec<NoProdInstruction>,
}
