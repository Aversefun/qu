//! `QIR` parser.

use std::ops::Add;

use crate::{List, Str, Version, ir::code::Block, version};

/// List of supported QUIR versions.
pub const SUPPORTED_QUIR_VERSIONS: &[Version<'_>] = &[version!(0, 1, 0)];

pub mod code;
pub mod parse;
pub mod owned;

/// A location in a file.
#[derive(Clone, PartialEq, Eq, Default, Hash)]
pub struct Location<'a> {
    /// The line number.
    pub line: Option<usize>,
    /// The column number.
    pub column: Option<usize>,
    /// The character index into this location.
    pub index: Option<usize>,
    /// The file. If none, the formatter replaces it with "-".
    pub file: Option<Str<'a>>,
}

impl Ord for Location<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl PartialOrd for Location<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Location<'_> {
    /// Add a column offset to this `Location`.
    pub fn add_column(&mut self, i: usize) -> &mut Self {
        self.column = self.column.map(|v| v + i);
        self.index = self.index.map(|v| v + i);
        self
    }
    /// Add a column offset to this `Location`.
    pub fn add_line(&mut self, i: usize) -> &mut Self {
        self.line = self.line.map(|v| v + i);
        self.column = Some(1);
        self.index = self.index.map(|v| v + i);
        self
    }
}

impl Add for Location<'_> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            line: self.line.and_then(|v1| rhs.line.map(|v2| v1 + v2)),
            column: self.column.and_then(|v1| rhs.column.map(|v2| v1 + v2)),
            index: self.index.and_then(|v1| rhs.index.map(|v2| v1 + v2)),
            file: self.file.or(rhs.file),
        }
    }
}

impl std::fmt::Debug for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

impl std::fmt::Display for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (
            self.file.as_ref().unwrap_or(&("-".into())),
            self.line,
            self.column,
            self.index,
        ) {
            (file, Some(l), Some(col), _) => f.write_fmt(format_args!("{file}:{l}:{col}")),
            (file, Some(l), None, _) => f.write_fmt(format_args!("{file}:{l}")),
            (file, None, _, Some(i)) => f.write_fmt(format_args!("{file} at index {i}")),
            (file, None, None, None) => f.write_fmt(format_args!("{file}")),
            (file, None, Some(c), None) => f.write_fmt(format_args!("{file} at column {c}")),
        }
    }
}

/// Implement an is function.
macro_rules! impl_is {
    ($(#[$attrs:meta])* $p:pat, $name:ident) => {
        paste::paste! {
            #[doc = concat!("Returns whether this is a ", stringify!($ty), ".")]
            $(#[$attrs])*
            #[allow(unused_variables)]
            pub fn [< is_ $name >](&self) -> bool {
                matches!(self, $p)
            }
        }
    };
}

pub(crate) use impl_is;

/// Implement an unwrap function.
macro_rules! impl_unwrap {
    ($(#[$attrs:meta])* $parent:ty, $t:ty, $ty:ty, $p:pat => $e:expr, $name:ident) => {
        paste::paste! {
            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = "# Panics"]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will panic")]
            #[allow(clippy::double_must_use, unused_variables)]
            #[must_use]
            pub fn [< unwrap_ $name >](self) -> $t {
                match self {
                    $p => {$e},
                    _ => panic!(concat!(
                        "`", stringify!($parent), "::unwrap_", stringify!($t), "` attempted on non-`", stringify!($t),"` value"
                    )),
                }
            }

            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = "# Errors"]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will return a
                             [`IRWrongValueError`](crate::errors::IRWrongValueError).")]
            #[allow(unused_variables)]
            pub fn [< ok_ $name >](self) -> Result<$t, $crate::errors::IRWrongValueError<'a>> {
                match self {
                    $p => Ok({$e}),
                    _ => Err($crate::errors::IRWrongValueError::IsntExpectedValue(
                        stringify!($parent), stringify!($t)
                    )),
                }
            }

            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will return None.")]
            #[allow(unused_variables)]
            pub fn [< some_ $name >](self) -> Option<$t> {
                match self {
                    $p => Some({$e}),
                    _ => None,
                }
            }
        }
    };
    ($(#[$attrs:meta])* $parent:ty, $t:ty, $ty:path, $name:ident) => {
        impl_unwrap!($(#[$attrs])* $parent, $t, $ty, $ty (v) => v, $name);
        $crate::ir::impl_is!($(#[$attrs])* $ty (v), $name);
    };
}

pub(crate) use impl_unwrap;

/// A private trait implemented for all IR items.
pub(crate) trait ClosedIRItem {}

/// A trait implemented for all IR items that can be transformed.
/// See the [`Transformer`](crate::transformers::Transformer) trait.
#[expect(private_bounds, reason = "intended behavior")]
pub trait IRItem: ClosedIRItem {}

/// Easily implement [`IRItem`] and [`ClosedIRItem`] for something.
macro_rules! impl_ir_item {
    ($ty:ty) => {
        impl crate::ir::ClosedIRItem for $ty {}
        impl crate::ir::IRItem for $ty {}
    };
}

pub(crate) use impl_ir_item;

/// A constant value.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs, reason = "types are self explanatory")]
pub enum ConstValue<'a> {
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

    CString(Str<'a>),
    String(Str<'a>),
}

impl_ir_item!(ConstValue<'_>);

impl<'a> ConstValue<'a> {
    impl_unwrap!(ConstValue, u8, ConstValue::U8, u8);
    impl_unwrap!(ConstValue, u16, ConstValue::U16, u16);
    impl_unwrap!(ConstValue, u32, ConstValue::U32, u32);
    impl_unwrap!(ConstValue, u64, ConstValue::U64, u64);
    impl_unwrap!(ConstValue, u128, ConstValue::U128, u128);
    impl_unwrap!(ConstValue, u64, ConstValue::Uptr, uptr);
    impl_is!(ConstValue::DropAddress, drop_addr);

    impl_unwrap!(ConstValue, i8, ConstValue::I8, i8);
    impl_unwrap!(ConstValue, i16, ConstValue::I16, i16);
    impl_unwrap!(ConstValue, i32, ConstValue::I32, i32);
    impl_unwrap!(ConstValue, i64, ConstValue::I64, i64);
    impl_unwrap!(ConstValue, i128, ConstValue::I128, i128);
    impl_unwrap!(ConstValue, i64, ConstValue::Iptr, iptr);

    impl_unwrap!(ConstValue, f32, ConstValue::F32, f32);
    impl_unwrap!(ConstValue, f64, ConstValue::F64, f64);

    impl_unwrap!(ConstValue, Str<'a>, ConstValue::CString, cstr);
    impl_unwrap!(ConstValue, Str<'a>, ConstValue::String, str);
}

/// A primitive type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(missing_docs, reason = "types are self-explanatory")]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    U128,
    Uptr,

    I8,
    I16,
    I32,
    I64,
    I128,
    Iptr,

    F32,
    F64,

    CString,
    String,
}

impl_ir_item!(Primitive);

impl Primitive {
    impl_is!(Primitive::U8, u8);
    impl_is!(Primitive::U16, u16);
    impl_is!(Primitive::U32, u32);
    impl_is!(Primitive::U64, u64);
    impl_is!(Primitive::U128, u128);
    impl_is!(Primitive::Uptr, uptr);

    impl_is!(Primitive::I8, i8);
    impl_is!(Primitive::I16, i16);
    impl_is!(Primitive::I32, i32);
    impl_is!(Primitive::I64, i64);
    impl_is!(Primitive::I128, i128);
    impl_is!(Primitive::Iptr, iptr);

    impl_is!(Primitive::F32, f32);
    impl_is!(Primitive::F64, f64);

    impl_is!(Primitive::CString, cstr);
    impl_is!(Primitive::String, str);
}

/// A generic type.
#[derive(Clone, Debug, PartialEq)]
pub enum Type<'a> {
    /// A primitive type.
    Primitive(Primitive),
    /// A struct.
    Struct(Str<'a>),
    /// A list.
    List {
        /// The type of the items.
        ty: Box<Type<'a>>,
        /// The length of the list.
        length: Value<'a>,
    },
    /// A pointer.
    Pointer(Box<Type<'a>>),
    /// A value in memory (.data or .bss sections).
    Memory(Box<Type<'a>>),
}

impl_ir_item!(Type<'_>);

impl<'a> Type<'a> {
    impl_unwrap!(
        /// Why is this named `ty_primitive` and not just `primitive`? Because this and `ty_struct` are
        /// made using a macro and the compiler gets angry when we use a keyword where it expects an
        /// ident. For consistency, we made them all start with `ty_`.
        Type, Primitive, Type::Primitive, ty_primitive
    );
    impl_unwrap!(
        /// Why is this named `ty_struct` and not just `struct`? Because this is made using a macro
        /// and the compiler gets angry when we use a keyword where it expects an ident.
        Type,
        Str<'a>,
        Type::Struct,
        ty_struct
    );
    impl_unwrap!(
        /// Why is this named `ty_list` and not just `list`? Because this and `ty_struct` are
        /// made using a macro and the compiler gets angry when we use a keyword where it expects an
        /// ident. For consistency, we made them all start with `ty_`.
        Type, (Box<Type<'a>>, Value<'a>), Type::List, Type::List {ty, length} => (ty.clone(), length.clone()), ty_list
    );
}

/// The return type of a function.
#[derive(Clone, Debug, PartialEq)]
pub enum RetType<'a> {
    /// A normal type.
    Normal(Type<'a>),
    /// Never returns.
    Never,
}

impl_ir_item!(RetType<'_>);

impl<'a> RetType<'a> {
    impl_unwrap!(RetType, Type<'a>, RetType::Normal, normal);
    impl_is!(RetType::Never, never);
}

/// A reference to a specific variable version.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VarRef<'a> {
    /// The variable name. Cannot be `_` as that is mapped to [`ExtendedVarRef::Drop`].
    pub name: Str<'a>,
    /// The variable version. If None, means that it's a memory variable.
    pub version: Option<u32>,
}

impl_ir_item!(VarRef<'_>);

/// An extended variable reference. Used in assigning.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExtendedVarRef<'a> {
    /// A real variable.
    Real(VarRef<'a>),
    /// A field of a struct.
    Struct(VarRef<'a>, Str<'a>),
    /// Keyword to drop the variable.
    Drop,
}

impl_ir_item!(ExtendedVarRef<'_>);

impl<'a> ExtendedVarRef<'a> {
    impl_unwrap!(ExtendedVarRef, VarRef<'a>, ExtendedVarRef::Real, real);
    impl_unwrap!(ExtendedVarRef, VarRef<'a>, ExtendedVarRef::Struct, ExtendedVarRef::Struct(v, _) => v.clone(), struct_ref);
    impl_unwrap!(ExtendedVarRef, Str<'a>, ExtendedVarRef::Struct, ExtendedVarRef::Struct(_, v) => v.clone(), struct_field);
    impl_is!(ExtendedVarRef::Struct(_, _), ty_struct);
    impl_is!(ExtendedVarRef::Drop, drop);
}

/// A value.
#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    /// A constant value.
    Constant(ConstValue<'a>),
    /// A local variable.
    Variable(ExtendedVarRef<'a>),
    /// The default value for untaken branches. Used in phi.
    Undef,
}

impl_ir_item!(Value<'_>);

impl<'a> Value<'a> {
    impl_unwrap!(Value, ConstValue<'a>, Value::Constant, constant);
    impl_unwrap!(Value, ExtendedVarRef<'a>, Value::Variable, local);
    impl_is!(Value::Undef, undef);
}

/// A function call.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    /// The function itself.
    pub func: Str<'a>,
    /// The arguments.
    pub args: List<'a, Value<'a>>,
}

impl_ir_item!(FunctionCall<'_>);

/// Runtime checks that can be enabled in a QIR file. If conditions are violated,
/// then `check_violated` will be ran.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuntimeCheck {
    /// Check for out-of-bound accesses.
    ArrayBounds,
    /// Check for integer overflow.
    Overflow,
    /// Check for integer underflow.
    Underflow,
    /// Check for null pointers. In systems programming, should probably be disabled.
    NullPtr,
    /// Check for unaligned pointer accesses.
    UnalignedPtr,
}

impl RuntimeCheck {
    impl_is!(RuntimeCheck::ArrayBounds, array_bounds);
    impl_is!(RuntimeCheck::Overflow, overflow);
    impl_is!(RuntimeCheck::Underflow, underflow);
    impl_is!(RuntimeCheck::NullPtr, null_ptr);
    impl_is!(RuntimeCheck::UnalignedPtr, unaligned_ptr);
}

/// A module-level annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum ModuleAnnotation<'a> {
    /// The version of Qu that this module is designed for. Follows semver rules.
    QuVer(Version<'a>),
    /// Runtime checks to enable.
    RuntimeChecks(List<'a, RuntimeCheck>),
    /// The function(s) to call when a runtime check is violated.
    CheckViolated(List<'a, FunctionCall<'a>>),
}

impl_ir_item!(ModuleAnnotation<'_>);

impl<'a> ModuleAnnotation<'a> {
    impl_unwrap!(
        ModuleAnnotation,
        Version<'a>,
        ModuleAnnotation::QuVer,
        quver
    );
    impl_unwrap!(
        ModuleAnnotation,
        List<'a, RuntimeCheck>,
        ModuleAnnotation::RuntimeChecks,
        runtime_checks
    );
    impl_unwrap!(
        ModuleAnnotation,
        List<'a, FunctionCall<'a>>,
        ModuleAnnotation::CheckViolated,
        check_violated
    );
}

/// Foreign calling conventions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallingConvention {
    /// C calling convention.
    C,
    /// The default Qu calling convention.
    Qu,
}

impl CallingConvention {
    impl_is!(CallingConvention::C, c);
    impl_is!(CallingConvention::Qu, qu);
}

/// Hints for optimizations.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionHint {
    /// Inline this function always.
    InlineAlways,
    /// Never inline this function.
    InlineNever,
}

/// A single function annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionAnnotation<'a> {
    /// Export this function.
    Export(CallingConvention, Str<'a>),
    /// Used as an intermediate layer between [`FunctionDef::Internal`]
    /// and [`FunctionDef::External`].
    Extern(CallingConvention, Str<'a>),
    /// Hints for optimizations.
    Hint(List<'a, FunctionHint>),
}

impl<'a> FunctionAnnotation<'a> {
    impl_unwrap!(
        FunctionAnnotation,
        (CallingConvention, Str<'a>),
        FunctionAnnotation::Export,
        FunctionAnnotation::Export(cc, s) => (cc, s),
        export
    );
    impl_unwrap!(
        FunctionAnnotation,
        (CallingConvention, Str<'a>),
        FunctionAnnotation::Extern,
        FunctionAnnotation::Extern(cc, s) => (cc, s),
        extern
    );
    impl_unwrap!(
        FunctionAnnotation,
        List<'a, FunctionHint>,
        FunctionAnnotation::Hint,
        hint
    );
}

/// An internal function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct InternalFunctionSignature<'a> {
    /// Annotations on the function.
    pub annotations: List<'a, FunctionAnnotation<'a>>,
    /// The name of the function.
    pub name: Str<'a>,
    /// The parameters of the function. String is the name of the parameter.
    pub params: List<'a, (Str<'a>, Type<'a>)>,
    /// The result of the function.
    pub result: Option<RetType<'a>>,
}

/// An external function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternalFunctionSignature<'a> {
    /// Annotations on the function.
    pub annotations: List<'a, FunctionAnnotation<'a>>,
    /// The name of the function.
    pub name: Str<'a>,
    /// The parameters of the function. String is the name of the parameter.
    pub params: List<'a, Type<'a>>,
    /// Whether the number of parameters is undefined.
    pub params_continue: bool,
    /// The result of the function.
    pub result: Option<RetType<'a>>,
}

/// A function definition.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionDef<'a> {
    /// An external function.
    External {
        /// The calling convention.
        calling_conv: CallingConvention,
        /// The internal function signature.
        sig: ExternalFunctionSignature<'a>,
        /// The external name of the function.
        external_name: Str<'a>,
    },
    /// An internally defined function.
    Internal {
        /// The signature of the function.
        sig: InternalFunctionSignature<'a>,
        /// Function-scoped variables.
        func_vars: List<'a, (Str<'a>, Type<'a>)>,
        /// The code of the function.
        code: List<'a, Block<'a>>,
        /// The name of the entry block.
        entry_block: Str<'a>,
    },
}

impl_ir_item!(FunctionDef<'_>);

/// A struct annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StructAnnotation {
    /// The representation.
    Repr {
        /// The calling convention.
        cc: CallingConvention,
        /// Whether to remove any padding.
        packed: bool,
    },
}

/// A struct definition.
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef<'a> {
    /// Struct annotations.
    pub annotations: List<'a, StructAnnotation>,
    /// The name of the struct.
    pub name: Str<'a>,
    /// The fields of the struct.
    pub fields: List<'a, (Str<'a>, Type<'a>)>,
}

impl_ir_item!(StructDef<'_>);

/// A single module-level item in QIR.
#[derive(Clone, Debug, PartialEq)]
pub enum ModuleItem<'a> {
    /// A module annotation.
    ModuleAnnotation(ModuleAnnotation<'a>),
    /// A struct definition.
    StructDef(StructDef<'a>),
    /// A function definition.
    Func(FunctionDef<'a>),
}
