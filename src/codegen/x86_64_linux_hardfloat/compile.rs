//! The actual compilation.
#![allow(clippy::unwrap_used, reason = "used a lot for infallable checks")]

use crate::{
    codegen::register_alloc::{PhysicalRegister, allocate_single},
    ir::{
        Primitive,
        code::Cond,
        owned::{
            Block, ExtendedVarRef, ExternalFunctionSignature, InternalFunctionSignature,
            ModuleAnnotation, NoProdInstruction, ProdInstruction, RegRef, StructDef, Value, VarRef,
        },
    },
};

use super::{CodegenPacket, ResultPacket};
use std::{
    collections::HashMap,
    fmt::Write,
    sync::mpmc::{Receiver, Sender},
};

use paste::paste;

/// The compilation thread.
#[allow(clippy::too_many_arguments, reason = "i don't give a fuck")]
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
            .map(|v| (v.0.as_str(), v.1.as_deref()))
            .collect::<Vec<_>>();
        for packet in &new_block_chan {
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
                .expect("ICE: thread comm error");
        }
    }
}

/// Mangle the name of a function/block.
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
        entry_block: _,
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

/// Macro for easily creating x86 [`PhysicalRegisters`].
macro_rules! reg {
    ($($start:literal: $name:literal),* $(, ($($start2:literal: $name2:literal),+ $(,)?))? $(,)?) => {
        const REGISTERS: &[PhysicalRegister] = &[$(PhysicalRegister::PartOf {
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
        }),*
        $(, $(PhysicalRegister::PartOf {
            id: $start2,
            size: 8,
            part_of: &[($start2)+1, ($start2)+2, ($start2)+3],
        },
        PhysicalRegister::PartOf {
            id: ($start2)+1,
            size: 4,
            part_of: &[($start2), ($start2)+2, ($start2)+3],
        },
        PhysicalRegister::PartOf {
            id: ($start2)+2,
            size: 2,
            part_of: &[($start2), ($start2)+1, ($start2)+3],
        },
        PhysicalRegister::PartOf {
            id: ($start2)+3,
            size: 1,
            part_of: &[($start2), ($start2)+1, ($start2)+2],
        }),+)?];
        paste!{
            $(
                const [< ID_ $start >]: usize = $start;
                const [< ID_ $start _1 >]: usize = $start+1;
                const [< ID_ $start _2 >]: usize = $start+2;
                const [< ID_ $start _3 >]: usize = $start+3;
                const [< ID_ $start _4 >]: usize = $start+4;
            )*
            $($(
                const [< ID_ $start2 >]: usize = $start2;
                const [< ID_ $start2 _1 >]: usize = $start2+1;
                const [< ID_ $start2 _2 >]: usize = $start2+2;
                const [< ID_ $start2 _3 >]: usize = $start2+3;
            )+)?
        }
        pub const fn reg_id_to_name(id: usize) -> &'static str {
            paste!{
                match id {
                    $(
                        [< ID_ $start >] => concat!("R", $name, "X"),
                        [< ID_ $start _1 >] => concat!("E", $name, "X"),
                        [< ID_ $start _2 >] => concat!($name, "X"),
                        [< ID_ $start _3 >] => concat!($name, "H"),
                        [< ID_ $start _4 >] => concat!($name, "L")
                    ),*
                    $(, $(
                        [< ID_ $start2 >] => $name2,
                        [< ID_ $start2 _1 >] => concat!($name2, "D"),
                        [< ID_ $start2 _2 >] => concat!($name2, "W"),
                        [< ID_ $start2 _3 >] => concat!($name2, "B")
                    ),+, )?
                    _ => panic!("ICE: Invalid register ID passed to reg_id_to_name!")
                }
            }
        }
        pub const fn reg_name_to_id(name: &'static str) -> usize {
            paste!{
                match name {
                    $(
                        concat!("R", $name, "X") => [< ID_ $start >],
                        concat!("E", $name, "X") => [< ID_ $start _1 >],
                        concat!($name, "X") => [< ID_ $start _2 >],
                        concat!($name, "H") => [< ID_ $start _3 >],
                        concat!($name, "L") => [< ID_ $start _4 >]
                    ),*
                    $(, $(
                        $name2 => [< ID_ $start2 >],
                        concat!($name2, "D") => [< ID_ $start2 _1 >],
                        concat!($name2, "W") => [< ID_ $start2 _2 >],
                        concat!($name2, "B") => [< ID_ $start2 _3 >]
                    ),+, )?
                    _ => panic!("ICE: Invalid register name passed to reg_name_to_id!")
                }
            }
        }
        pub const fn reg_id_to_size(id: usize) -> u64 {
            paste!{
                match id {
                    $(
                        [< ID_ $start >] => 8,
                        [< ID_ $start _1 >] => 4,
                        [< ID_ $start _2 >] => 2,
                        [< ID_ $start _3 >] => 1,
                        [< ID_ $start _4 >] => 1
                    ),*
                    $(, $(
                        [< ID_ $start2 >] => 8,
                        [< ID_ $start2 _1 >] => 4,
                        [< ID_ $start2 _2 >] => 2,
                        [< ID_ $start2 _3 >] => 1
                    ),+, )?
                    _ => panic!("ICE: Invalid register ID passed to reg_id_to_size!")
                }
            }
        }
    };
}

reg!(
    0: "A",
    5: "B",
    10: "C",
    15: "D",
    (
        20: "R8",
        24: "R9",
        28: "R10",
        32: "R11",
        36: "R12",
        40: "R13",
        // reserved for compiler as temporary locations:
        // 44: "R14",
        // 48: "R15",
    ),
);

/// The location a register is being stored.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum RegLoc {
    /// A register.
    Reg(usize),
    /// A memory location.
    Mem(String),
}

/// Get the size of a [`Value`]. If stored in memory, assumes the size of a pointer.
fn get_value_size(reg_mapping: &HashMap<RegRef, RegLoc>, value: &Value) -> u64 {
    use crate::ir::owned::ConstValue::{
        CString, DropAddress, F32, F64, I8, I16, I32, I64, I128, Iptr, String, U8, U16, U32, U64,
        U128, Uptr,
    };
    match value {
        Value::Constant(v) => match v {
            U8(_) | I8(_) => 1,
            U16(_) | I16(_) => 2,
            U32(_) | I32(_) | F32(_) => 4,
            U64(_) | Uptr(_) | I64(_) | Iptr(_) | DropAddress | F64(_) | CString(_) | String(_) => {
                8
            }
            U128(_) | I128(_) => {
                eprintln!("[WARN] interpreting x128 as x64");
                8
            }
        },
        Value::Undef => 0,
        Value::Variable(vref) => match reg_mapping.get(vref).unwrap() {
            RegLoc::Mem(_) => 8, // interpret mem as a pointer
            RegLoc::Reg(id) => reg_id_to_size(*id),
        },
    }
}

/// Get the return register for a value.
fn get_value_ret_reg(reg_mapping: &HashMap<RegRef, RegLoc>, value: &Value) -> usize {
    match get_value_size(reg_mapping, value) {
        0 | 1 => reg_name_to_id("R14B"),
        2 => reg_name_to_id("R14W"),
        4 => reg_name_to_id("R14D"),
        8 => reg_name_to_id("R14"),
        _ => unreachable!(),
    }
}

/// Generate a load of the provided value to the provided register.
///
/// # Returns
/// Returns the register adjusted for the size of the value.
fn generate_load(
    reg_mapping: &HashMap<RegRef, RegLoc>,
    value: &Value,
    result: &mut String,
    constants: &mut String,
    reg: usize,
) -> usize {
    use crate::ir::owned::ConstValue::{
        CString, DropAddress, F32, F64, I8, I16, I32, I64, I128, Iptr, String, U8, U16, U32, U64,
        U128, Uptr,
    };
    let reg = reg
        + match get_value_size(reg_mapping, value) {
            8 | 0 => 0,
            4 => 1,
            2 => 2,
            1 => 3,
            _ => unreachable!(),
        };
    match value {
        #[allow(
            clippy::format_push_string,
            reason = "would require a lot of work i don't want to do"
        )]
        Value::Constant(v) => result.push_str(&match v {
            U8(v) => format!("    mov {reg}, {v}"),
            U16(v) => format!("    mov {reg}, {v}"),
            U32(v) => format!("    mov {reg}, {v}"),
            U64(v) | Uptr(v) => format!("    mov {reg}, {v}"),
            U128(v) => {
                format!("    mov {reg}, {v}")
            }
            DropAddress => std::string::String::new(),

            I8(v) => format!("    mov {reg}, {v}"),
            I16(v) => format!("    mov {reg}, {v}"),
            I32(v) => format!("    mov {reg}, {v}"),
            I64(v) | Iptr(v) => format!("    mov {reg}, {v}"),
            I128(v) => {
                format!("    mov {reg}, {v}")
            }

            F32(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .float {v}"));
                format!("    fld dword [.data.const_{index}]")
            }
            F64(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .double {v}"));
                format!("    fld qword [.data.const_{index}]")
            }

            CString(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .string \"{v}\\x00\""));
                format!("    mov {reg}, .data.const_{index}")
            }
            String(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .string \"{v}\""));
                format!("    mov {reg}, .data.const_{index}")
            }
        }),
        Value::Undef => result.push_str("    ; undef/undefined behavior!"),
        Value::Variable(v) => write!(
            result,
            "    mov {reg}, {}",
            match reg_mapping.get(v).unwrap() {
                RegLoc::Reg(reg) => reg_id_to_name(*reg).to_string(),
                RegLoc::Mem(mem) => mem.clone(),
            }
        )
        .unwrap(),
    }
    result.push('\n');
    reg
}

/// generate a load into the output register for the provided value
fn generate_output_ret_val(
    reg_mapping: &HashMap<RegRef, RegLoc>,
    value: &Value,
    result: &mut String,
    constants: &mut String,
) {
    generate_load(
        reg_mapping,
        value,
        result,
        constants,
        get_value_ret_reg(reg_mapping, value),
    );
}

/// Generate a load onto the stack for the provided value.
fn generate_value_stack(
    reg_mapping: &HashMap<RegRef, RegLoc>,
    value: &Value,
    result: &mut String,
    constants: &mut String,
) {
    use crate::ir::owned::ConstValue::{
        CString, DropAddress, F32, F64, I8, I16, I32, I64, I128, Iptr, String, U8, U16, U32, U64,
        U128, Uptr,
    };
    match value {
        Value::Constant(v) => result.push_str(&match v {
            U8(v) => format!("    push byte {v}"),
            U16(v) => format!("    push word {v}"),
            U32(v) => format!("    push dword {v}"),
            U64(v) | Uptr(v) => format!("    push qword {v}"),
            U128(v) => {
                println!("[WARN] interpreting u128 as u64");
                format!("    push qword {v}")
            }
            DropAddress => std::string::String::new(),

            I8(v) => format!("    push byte {v}"),
            I16(v) => format!("    push word {v}"),
            I32(v) => format!("    push dword {v}"),
            I64(v) | Iptr(v) => format!("    push qword {v}"),
            I128(v) => {
                eprintln!("[WARN] interpreting i128 as i64");
                format!("    push qword {v}")
            }

            F32(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .float {v}"));
                format!("    fld dword [.data.const_{index}]")
            }
            F64(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .double {v}"));
                format!("    fld qword [.data.const_{index}]")
            }

            CString(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .string \"{v}\\x00\""));
                format!("    push .data.const_{index}")
            }
            String(v) => {
                let index = constants.len();
                constants.push_str(&format!("    .const_{index}: .string \"{v}\""));
                format!("    push .data.const_{index}")
            }
        }),
        Value::Undef => result.push_str("    ; undef/undefined behavior!"),
        Value::Variable(v) => result.push_str(&format!(
            "    push {}",
            match reg_mapping.get(v).unwrap() {
                RegLoc::Reg(reg) => reg_id_to_name(*reg).to_string(),
                RegLoc::Mem(mem) => mem.clone(),
            }
        )),
    }
    result.push('\n');
}

fn generate_prod_instr(
    reg_mapping: &HashMap<RegRef, RegLoc>,
    instr: ProdInstruction,
    result: &mut String,
    constants: &mut String,
    to: ExtendedVarRef,
) {
    use ProdInstruction::{Add, Sub, Mul, Div, Rem, And, Or, Not, Xor, DerefPtr, CreatePtr, Phi, Call, Value};
    match instr {
        Add(v0, v1) => {
            if to == ExtendedVarRef::Drop {
                return;
            }
            let reg1 = generate_load(reg_mapping, &v0, result, constants, reg_name_to_id("R14"));
            let reg2 = generate_load(reg_mapping, &v0, result, constants, reg_name_to_id("R15"));
            result.push_str(&format!(
                "    add {}, {}",
                reg_id_to_name(reg1),
                reg_id_to_name(reg2)
            ));
            reg_mapping
                .get(&match to {
                    ExtendedVarRef::Real(vref) => RegRef::Real(vref),
                    ExtendedVarRef::Struct(vref, s) => RegRef::Struct(vref, s),
                    ExtendedVarRef::Drop => unreachable!(),
                })
                .unwrap();
            result.push_str(&format!("    pop {reg2}"));
            result.push_str(&format!("    pop {reg1}"));
        }
        Sub(v0, v1) => {}
        Mul(v0, v1) => {}
        Div(v0, v1) => {}
        Rem(v0, v1) => {}

        And(v0, v1) => {}
        Or(v0, v1) => {}
        Not(v0) => {}
        Xor(v0, v1) => {}

        DerefPtr(v0) => {}
        CreatePtr(v0) => {}

        Phi(regs) => {}

        Call(call) => {}

        Value(v0) => generate_value_stack(reg_mapping, &v0, result, constants),
    }
}

/// Compile a block.
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
    let mut constants = String::new();
    let mut bss = String::new();

    let Block {
        name: block_name,
        instructions,
    } = block;

    if is_entry {
        let InternalFunctionSignature {
            ref annotations,
            name: _,
            params: _,
            result: _,
            entry_block: _,
        } = sig;
        if let Some(export) = annotations.iter().find(|v| v.is_export()) {
            let (_, name) = export.clone().unwrap_export();
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
        .chain(sig.params.iter())
        .map(|v| {
            (
                v.0.clone(),
                v.1.clone(),
                v.1.get_size()
                    .unwrap_or(Primitive::Uptr.get_size().unwrap()),
            )
        })
        .collect::<Vec<_>>();
    let mut allocate_regs = var_sizes
        .iter()
        .map(|v| {
            (
                RegRef::Real(VarRef {
                    name: v.0.clone(),
                    version: Some(0),
                }),
                v.2,
            )
        })
        .collect::<Vec<_>>();

    for instr in instructions {
        if let NoProdInstruction::VarDef(ref name, ref ty) = instr {
            let size = ty.get_size().unwrap_or(Primitive::Uptr.get_size().unwrap());
            var_sizes.push((name.clone(), ty.clone(), size));
            allocate_regs.push((
                RegRef::Real(VarRef {
                    name: name.clone(),
                    version: Some(0),
                }),
                size,
            ));
        }
        let new_vars: Vec<RegRef> = instr.clone().into();
        for vref in new_vars {
            if !allocate_regs.iter().any(|v| v.0 == vref) {
                let name = dbg!(vref.clone().unwrap_real().name);
                allocate_regs.push((
                    vref,
                    allocate_regs
                        .iter()
                        .find(|v| v.0.clone().unwrap_real().name == name)
                        .unwrap()
                        .1,
                ));
            }
        }
        let reg_mapping = allocate_single(REGISTERS, dbg!(&allocate_regs))
            .into_iter()
            .map(|v| {
                (
                    v.0,
                    v.1.some_reg_id().map_or_else(|| {
                        let index = bss.len();
                        bss.push_str(&format!("    .bss_{index}: .space 16"));
                        let reg_ref = format!(".bss.bss_{index}");
                        RegLoc::Mem(reg_ref)
                    }, RegLoc::Reg),
                )
            })
            .collect::<HashMap<_, _>>();

        match instr {
            NoProdInstruction::Call(call) => {
                for val in call.args {
                    generate_value_stack(&reg_mapping, &val, &mut result, &mut constants);
                }
                let sig = intern_funcs.iter().find(|v| v.name == call.func).unwrap();
                let entry = sig.entry_block.clone();
                result.push_str(&format!(
                    "    call {}",
                    mangle_name(sig, Some(&entry), input_name)
                ));
            }
            NoProdInstruction::Assign(vref, instr) => {
                generate_prod_instr(&reg_mapping, instr, &mut result, &mut constants, vref);
            }
            NoProdInstruction::CmpBr {
                v0,
                cond,
                v1,
                b_true,
                b_false,
            } => {
                generate_value_stack(&reg_mapping, &v1, &mut result, &mut constants);
                generate_value_stack(&reg_mapping, &v0, &mut result, &mut constants);
                let (size_1, size_2) = (
                    get_value_size(&reg_mapping, &v0),
                    get_value_size(&reg_mapping, &v1),
                );
                if size_1 != size_2 {
                    return Err(crate::errors::CodegenError::MismatchedComparisonSize(
                        v0, cond, v1,
                    ));
                }
                let suffix = match size_1 {
                    1 | 2 => "W",
                    4 => "D",
                    8 => "",
                    _ => unreachable!(),
                };
                result.push_str(&format!("    pop R14{suffix}\n"));
                result.push_str(&format!("    pop R15{suffix}\n"));
                result.push_str(&format!("    cmp R14{suffix}, R15{suffix}\n"));
                result.push_str(&match cond {
                    Cond::Equal => {
                        format!("    je {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                    Cond::NotEqual => {
                        format!("    jne {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                    Cond::LessThan => {
                        format!("    jl {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                    Cond::LessEqual => {
                        format!("    jle {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                    Cond::GreaterThan => {
                        format!("    jg {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                    Cond::GreaterEqual => {
                        format!("    jge {}\n", mangle_name(&sig, Some(&b_true), input_name))
                    }
                });
                result.push_str(&format!(
                    "    jmp {}\n",
                    mangle_name(&sig, Some(&b_false), input_name)
                ));
            }
            NoProdInstruction::InlineAssembly {
                target,
                target_opts,
                asm,
                mut opts,
            } => {
                if target != "x86_64" {
                    eprintln!("[INFO] skipping non-x86_64 assembly block");
                    continue;
                }
                for opt in &opts {
                    match opt {
                        crate::ir::owned::AssemblyOpt::Option(_) => {}
                        crate::ir::owned::AssemblyOpt::RegToVar(reg, _) => {
                            result.push_str(&format!("    push {reg}"));
                        }
                        crate::ir::owned::AssemblyOpt::VarToReg(var, reg) => {
                            result.push_str(&format!("    push {reg}"));
                            result.push_str(&format!(
                                "mov {reg}, {}",
                                match dbg!(&reg_mapping)
                                    .get(&RegRef::Real(dbg!(var.clone())))
                                    .unwrap()
                                {
                                    RegLoc::Reg(reg) => reg_id_to_name(*reg).to_string(),
                                    RegLoc::Mem(mem) => mem.clone(),
                                }
                            ));
                        }
                    }
                }

                if let Some(ref target_opts) = target_opts {
                    match target_opts.to_lowercase().as_str() {
                        "intel" | "" => {}
                        "att" => {
                            result.push_str("    .att_syntax prefix\n");
                        }
                        opt => {
                            return Err(crate::errors::CodegenError::InvalidInlineAsmOption(
                                "x86_64".to_string(),
                                opt.to_string(),
                                "intel,att".to_string(),
                            ));
                        }
                    }
                }

                result.push_str("    ");
                result.push_str(&asm);

                if let Some(ref target_opts) = target_opts {
                    match target_opts.to_lowercase().as_str() {
                        "intel" | "" => {}
                        "att" => {
                            result.push_str("    .intel_syntax noprefix\n");
                        }
                        _ => unreachable!(),
                    }
                }
                opts.reverse();
                for opt in opts {
                    match opt {
                        crate::ir::owned::AssemblyOpt::Option(_) => {}
                        crate::ir::owned::AssemblyOpt::RegToVar(reg, var) => {
                            match var {
                                ExtendedVarRef::Drop => {}
                                ExtendedVarRef::Real(vref) => {
                                    writeln!(
                                        result,
                                        "    mov {}, {reg}",
                                        match reg_mapping.get(dbg!(&RegRef::Real(vref))).unwrap() {
                                            RegLoc::Reg(reg) => reg_id_to_name(*reg).to_string(),
                                            RegLoc::Mem(mem) => mem.clone(),
                                        }
                                    ).unwrap();
                                }
                                ExtendedVarRef::Struct(_, _) => eprintln!(
                                    "[WARN] structs are currently unimplemented and broken"
                                ),
                            }
                            result.push_str(&format!("    pop {reg}"));
                        }
                        crate::ir::owned::AssemblyOpt::VarToReg(_, reg) => {
                            result.push_str(&format!("    pop {reg}"));
                        }
                    }
                }
            }
            NoProdInstruction::Jmp(block) => {
                result.push_str(&format!(
                    "    jmp {}\n",
                    mangle_name(&sig, Some(&block), input_name)
                ));
            }
            NoProdInstruction::Return(val) => {
                if let Some(val) = val {
                    generate_output_ret_val(&reg_mapping, &val, &mut result, &mut constants);
                }
                result.push_str("ret");
            }
            NoProdInstruction::VarDef(_, _) => {}
        }
    }

    Ok(super::OkResultPacket {
        func: sig.name.clone(),
        block: block_name.clone(),
        asm: format!(
            "{result}\n.section .rodata.{0}\n.data:\n    {constants}\n.section .bss.{0}\n.bss:\n    {bss}",
            mangle_name(&sig, Some(&block_name), input_name)
        ),
        i,
    })
}
