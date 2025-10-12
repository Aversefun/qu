//! Register allocation.
use std::range::RangeInclusive;

use crate::ir::owned::{NoProdInstruction, ProdInstruction, RegRef, Value};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Range(pub RangeInclusive<usize>);

impl Ord for Range {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .start
            .cmp(&other.0.start)
            .then(self.0.last.cmp(&other.0.last))
    }
}

impl PartialOrd for Range {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PhysicalRegister {
    Normal {
        id: usize,
        size: usize,
    },
    PartOf {
        id: usize,
        size: usize,
        part_of: &'static [usize],
    },
}

impl PhysicalRegister {
    pub fn get_id(self) -> usize {
        match self {
            PhysicalRegister::Normal { id, size: _ }
            | PhysicalRegister::PartOf {
                id,
                size: _,
                part_of: _,
            } => id,
        }
    }
    pub fn get_size(self) -> usize {
        match self {
            PhysicalRegister::Normal { id: _, size }
            | PhysicalRegister::PartOf {
                id: _,
                size,
                part_of: _,
            } => size,
        }
    }
    pub fn is_part_of_any(self, used: impl AsRef<[PhysicalRegister]>) -> bool {
        match self {
            PhysicalRegister::PartOf {
                id: _,
                size: _,
                part_of,
            } => {
                for reg in used.as_ref() {
                    if part_of.contains(&reg.get_id()) {
                        return true;
                    }
                }
                false
            }
            PhysicalRegister::Normal { id: _, size: _ } => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OutRegister {
    Reg(usize),
    Spilled,
}

impl From<Value> for Option<RegRef> {
    fn from(v: Value) -> Self {
        match v {
            Value::Variable(vref) => Some(vref),
            _ => None,
        }
    }
}

impl From<ProdInstruction> for Vec<RegRef> {
    fn from(value: ProdInstruction) -> Self {
        use ProdInstruction::*;
        match value {
            Add(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Sub(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Mul(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Div(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Rem(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),

            And(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Or(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),
            Not(v0) => match v0.into() {
                Some(v) => vec![v],
                None => vec![],
            },
            Xor(v0, v1) => [v0.into(), v1.into()]
                .into_iter()
                .flat_map(|v: Option<_>| v)
                .collect(),

            DerefPtr(v0) => match v0.into() {
                Some(v) => vec![v],
                None => vec![],
            },
            CreatePtr(_) => vec![],

            Phi(vrefs) => vrefs,

            Call(call) => call.args.into_iter().filter_map(Into::into).collect(),

            Value(v0) => match v0.into() {
                Some(v) => vec![v],
                None => vec![],
            },
        }
    }
}

impl From<NoProdInstruction> for Vec<RegRef> {
    fn from(value: NoProdInstruction) -> Self {
        match value {
            NoProdInstruction::Assign(vref, val) => [
                match vref.into() {
                    None => vec![],
                    Some(v) => vec![v],
                }
                .as_slice(),
                std::convert::Into::<Vec<_>>::into(val).as_slice(),
            ]
            .concat(),
            NoProdInstruction::VarDef(_, _) => vec![],
            NoProdInstruction::Call(call) => call.args.into_iter().filter_map(Into::into).collect(),
            NoProdInstruction::CmpBr {
                v0,
                cond: _,
                v1,
                b_true: _,
                b_false: _,
            } => [v0.into(), v1.into()]
                .into_iter()
                .filter_map(|v| v)
                .collect(),
            NoProdInstruction::InlineAssembly {
                target: _,
                target_opts: _,
                asm: _,
                opts,
            } => opts
                .into_iter()
                .filter_map(|v| match v {
                    crate::ir::owned::AssemblyOpt::Option(_) => None,
                    crate::ir::owned::AssemblyOpt::RegToVar(_, vref) => vref.into(),
                    crate::ir::owned::AssemblyOpt::VarToReg(vref, _) => Some(RegRef::Real(vref)),
                })
                .collect(),
            NoProdInstruction::Jmp(_) => vec![],
            NoProdInstruction::Return(v) => match v.and_then(Into::into) {
                Some(v) => vec![v],
                None => vec![],
            },
        }
    }
}

/// Do a single iteration of second-chance bin packing.
pub fn allocate_single<T: AsRef<[(RegRef, usize)]> + Clone>(
    registers: &[PhysicalRegister],
    vars: T,
) -> Vec<(RegRef, OutRegister)> {
    allocate_single_priv(registers)(&(Range((0..=0).into()), vars)).1
}

/// Do a single iteration of second-chance bin packing.
fn allocate_single_priv<T: AsRef<[(RegRef, usize)]> + Clone>(
    registers: &[PhysicalRegister],
) -> impl Fn(&(Range, T)) -> (Range, Vec<(RegRef, OutRegister)>) {
    move |(range, vars)| {
        let vars = vars.as_ref();
        let mut out = Vec::with_capacity(vars.len());
        let mut used = Vec::new();
        let mut num_used = 0usize;

        'var_loop: for (var, size) in vars {
            if num_used < registers.len() {
                for reg in (&registers[num_used..]) {
                    if !reg.is_part_of_any(&used) && reg.get_size() >= *size {
                        out.push((var.clone(), OutRegister::Reg(registers[num_used].get_id())));
                        used.push(registers[num_used]);
                        num_used += 1;
                        continue 'var_loop;
                    }
                }
            }
            out.push((var.clone(), OutRegister::Spilled));
        }

        (range.clone(), out)
    }
}

/// Allocate registers based on the ranges in which they are live.
pub fn allocate<T: AsRef<[(RegRef, usize)]> + Clone>(
    registers: impl AsRef<[PhysicalRegister]>,
    ranges: impl AsRef<[(Range, T)]>,
) -> Vec<(Range, Vec<(RegRef, OutRegister)>)> {
    let registers = registers.as_ref();

    ranges
        .as_ref()
        .into_iter()
        .map(allocate_single_priv(registers))
        .collect::<Vec<_>>()
}
