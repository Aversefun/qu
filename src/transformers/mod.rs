//! Transformers for code.

use crate::ir::{ConstValue, IRItem};

/// A transformer that acts upon IR.
pub trait Transformer<'a, T: IRItem> {
    /// Metadata needed to transform.
    type Meta: Default;
    /// The default meta value.
    #[must_use]
    fn default_meta() -> Self::Meta {
        Default::default()
    }
    /// Transform the items in some way.
    ///
    /// # Errors
    /// Returns an error if:
    /// 1. An error was detected with the IR.
    /// 2. An error was encountered while attempting to apply the transform.
    /// 3. An error in a sub-transformer occurred (propogation).
    fn transform(
        meta: Self::Meta,
        items: &'a mut [T],
    ) -> Result<(), crate::errors::TransformerError<'a>>;
}

pub struct ConstantShrink;

impl<'a> Transformer<'a, crate::ir::ConstValue<'a>> for ConstantShrink {
    type Meta = ();
    fn transform(
            (): Self::Meta,
            items: &'a mut [crate::ir::ConstValue],
        ) -> Result<(), crate::errors::TransformerError<'a>> {
        for item in items {
            if item.is_integer() {
                use ConstValue::{U8, U16, U32, U64, U128, Uptr, I8, I16, I32, I64, I128, Iptr};
                *item = match item {
                    U16(v) => if *v > u8::MAX as u16 {
                        item.clone()
                    } else {
                        U8(*v as u8)
                    },
                    U32(v) => if *v > u16::MAX as u32 {
                        item.clone()
                    } else {
                        if *v > u8::MAX as u32 {
                            U16(*v as u16)
                        } else {
                            U8(*v as u8)
                        }
                    },
                    U64(v) => if *v > u32::MAX as u64 {
                        item.clone()
                    } else {
                        if *v > u16::MAX as u64 {
                            U32(*v as u32)
                        } else {
                            if *v > u8::MAX as u64 {
                                U16(*v as u16)
                            } else {
                                U8(*v as u8)
                            }
                        }
                    },
                    U128(v) => if *v > u64::MAX as u128 {
                        item.clone()
                    } else {
                        if *v > u32::MAX as u128 {
                            U64(*v as u64)
                        } else {
                            if *v > u16::MAX as u128 {
                                U32(*v as u32)
                            } else {
                                if *v > u8::MAX as u128 {
                                    U16(*v as u16)
                                } else {
                                    U8(*v as u8)
                                }
                            }
                        }
                    },
                    I16(v) => if *v > i8::MAX as i16 || *v < i8::MIN as i16 {
                        item.clone()
                    } else {
                        I8(*v as i8)
                    },
                    I32(v) => if *v > i16::MAX as i32 || *v < i16::MIN as i32 {
                        item.clone()
                    } else {
                        if *v > i8::MAX as i32 || *v < i8::MIN as i32 {
                            I16(*v as i16)
                        } else {
                            I8(*v as i8)
                        }
                    },
                    I64(v) => if *v > i32::MAX as i64 || *v < i32::MIN as i64 {
                        item.clone()
                    } else {
                        if *v > i16::MAX as i64 || *v < i16::MIN as i64 {
                            I32(*v as i32)
                        } else {
                            if *v > i8::MAX as i64 || *v < i8::MIN as i64 {
                                I16(*v as i16)
                            } else {
                                I8(*v as i8)
                            }
                        }
                    },
                    I128(v) => if *v > i64::MAX as i128 || *v < i64::MIN as i128 {
                        item.clone()
                    } else {
                        if *v > i32::MAX as i128 || *v < i32::MIN as i128 {
                            I64(*v as i64)
                        } else {
                            if *v > i16::MAX as i128 || *v < i16::MIN as i128 {
                            I32(*v as i32)
                        } else {
                            if *v > i8::MAX as i128 || *v < i8::MIN as i128 {
                                I16(*v as i16)
                            } else {
                                I8(*v as i8)
                            }
                        }
                        }
                    },
                    U8(_) | Uptr(_) | I8(_) | Iptr(_) => item.clone(),

                    _ => unreachable!()
                };
            }
        }
        Ok(())
    }
}
