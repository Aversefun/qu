//! New Rust Project
#![warn(
    missing_docs,
    clippy::missing_docs_in_private_items,
    clippy::pedantic,
    clippy::all,
    clippy::ignore_without_reason,
    clippy::alloc_instead_of_core,
    clippy::as_underscore,
    clippy::assertions_on_result_states
)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::cast_possible_wrap,
    clippy::too_many_lines,
    reason = "unwanted"
)]

use std::fs::File;

use qu::ir::{parse::Parse as _, FunctionAnnotation, Location, ModuleAnnotation};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    let tokens = qu::ir::parse::tokenizer::read_tokens(
        &mut File::open("example.qir")?,
        &Location {
            line: Some(1),
            column: Some(1),
            index: Some(0),
            file: Some("example.qir".into()),
        },
    )?;

    let (_, len1) = ModuleAnnotation::debug_output((), &tokens).unwrap();
    let (_, len2) = ModuleAnnotation::debug_output((), &tokens[len1..]).unwrap();
    let (_, len3) = ModuleAnnotation::debug_output((), &tokens[(len1 + len2)..]).unwrap();
    let (_, len4) = FunctionAnnotation::debug_output((), &tokens[(len1 + len2 + len3)..]).unwrap();

    Ok(())
}
