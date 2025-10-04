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

use qu::ir::{Location, ModuleAnnotation, parse::Parse as _};

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

    let (annotation1, len1) = ModuleAnnotation::parse((), &tokens).unwrap();
    println!("{annotation1:#?} (length {len1})");
    let (annotation2, len2) = ModuleAnnotation::parse((), &tokens[len1..]).unwrap();
    println!("{annotation2:#?} (length {len2})");
    let (annotation3, len3) = ModuleAnnotation::parse((), &tokens[(len1 + len2)..]).unwrap();
    println!("{annotation3:#?} (length {len3})");

    Ok(())
}
