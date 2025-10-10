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

use std::{fmt::Write as _, fs::File};

use qu::ir::{Location, ModuleItem, parse::Parse as _};

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

    std::fs::write("example.tokens", format!("{tokens:#?}"))?;

    let mut i = 0usize;

    let mut parsed_str = String::new();

    while i < tokens.len() {
        let (v, len) = ModuleItem::debug_output((), &tokens[i..]).unwrap();
        writeln!(parsed_str, "{v:#?}")?;
        i += len;
    }

    std::fs::write("example.parsed", parsed_str)?;

    Ok(())
}
