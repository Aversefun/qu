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

use qu::ir::{parse::Parse as _, Location, ModuleItem};

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

    while i < tokens.len() {
        let (_, len) = ModuleItem::debug_output((), &tokens[i..]).unwrap();
        i += len;
    }

    Ok(())
}
