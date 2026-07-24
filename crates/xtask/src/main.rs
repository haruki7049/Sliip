use clap::Parser;
use lll_xtask::builder::{Builder, LLLBuilder};
use lll_xtask::cli::CLIArgs;
use std::sync::LazyLock;

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

fn main() -> Result {
    tracing_subscriber::fmt::init();

    tracing::debug!("Parsing CLI arguments...");
    let args = CLIArgs::parse();
    tracing::debug!("Parsed CLI arguments.");

    tracing::debug!("Creating a LLLBuilder...");
    let builder = LLLBuilder::new(args, CARGO.to_string());
    tracing::debug!("Created a LLLBuilder.");

    builder.run()?;

    Ok(())
}

static CARGO: LazyLock<String> =
    LazyLock::new(|| std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string()));
