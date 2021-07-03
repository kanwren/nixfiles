use std::collections::HashMap;
use std::process::Command;

use clap::{App, ArgMatches, SubCommand, AppSettings};
use serde::{Deserialize, Serialize};

pub fn subcommand<'a, 'b>() -> App<'a, 'b> {
    SubCommand::with_name("flake")
        .subcommand(
            SubCommand::with_name("list-inputs")
                .about("List the inputs of a flake from its metadata"),
        )
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .about("Flake-related utilities")
}

#[derive(Debug, Serialize, Deserialize)]
struct Node {
    inputs: Option<HashMap<String, serde_json::Value>>,
}
#[derive(Debug, Serialize, Deserialize)]
struct Locks {
    nodes: HashMap<String, Node>,
    root: String,
}
#[derive(Debug, Serialize, Deserialize)]
struct Metadata {
    locks: Locks,
}

pub fn list_inputs(_: &ArgMatches) -> Result<(), String> {
    let output = Command::new("nix")
        .args(vec!["flake", "metadata", "--json"])
        .output()
        .map_err(|err| format!("Error: failed to query nix flake metadata: {:?}", err))?;
    let output = if output.status.success() {
        String::from_utf8(output.stdout).map_err(|e| format!("{:?}", e))
    } else {
        let err = String::from_utf8(output.stderr).map_err(|e| format!("{:?}", e))?;
        match output.status.code() {
            Some(code) => Err(format!(
                "Error: 'nix flake metadata' exited with status code {}\n{}",
                code, err
            )),
            None => Err(format!("Error: 'nix flake metadata' exited:\n{}", err)),
        }
    }?;

    let mut metadata: Metadata = serde_json::from_str(&output)
        .map_err(|e| format!("Error: failed to parse nix flake metadata: {}", e))?;

    let root_name = metadata.locks.root;
    let node = metadata
        .locks
        .nodes
        .remove(&root_name)
        .ok_or("Error: flake metadata is missing the root lock node")?;
    if let Some(inputs) = node.inputs.map(|h| h.into_keys().collect::<Vec<_>>()) {
        println!("{}", inputs.join("\n"));
    }

    Ok(())
}

pub fn handle(matches: &ArgMatches) -> Result<(), String> {
    match matches.subcommand() {
        ("list-inputs", Some(matches)) => list_inputs(matches),
        (_, _) => panic!("Unreachable"),
    }
}
