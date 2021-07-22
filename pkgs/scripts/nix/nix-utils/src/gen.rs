use std::path::Path;

use clap::{App, Arg, ArgMatches, SubCommand};

pub fn subcommand<'a, 'b>() -> App<'a, 'b> {
    SubCommand::with_name("gen")
        .about("Generate common nix code")
        .arg(
            Arg::with_name("TEMPLATE")
                .required(true)
                .possible_values(&["flake-compat", "flake-compat-default", "flake-compat-shell"])
                .index(1)
                .multiple(true)
                .help("The template code to generate"),
        )
}

fn gen_files<P: AsRef<Path>, C: AsRef<[u8]>>(ps: &[(P, C)]) -> Result<(), String> {
    match ps.iter().map(|x| &x.0).find(|name| name.as_ref().exists()) {
        None => ps
            .iter()
            .try_for_each(|(name, contents)| {
                let r = std::fs::write(name, contents);
                println!("Generated {}", name.as_ref().display());
                r
            })
            .map_err(|e| format!("{}", e)),
        Some(name) => Err(format!("Error: could not generate {}, file already exists", name.as_ref().display())),
    }
}

pub fn handle(matches: &ArgMatches) -> Result<(), String> {
    static FLAKE_COMPAT_DEFAULT: &str = include_str!("templates/flake-compat-default.nix");
    static FLAKE_COMPAT_SHELL: &str = include_str!("templates/flake-compat-shell.nix");

    let templates = {
        let mut templates = matches.values_of("TEMPLATE").unwrap().collect::<Vec<_>>();
        templates.sort_unstable();
        templates.dedup();
        templates
    };

    templates.into_iter().try_for_each(|t| match t {
        "flake-compat" => gen_files(&[
            (Path::new("default.nix"), FLAKE_COMPAT_DEFAULT),
            (Path::new("shell.nix"), FLAKE_COMPAT_SHELL),
        ]),
        "flake-compat-default" => gen_files(&[(Path::new("default.nix"), FLAKE_COMPAT_DEFAULT)]),
        "flake-compat-shell" => gen_files(&[(Path::new("shell.nix"), FLAKE_COMPAT_SHELL)]),
        _ => {
            todo!("{}", t);
        }
    })
}
