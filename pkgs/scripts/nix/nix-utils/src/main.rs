use clap::{App, AppSettings};

mod flake;
mod gcroots;

fn main() {
    let matches = App::new("nix-utils")
        .about("Various extra Nix utilities")
        .subcommand(gcroots::subcommand())
        .subcommand(flake::subcommand())
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .get_matches();

    let res = match matches.subcommand() {
        ("gcroots", Some(matches)) => gcroots::handle(matches),
        ("flake", Some(matches)) => flake::handle(matches),
        (_, _) => panic!("Unreachable"),
    };

    if let Err(msg) = res {
        eprintln!("{}", msg);
        std::process::exit(1);
    }
}
