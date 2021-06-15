extern crate ascii_tree;
extern crate clap;
extern crate serde;
extern crate serde_json;

use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::fs::Metadata;
use std::path::{Path, PathBuf};

use clap::{App, Arg};
use serde::{Deserialize, Serialize};

fn list_dir<F>(
    include_broken: bool,
    path: PathBuf,
    filter: F,
) -> impl Iterator<Item = (OsString, Metadata, PathBuf)>
where
    F: Fn(&Path) -> bool,
{
    path.read_dir()
        .expect("Failed to read directory")
        .flatten()
        .flat_map(move |ent| {
            let path = ent.path();
            let meta = ent
                .metadata()
                .unwrap_or_else(|e| panic!("Failed to read metadata for {:?}: {}", path, e));
            if meta.file_type().is_symlink() {
                // Discard errors from broken symlinks
                let path = path.read_link();
                match path {
                    Ok(p) if filter(&p) && (include_broken || p.exists()) => {
                        Some((ent.file_name(), meta, p))
                    }
                    _ => None,
                }
            } else {
                filter(&path).then(|| (ent.file_name(), meta, path))
            }
        })
}

#[derive(Debug, Serialize, Deserialize)]
struct Roots {
    auto: HashSet<PathBuf>,
    per_user: HashMap<String, HashSet<PathBuf>>,
}

fn get_roots<P, F>(nix_dir: &P, include_broken: bool, filter: &F) -> Roots
where
    P: AsRef<Path>,
    F: Fn(&Path) -> bool,
{
    let gcroots = nix_dir.as_ref().join(r"var").join("nix").join("gcroots");
    Roots {
        auto: list_dir(include_broken, gcroots.join("auto"), filter)
            .map(|x| x.2)
            .collect(),
        per_user: list_dir(false, gcroots.join("per-user"), filter)
            .filter(|(_, meta, _)| meta.is_dir())
            .map(|(name, _, path)| {
                let contents = list_dir(include_broken, path, &filter)
                    .map(|x| x.2)
                    .collect();
                (name.into_string().unwrap(), contents)
            })
            .collect(),
    }
}

fn render_tree(roots: Roots) -> String {
    use ascii_tree::Tree;

    fn paths_to_node(name: String, paths: impl IntoIterator<Item = PathBuf>) -> Tree {
        let paths_vec = {
            let mut v = paths.into_iter().collect::<Vec<_>>();
            v.sort();
            v.into_iter()
                .map(|r| Tree::Leaf(vec![r.into_os_string().into_string().unwrap()]))
                .collect()
        };
        Tree::Node(name, paths_vec)
    }

    let auto = paths_to_node(String::from("auto"), roots.auto);
    let per_user: Vec<Tree> = {
        let mut v: Vec<_> = roots.per_user.into_iter().collect();
        v.sort_by(|a, b| a.0.cmp(&b.0));
        v.into_iter()
            .map(|(user, paths)| paths_to_node(user, paths))
            .collect()
    };
    let tree = Tree::Node(
        String::from("roots"),
        vec![auto, Tree::Node(String::from("per-user"), per_user)],
    );
    let mut output = String::new();
    ascii_tree::write_tree(&mut output, &tree).unwrap();
    output
}

fn main() {
    let matches = App::new("nix-gcroots")
        .arg(
            Arg::with_name("nix-dir")
                .short("n")
                .long("nix-dir")
                .takes_value(true)
                .value_name("DIR")
                .help("Location of the the nix installation (/nix by default)"),
        )
        .arg(
            Arg::with_name("json")
                .short("j")
                .long("json")
                .help("Output in JSON instead of pretty-printing"),
        )
        .arg(
            Arg::with_name("no-dots")
                .long("no-dots")
                .help("Exclude paths containing a component starting with a '.' from results"),
        )
        .arg(
            Arg::with_name("flat")
                .short("f")
                .long("flat")
                .help("Print all gcroots as a list, rather than a tree"),
        )
        .arg(
            Arg::with_name("include_broken")
                .short("b")
                .long("include-broken")
                .help("Include broken symlinks"),
        )
        .get_matches();

    let nix_dir = PathBuf::from(matches.value_of("nix-dir").unwrap_or("/nix"));
    let output_json = matches.is_present("json");
    let flat = matches.is_present("flat");
    let include_broken = matches.is_present("include_broken");
    let exclude_direnv = matches.is_present("no-dots");

    let res = get_roots(
        &nix_dir,
        include_broken,
        &if exclude_direnv {
            |path: &Path| {
                path.components()
                    .all(|c| !c.as_os_str().to_string_lossy().starts_with("."))
            }
        } else {
            |_: &Path| true
        },
    );

    let mut rendered;
    if flat {
        let res = {
            let mut paths = Vec::new();
            paths.extend(res.auto);
            for (_, more) in res.per_user.into_iter() {
                paths.extend(more);
            }
            paths.sort();
            paths
        };
        if output_json {
            rendered = serde_json::to_string(&res).unwrap();
        } else {
            rendered = String::new();
            for path in res {
                rendered += &format!("{}\n", path.into_os_string().into_string().unwrap());
            }
        }
    } else {
        if output_json {
            rendered = serde_json::to_string(&res).unwrap();
        } else {
            rendered = render_tree(res);
        }
    }

    println!("{}", rendered);
}
