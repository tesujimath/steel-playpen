use std::collections::{HashMap, HashSet};

use abi_stable::std_types::RVec;
// Using ABI Stable types is very important
use steel::{
    rvals::IntoSteelVal,
    steel_vm::{
        ffi::{FFIModule, FFIValue, RegisterFFIFn},
        register_fn::RegisterFn,
    },
    SteelVal,
};
use steel_derive::Steel;

#[derive(Clone, Steel, Default, Debug)]
struct DirTree(std::collections::HashMap<String, Box<Node>>);

#[derive(Clone, Steel, Debug)]
enum Node {
    File,
    Symlink(String),
    Dir(DirTree),
}

impl DirTree {
    fn size(&self) -> usize {
        self.0.len()
    }
}

fn dir_tree(path: String) -> DirTree {
    DirTree(
        std::fs::read_dir(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e))
            .filter_map(|entry| {
                let entry = entry.unwrap_or_else(|e| panic!("failed on entry in {}: {}", path, e));
                let (file_name, file_type) = (
                    entry.file_name().into_string().unwrap(),
                    entry.file_type().unwrap(),
                );
                if file_type.is_file() {
                    Some((file_name, Box::new(Node::File)))
                } else if file_type.is_symlink() {
                    Some((
                        file_name,
                        Box::new(Node::Symlink("target TODO".to_string())),
                    ))
                } else if file_type.is_dir() {
                    Some((file_name, Box::new(Node::Dir(DirTree::default()))))
                } else {
                    None
                }
            })
            .collect::<std::collections::HashMap<String, Box<Node>>>(),
    )
}

fn dir_iter(path: String) -> impl Iterator<Item = String> {
    let read_dir =
        std::fs::read_dir(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e));

    read_dir.map(|entry| {
        entry
            .unwrap_or_else(|e| panic!("failed on entry : {}", e))
            .file_name()
            .to_string_lossy()
            .into_owned()
    })
}

fn dir_map(path: String) -> HashMap<String, bool> {
    dir_iter(path)
        .map(|s| (s, true))
        .collect::<HashMap<String, bool>>()
}

fn dir_list(path: String) -> Vec<String> {
    // Rust's Vec is returned as Steel List, probably because Steel Vector is immutable
    dir_iter(path).collect::<Vec<String>>()
}

// returned as a Steel List, alas
fn dir_vec(path: String) -> FFIValue {
    // Rust's Vec is returned as Steel List, probably because Steel Vector is immutable
    // but this is also mapped to a Steel List, because of FFIValue::as_steelval,
    // which means we can't create Steel Vectors from Rust AFAICT
    let v = dir_iter(path)
        .map(|s| FFIValue::StringV(s.into()))
        .collect::<Vec<FFIValue>>();
    let rvec = RVec::from(v);
    FFIValue::Vector(rvec)
}

pub fn register_fns(module: &mut FFIModule) {
    module.register_fn("dir-tree", dir_tree);
    module.register_fn("DirTree-size", DirTree::size);

    module.register_fn("dir-list", dir_list);
    module.register_fn("dir-vec", dir_vec);
    module.register_fn("dir-map", dir_map);
}
