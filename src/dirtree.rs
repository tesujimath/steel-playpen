#![allow(dead_code)]
use std::{collections::HashMap, fs::read_link};

use abi_stable::std_types::RVec;
// Using ABI Stable types is very important
use steel::steel_vm::{
    ffi::{as_underlying_ffi_type, CustomRef, FFIArg, FFIModule, FFIValue, RegisterFFIFn},
    register_fn::RegisterFn,
};
use steel_derive::Steel;

#[derive(Clone, Steel, Default, Debug)]
struct DirTree(std::collections::HashMap<String, Box<Node>>);

impl DirTree {
    fn size(&self) -> usize {
        self.0.len()
    }
}

#[derive(Clone, Steel, Debug)]
enum Node {
    File,
    Symlink(String),
    Dir(DirTree),
}

#[derive(Clone, Steel, Debug)]
enum Entry {
    File,
    Symlink(String),
    Dir,
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

fn dir_file_name_iter(path: String) -> impl Iterator<Item = String> {
    let read_dir =
        std::fs::read_dir(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e));

    read_dir.map(|entry| {
        let entry = entry.unwrap_or_else(|e| panic!("failed on entry : {}", e));
        entry.file_name().to_string_lossy().into_owned()
    })
}

fn dir_map(path: String) -> HashMap<String, bool> {
    dir_file_name_iter(path)
        .map(|file_name| (file_name, true))
        .collect::<HashMap<String, bool>>()
}

fn dir_entries(path: String) -> HashMap<String, Entry> {
    let read_dir =
        std::fs::read_dir(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e));

    read_dir
        .filter_map(|entry| {
            let entry = entry.unwrap_or_else(|e| panic!("failed on entry : {}", e));
            let file_name = entry.file_name().to_string_lossy().into_owned();
            let file_type = entry.file_type().unwrap();

            let entry = if file_type.is_file() {
                Some(Entry::File)
            } else if file_type.is_symlink() {
                let target = read_link(entry.path())
                    .unwrap()
                    .to_string_lossy()
                    .into_owned();
                Some(Entry::Symlink(target))
            } else if file_type.is_dir() {
                Some(Entry::Dir)
            } else {
                // ignore unknown file type, probably a pipe
                None
            };

            entry.map(|entry| (file_name, entry))
        })
        .collect::<HashMap<String, Entry>>()
}

fn dir_list(path: String) -> Vec<String> {
    // Rust's Vec is returned as Steel List, probably because Steel Vector is immutable
    dir_file_name_iter(path).collect::<Vec<String>>()
}

// returned as a Steel List, alas
fn dir_vec(path: String) -> FFIValue {
    // Rust's Vec is returned as Steel List, probably because Steel Vector is immutable
    // but this is also mapped to a Steel List, because of FFIValue::as_steelval,
    // which means we can't create Steel Vectors from Rust AFAICT
    let v = dir_file_name_iter(path)
        .map(|file_name| FFIValue::StringV(file_name.into()))
        .collect::<Vec<FFIValue>>();
    let rvec = RVec::from(v);
    FFIValue::Vector(rvec)
}

pub fn register_fns(module: &mut FFIModule) {
    module.register_fn("dir-tree", dir_tree);
    module.register_fn("DirTree-size", DirTree::size);

    // see https://github.com/mattwparas/steel/issues/358#issuecomment-2791237612
    module.register_fn("DirTree?", |value: FFIArg| {
        if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
            as_underlying_ffi_type::<DirTree>(custom.get_mut()).is_some()
        } else {
            false
        }
    });

    // module.register_fn("Entry?", |value: FFIArg| {
    //     if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
    //         as_underlying_ffi_type::<Entry>(custom.get_mut()).is_some()
    //     } else {
    //         false
    //     }
    // });
    // module.register_fn("Entry-File?", |value: FFIArg| {
    //     if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
    //         as_underlying_ffi_type::<Entry>(custom.get_mut())
    //             .is_some_and(|entry| matches!(entry, Entry::File))
    //     } else {
    //         false
    //     }
    // });

    // trace_macros!(true);
    register_enum!(module, Entry, File, Dir);
    register_enum_data!(module, Entry, Symlink);
    // trace_macros!(false);

    module.register_fn("dir-list", dir_list);
    module.register_fn("dir-vec", dir_vec);
    module.register_fn("dir-map", dir_map);
    module.register_fn("dir-entries", dir_entries);
}
