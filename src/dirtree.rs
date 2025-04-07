// Using ABI Stable types is very important
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::{
        ffi::{FFIModule, RegisterFFIFn},
        register_fn::RegisterFn,
    },
};
use steel_derive::Steel;

#[derive(Clone, Steel, PartialEq, Default, Debug)]
struct DirTree(std::collections::HashMap<String, Box<Node>>);

#[derive(Clone, Steel, PartialEq, Debug)]
enum Node {
    File,
    Symlink(String),
    Dir(DirTree),
}

fn dir_tree(path: String) -> DirTree {
    DirTree(
        std::fs::read_dir(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e))
            .map(|entry| {
                let entry = entry.unwrap_or_else(|e| panic!("failed on entry in {}: {}", path, e));
                let (file_name, file_type) = (
                    entry.file_name().into_string().unwrap(),
                    entry.file_type().unwrap(),
                );
                if file_type.is_file() {
                    (file_name, Box::new(Node::File))
                } else if file_type.is_symlink() {
                    (
                        file_name,
                        Box::new(Node::Symlink("target TODO".to_string())),
                    )
                } else if file_type.is_dir() {
                    (file_name, Box::new(Node::Dir(DirTree::default())))
                } else {
                    panic!("bad file_type")
                }
            })
            .collect::<std::collections::HashMap<String, Box<Node>>>(),
    )
}

pub fn register_fns(module: &mut FFIModule) {
    module.register_fn("dir-tree", dir_tree);
}
