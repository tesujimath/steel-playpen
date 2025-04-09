// Using ABI Stable types is very important
use steel::steel_vm::{
    ffi::{FFIModule, RegisterFFIFn},
    register_fn::RegisterFn,
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

pub fn register_fns(module: &mut FFIModule) {
    module.register_fn("dir-tree", dir_tree);
    module.register_fn("DirTree-size", DirTree::size);
}
