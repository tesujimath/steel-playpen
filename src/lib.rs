// Using ABI Stable types is very important
use steel::{declare_module, steel_vm::ffi::FFIModule};

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/playpen");

    meminfo::register_fns(&mut module);
    dirtree::register_fns(&mut module);

    module
}

mod dirtree;
mod meminfo;
