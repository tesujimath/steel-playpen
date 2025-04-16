macro_rules! register_enum {
    ($module: expr, $enum:ident, $( $variant:ident ),* ) => {
        $module.register_type::<$enum>(concat!(stringify!($enum), "?"));

        $(
        $module.register_fn(
            concat!(stringify!($enum), "-", stringify!($variant), "?"),
            |value: FFIArg| {
                if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
                    as_underlying_ffi_type::<$enum>(custom.get_mut())
                        .is_some_and(|entry| matches!(entry, $enum::$variant))
                } else {
                    false
                }
            },
        );
        )*
    };
}

macro_rules! register_enum_data {
    ($module: expr, $enum:ident, $( $variant:ident ),* ) => {
        $(
        $module.register_fn(
            concat!(stringify!($enum), "-", stringify!($variant), "?"),
            |value: FFIArg| {
                if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
                    as_underlying_ffi_type::<$enum>(custom.get_mut())
                        .is_some_and(|entry| matches!(entry, $enum::$variant(_)))
                } else {
                    false
                }
            },
        );

        $module.register_fn(
            concat!(stringify!($enum), "-", stringify!($variant)),
            |value: FFIArg| {
                if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
                    as_underlying_ffi_type::<$enum>(custom.get_mut())
                        .and_then(|entry| if let $enum::$variant(value) = entry { Some(value.clone().into())} else {None})
                        .unwrap_or(FFIValue::Void)
                } else {
                    FFIValue::Void               }
            },
        );
        )*
    };
}
