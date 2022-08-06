#[macro_export(local_inner_macros)]
macro_rules! hashmap {
    () => { ::std::collections::HashMap::new() };
    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let mut _map = ::std::collections::HashMap::new();
            $(
                let _ = _map.insert($key, $value);
            )*
                _map
        }
    };
}
