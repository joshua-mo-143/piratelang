pub mod errors;
pub mod interpreter;
pub mod parser;
pub mod symbols;

pub mod chained_methods;
pub mod stdlib;

#[macro_export]
macro_rules! check_args_num {
    ($expected_args:expr, $given_args:expr) => {
        if $expected_args != $given_args {
            panic!("Expected {} args, got {} args", $expected_args, $given_args)
        };
    };
}

#[macro_export]
macro_rules! get_variables {
    ($args:expr, $typelist:expr, $expected_type:expr) => {
        let tuple: $expected_type = ();
        $args.iter().zip($typelist).for_each(|(args, type) {

        })
    };
}
