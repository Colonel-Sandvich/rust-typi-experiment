use oxc::allocator::Allocator;
use rust_typi_experiment::{
    parser::{parse_typescript, print_program},
    type_checker::TypeChecker,
};

fn main() {
    let allocator = Allocator::default();
    let code = r#"let x = {...{a : 1}};"#;

    match parse_typescript(&allocator, code) {
        Ok(result) => {
            let program = result.program;
            print_program(&program);

            let mut checker = TypeChecker::new();
            checker.infer_types(&program);
            checker.print_env();
        }
        Err(e) => eprintln!("Error: {}", e),
    }

    drop(allocator);
}
