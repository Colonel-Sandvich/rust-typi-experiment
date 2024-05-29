#[cfg(test)]
mod tests {
    use std::{assert_matches::assert_matches, collections::BTreeSet};

    use oxc::allocator::Allocator;

    use crate::{
        parser::parse_typescript,
        type_checker::{TypeChecker, TypeError},
        types::{IdentifierName, Literal, PropertySignature, TSType},
    };

    #[test]
    fn test_infer_types() {
        let allocator = Allocator::default();
        let code = r#"
            let x = 42;
            let y = "hello";
            let z = true;
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(*checker.lookup_variable("x"), TSType::Number);
        assert_eq!(*checker.lookup_variable("y"), TSType::String);
        assert_eq!(*checker.lookup_variable("z"), TSType::Boolean);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_as_const() {
        let allocator = Allocator::default();
        let code = r#"let x = 42 as const;"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(
            *checker.lookup_variable("x"),
            Literal::Numeric(42.into()).into()
        );
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_annotate_number_as_const() {
        let allocator = Allocator::default();
        let code = r#"let x: number = 42 as const;"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(*checker.lookup_variable("x"), TSType::Number);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_error_if_mismatched_type() {
        let allocator = Allocator::default();
        let code = r#"let x: string = 42;"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let expected_message = "Type 'number' is not assignable to type 'string'".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }

    #[test]
    fn test_print_types() {
        let allocator = Allocator::default();
        let code = r#"
            let x = 42;
            let y = "hello";
            let z = true;
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);
        checker.print_env();

        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_functions() {
        let allocator = Allocator::default();
        let code = r#"
            function add(a: number, b: number): number {
                return a + b;
            }
            let sum = add(1, 2);
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(*checker.lookup_variable("sum"), TSType::Number);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_arrays() {
        let allocator = Allocator::default();
        let code = r#"let arr: number[] = [1, 2, 3];"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(
            *checker.lookup_variable("arr"),
            TSType::Array(TSType::Number.into())
        );
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_const_array() {
        let allocator = Allocator::default();
        let code = r#"const arr = [1, 2, 3];"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(
            *checker.lookup_variable("arr"),
            TSType::Array(TSType::Number.into())
        );
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_union_array() {
        let allocator = Allocator::default();
        let code = r#"const arr = [1, 2, "three"];"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(
            *checker.lookup_variable("arr"),
            TSType::Array(TSType::Union(BTreeSet::from([TSType::String, TSType::Number])).into())
        );
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_array_as_const() {
        let allocator = Allocator::default();
        let code = r#"const arr = [1, 2, 3] as const;"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(
            *checker.lookup_variable("arr"),
            TSType::Array(Box::new(
                Literal::Array(BTreeSet::from([
                    Literal::Numeric(1.0).into(),
                    Literal::Numeric(2.0).into(),
                    Literal::Numeric(3.0).into(),
                ]))
                .into()
            ))
        );
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_objects() {
        let allocator = Allocator::default();
        let code = r#"let obj = { a: 1 };"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let obj_type =
            TSType::TypeLiteral(BTreeSet::from([PropertySignature::new_from_key_and_type(
                IdentifierName("a".to_string()).into(),
                TSType::Number,
            )
            .into()]));
        assert_eq!(*checker.lookup_variable("obj"), obj_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_objects_with_spread() {
        let allocator = Allocator::default();
        let code = r#"let obj = { ...{a: 1} };"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let obj_type =
            TSType::TypeLiteral(BTreeSet::from([PropertySignature::new_from_key_and_type(
                IdentifierName("a".to_string()).into(),
                TSType::Number,
            )
            .into()]));
        assert_eq!(*checker.lookup_variable("obj"), obj_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_unions() {
        let allocator = Allocator::default();
        let code = r#"
            let value: number | string = 42;
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let union_type = TSType::Union(BTreeSet::from([TSType::Number, TSType::String]));
        assert_eq!(*checker.lookup_variable("value"), union_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_intersections() {
        let allocator = Allocator::default();
        let code = r#"
            let obj: { a: number; } & { b: string; } = { a: 1, b: "hello" };
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let intersection_type = TSType::Intersection(BTreeSet::from([
            TSType::TypeLiteral(BTreeSet::from([PropertySignature::new_from_key_and_type(
                IdentifierName("a".to_string()).into(),
                TSType::Number,
            )
            .into()])),
            TSType::TypeLiteral(BTreeSet::from([PropertySignature::new_from_key_and_type(
                IdentifierName("b".to_string()).into(),
                TSType::String,
            )
            .into()])),
        ]));
        assert_eq!(*checker.lookup_variable("obj"), intersection_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_intersections_known_properties() {
        let allocator = Allocator::default();
        let code = r#"
            let obj: { a: number } = { a: 1, b: "hello" };
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let intersection_type =
            TSType::TypeLiteral(BTreeSet::from([PropertySignature::new_from_key_and_type(
                IdentifierName("a".to_string()).into(),
                TSType::Number,
            )
            .into()]));
        assert_eq!(*checker.lookup_variable("obj"), intersection_type);

        let expected_message = "Object literal may only specify known properties, and 'b' does not exist in type '{ a: number; }'.".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }

    #[test]
    fn test_infer_types_with_generics() {
        let allocator = Allocator::default();
        let code = r#"
            function identity<T>(arg: T): T {
                return arg;
            }
            let strIdentity = identity("hello");
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        assert_eq!(*checker.lookup_variable("strIdentity"), TSType::String);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_tuples() {
        let allocator = Allocator::default();
        let code = r#"
            let tuple: [number, string, boolean] = [42, "hello", true];
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        // let tuple_type = TSType::Tuple(vec![TSType::Number, TSType::String, TSType::Boolean]);
        // assert_eq!(*checker.lookup_variable("tuple"), tuple_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_infer_types_with_enums() {
        let allocator = Allocator::default();
        let code = r#"
            enum Color { Red, Green, Blue }
            let c: Color = Color.Green;
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        // let enum_type = TSType::Enum("Color".to_string());
        // assert_eq!(*checker.lookup_variable("c"), enum_type);
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_error_if_wrong_function_return_type() {
        let allocator = Allocator::default();
        let code = r#"
            function add(a: number, b: number): string {
                return a + b;
            }
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let expected_message = "Type 'number' is not assignable to type 'string'".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }

    #[test]
    fn test_error_if_top_level_shape_mismatch() {
        let allocator = Allocator::default();
        let code = r#"let arr: number[] = "hello";"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let expected_message = "Type 'string' is not assignable to type 'number[]'".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }

    #[test]
    fn test_error_if_array_element_mismatch() {
        let allocator = Allocator::default();
        let code = r#"let arr: number[] = [1, "hello"];"#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let expected_message = "Type 'string' is not assignable to type 'number'".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }
}
