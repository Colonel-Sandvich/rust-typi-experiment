use std::{
    collections::{BTreeSet, HashMap, HashSet},
    rc::Rc,
};

use oxc::{
    ast::ast::{
        ArrayExpressionElement, BindingPatternKind, Expression, ObjectPropertyKind, Program,
        Statement, VariableDeclaration, VariableDeclarationKind,
    },
    span::{GetSpan, Span},
};
use serde::Serialize;

use crate::types::{
    make_literal, oxc_property_key_to_local, oxc_to_local_type, Literal, PropertySignature, TSType,
};

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
pub struct TypeError {
    pub message: String,
    pub span: Option<Span>,
}

impl TypeError {
    pub fn new(message: &str, span: Option<Span>) -> Self {
        TypeError {
            message: message.to_string(),
            span,
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub struct TypeChecker {
    env: HashMap<String, Rc<TSType>>,
    current_scope: usize,
    scope_stack: Vec<HashSet<String>>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: HashMap::new(),
            current_scope: 0,
            scope_stack: vec![HashSet::new()],
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, message: &str, span: Option<Span>) {
        self.errors.push(TypeError::new(message, span));
    }

    pub fn infer_types(&mut self, program: &Program) {
        for stmt in &program.body {
            self.infer_stmt(stmt);
        }
    }

    fn infer_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VariableDeclaration(var_decl) => self.infer_var_decl(var_decl),
            _ => (),
        }
    }

    fn is_const_type_reference(&self, expr: &Expression) -> bool {
        matches!(expr, Expression::TSAsExpression(x) if x.type_annotation.is_const_type_reference())
    }

    fn infer_var_decl(&mut self, var_decl: &VariableDeclaration) {
        for decl in &var_decl.declarations {
            let kind_const = decl.kind == VariableDeclarationKind::Const;
            match &decl.id.kind {
                BindingPatternKind::BindingIdentifier(ident) => {
                    let Some(init) = decl.init.as_ref() else {
                        continue;
                    };
                    let name = ident.name.as_str();
                    let as_const = self.is_const_type_reference(init);
                    let inferred_type = Rc::new(self.infer_expr(init, kind_const, as_const));

                    let type_annotation = match init {
                        Expression::TSAsExpression(as_expr) => Some(&as_expr.type_annotation),
                        Expression::TSSatisfiesExpression(sat_expr) => {
                            Some(&sat_expr.type_annotation)
                        }
                        _ => None,
                    };

                    if !as_const {
                        if let Some(type_annotation) = type_annotation {
                            let expected = oxc_to_local_type(type_annotation);
                            self.check_type(&inferred_type, &expected, Some(init.span()));
                        }
                    }

                    if let Some(type_annotation) = &decl.id.type_annotation {
                        let expected = oxc_to_local_type(&type_annotation.type_annotation);
                        self.check_type(&inferred_type, &expected, Some(ident.span));
                        self.declare_variable(
                            name,
                            oxc_to_local_type(&type_annotation.type_annotation).into(),
                        );
                        return;
                    }

                    self.declare_variable(name, inferred_type);
                }
                BindingPatternKind::ObjectPattern(_) => todo!(),
                BindingPatternKind::ArrayPattern(_) => todo!(),
                BindingPatternKind::AssignmentPattern(_) => todo!(),
            };
        }
    }

    fn check_type(&mut self, inferred: &TSType, expected: &TSType, span: Option<Span>) {
        if inferred.extends(expected) {
            return;
        }

        let message = format!(
            "Type '{}' is not assignable to type '{}'",
            inferred, expected,
        );
        self.add_error(&message, span);
    }

    fn infer_expr(&mut self, expr: &Expression, kind_const: bool, as_const: bool) -> TSType {
        use Expression::*;

        match expr {
            StringLiteral(lit) => {
                if as_const || kind_const {
                    make_literal(lit.value.to_string())
                } else {
                    TSType::String
                }
            }
            NumericLiteral(lit) => {
                if as_const || kind_const {
                    make_literal(lit.value)
                } else {
                    TSType::Number
                }
            }
            BooleanLiteral(lit) => {
                if as_const || kind_const {
                    make_literal(lit.value)
                } else {
                    TSType::Boolean
                }
            }
            Identifier(ident) => (*self.lookup_variable(&ident.name)).clone(),
            TSAsExpression(init) => self.infer_expr(&init.expression, kind_const, as_const),
            TSSatisfiesExpression(init) => self.infer_expr(&init.expression, kind_const, as_const),
            ObjectExpression(obj) => {
                let mut members = BTreeSet::new();

                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(property) => {
                            members.insert(
                                PropertySignature {
                                    key: oxc_property_key_to_local(&property.key),
                                    computed: property.computed,
                                    optional: false,
                                    readonly: false,
                                    type_annotation: Some(self.infer_expr(
                                        &property.value,
                                        false,
                                        as_const,
                                    )),
                                }
                                .into(),
                            );
                        }
                        ObjectPropertyKind::SpreadProperty(property) => {
                            let ts_type = self.infer_expr(&property.argument, false, false);

                            if !ts_type.extends(&TSType::Object) {
                                self.add_error(
                                    "Spread types may only be created from object types.",
                                    Some(property.span),
                                );
                                return TSType::Any;
                            } else {
                                // Assuming ts_type is a TypeLiteral, extract its members
                                if let TSType::TypeLiteral(spread_members) = ts_type {
                                    members.extend(spread_members);
                                }
                            }
                        }
                    }
                }

                TSType::TypeLiteral(members)
            }
            ArrayExpression(array) => {
                let mut members = BTreeSet::new();

                for element in &array.elements {
                    match element {
                        ArrayExpressionElement::SpreadElement(spread) => {
                            let ts_type = self.infer_expr(&spread.argument, false, as_const);

                            if !ts_type.extends(&TSType::Array(Box::new(TSType::Any))) {
                                self.add_error(
                                    &format!("Type '{}' must have a '[Symbol.iterator]()' method that returns an iterator.", ts_type),
                                    Some(spread.span),
                                );
                            } else {
                                // Assuming ts_type is an Array, extract its members
                                if let TSType::Array(spread_members) = ts_type {
                                    if !members.contains(&*spread_members) {
                                        members.insert(*spread_members);
                                    }
                                }
                            }
                        }
                        ArrayExpressionElement::Elision(_) => todo!(),
                        _ => {
                            let ts_type = self.infer_expr(element.to_expression(), false, as_const);

                            if !members.contains(&ts_type) {
                                members.insert(ts_type);
                            }
                        }
                    }
                }

                if as_const {
                    return TSType::Array(Box::new(Literal::Array(members).into()));
                }

                TSType::Array(Box::new(match &members.len() {
                    0 => TSType::Any,
                    1 => members.pop_last().unwrap(),
                    _ => TSType::Union(members),
                }))
            }
            _ => todo!(),
        }
    }

    fn declare_variable(&mut self, name: &str, ty: Rc<TSType>) {
        if let Some(var_type) = self.env.get_mut(name) {
            *var_type = ty;
        } else {
            self.env.insert(name.to_string(), ty);
        }
        self.scope_stack
            .last_mut()
            .unwrap()
            .insert(name.to_string());
    }

    fn lookup_variable(&self, name: &str) -> Rc<TSType> {
        if let Some(ty) = self.env.get(name) {
            return ty.clone();
        }
        TSType::Unknown.into()
    }

    pub fn print_env(&self) {
        println!("Env: ");
        println!(
            "{}",
            serde_json::to_string_pretty(&self.env)
                .map_err(|e| e.to_string())
                .unwrap()
        );
        println!("Errors: ");
        println!(
            "{}",
            serde_json::to_string_pretty(&self.errors)
                .map_err(|e| e.to_string())
                .unwrap()
        );
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use oxc::allocator::Allocator;

    use crate::{
        parser::parse_typescript,
        types::{IdentifierName, Literal, PropertySignature},
    };

    use super::*;

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
    fn test_error_if_array_element_mismatch() {
        let allocator = Allocator::default();
        let code = r#"
            let arr: number[] = [1, "hello"];
        "#;

        let program = parse_typescript(&allocator, code).unwrap().program;
        let mut checker = TypeChecker::new();
        checker.infer_types(&program);

        let expected_message = "Type 'string' is not assignable to type 'number'".to_string();
        assert_eq!(checker.errors.len(), 1);
        assert_matches!(&checker.errors[..], [TypeError { message, .. }, ..] if *message == expected_message);
    }
}
