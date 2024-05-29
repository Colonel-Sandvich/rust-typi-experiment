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
    is_const_type_reference, make_literal, oxc_property_key_to_local, oxc_to_local_type, Literal,
    PropertySignature, TSType,
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
    pub errors: Vec<TypeError>,
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

    pub fn lookup_variable(&self, name: &str) -> Rc<TSType> {
        if let Some(ty) = self.env.get(name) {
            return ty.clone();
        }
        TSType::Unknown.into()
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

    fn infer_var_decl(&mut self, var_decl: &VariableDeclaration) {
        for decl in &var_decl.declarations {
            let kind_const = decl.kind == VariableDeclarationKind::Const;
            match &decl.id.kind {
                BindingPatternKind::BindingIdentifier(ident) => {
                    let Some(init) = decl.init.as_ref() else {
                        continue;
                    };
                    let name = ident.name.as_str();
                    let as_const = is_const_type_reference(init);
                    let inferred_type = Rc::new(self.infer_expr(init, kind_const, as_const, None));

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
                        let inferred_type =
                            Rc::new(self.infer_expr(init, kind_const, as_const, Some(&expected)));

                        self.declare_variable(name, Rc::new(expected));
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

    fn infer_expr(
        &mut self,
        expr: &Expression,
        kind_const: bool,
        as_const: bool,
        expected: Option<&TSType>,
    ) -> TSType {
        use Expression::*;

        let ty = match expr {
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
            TSAsExpression(init) => {
                self.infer_expr(&init.expression, kind_const, as_const, expected)
            }
            TSSatisfiesExpression(init) => {
                self.infer_expr(&init.expression, kind_const, as_const, expected)
            }
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
                                        expected,
                                    )),
                                }
                                .into(),
                            );
                        }
                        ObjectPropertyKind::SpreadProperty(property) => {
                            let ts_type =
                                self.infer_expr(&property.argument, false, false, expected);

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
                let expected2 = match expected {
                    Some(TSType::Array(x)) => Some(x.as_ref()),
                    _ => None,
                };

                let mut members = BTreeSet::new();

                for element in &array.elements {
                    match element {
                        ArrayExpressionElement::SpreadElement(spread) => {
                            let ts_type =
                                self.infer_expr(&spread.argument, false, as_const, expected2);

                            if !ts_type.extends(&TSType::Array(Box::new(TSType::Any))) {
                                self.add_error(
                                    &format!("Type '{}' must have a '[Symbol.iterator]()' method that returns an iterator.", ts_type),
                                    Some(spread.span),
                                );
                            } else {
                                // Assuming ts_type is an Array, extract its members
                                if let TSType::Array(spread_members) = ts_type {
                                    members.insert(*spread_members);
                                }
                            }
                        }
                        ArrayExpressionElement::Elision(_) => todo!(),
                        _ => {
                            let ts_type = self.infer_expr(
                                element.to_expression(),
                                false,
                                as_const,
                                expected2,
                            );

                            members.insert(ts_type);
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
        };

        if let Some(e) = expected {
            self.check_type(&ty, e, Some(expr.span()));
        }

        return ty;
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

    fn check_type(&mut self, inferred: &TSType, expected: &TSType, span: Option<Span>) {
        if inferred.is_assignable_to(expected) {
            return;
        }

        let message = format!(
            "Type '{}' is not assignable to type '{}'",
            inferred, expected,
        );
        self.add_error(&message, span);
    }

    fn add_error(&mut self, message: &str, span: Option<Span>) {
        self.errors.push(TypeError::new(message, span));
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
    // Should be used for testing private fields

    #[test]
    fn test_blank() {}
}
