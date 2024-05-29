use std::{cmp::Ordering, collections::BTreeSet};

use derive_more::From;
use oxc::ast::ast::TSLiteral;
use serde::Serialize;
use strum::Display;

#[derive(Debug, Display, PartialEq, From, Serialize, Clone, PartialOrd, Ord, Eq)]
#[strum(serialize_all = "snake_case")]
pub enum TSType {
    // Primitives
    Any,
    BigInt,
    Boolean,
    Never,
    Null,
    Number,
    Object,
    String,
    Symbol,
    This,
    Undefined,
    Unknown,
    Void,
    // Compound
    Array(Box<TSType>),
    // TSConditionalType(Box<'a, TSConditionalType<'a>>),
    // TSConstructorType(Box<'a, TSConstructorType<'a>>),
    // TSFunctionType(Box<'a, TSFunctionType<'a>>),
    // TSImportType(Box<'a, TSImportType<'a>>),
    // TSIndexedAccessType(Box<'a, TSIndexedAccessType<'a>>),
    // TSInferType(Box<'a, TSInferType<'a>>),
    #[from(ignore)]
    Intersection(BTreeSet<TSType>),
    Literal(Literal),
    // TSMappedType(Box<'a, TSMappedType<'a>>),
    // TSNamedTupleMember(Box<'a, TSNamedTupleMember<'a>>),
    // TSQualifiedName(Box<'a, TSQualifiedName<'a>>),
    // TSTemplateLiteralType(Box<'a, TSTemplateLiteralType<'a>>),
    // TSTupleType(Box<'a, TSTupleType<'a>>),
    TypeLiteral(BTreeSet<Signature>),
    // TSTypeOperatorType(Box<'a, TSTypeOperator<'a>>),
    // TSTypePredicate(Box<'a, TSTypePredicate<'a>>),
    // TSTypeQuery(Box<'a, TSTypeQuery<'a>>),
    // TSTypeReference(Box<'a, TSTypeReference<'a>>),
    #[from(ignore)]
    Union(BTreeSet<TSType>),
    // JSDoc
    // JSDocNullableType(Box<'a, JSDocNullableType<'a>>),
    // JSDocUnknownType(Box<'a, JSDocUnknownType>),
}

#[derive(Debug, PartialEq, Serialize, From, Clone, PartialOrd, Ord, Eq)]
pub enum Signature {
    // TSIndexSignature(Box<'a, TSIndexSignature<'a>>),
    PropertySignature(PropertySignature),
    // TSCallSignatureDeclaration(Box<'a, TSCallSignatureDeclaration<'a>>),
    // TSConstructSignatureDeclaration(Box<'a, TSConstructSignatureDeclaration<'a>>),
    // TSMethodSignature(Box<'a, TSMethodSignature<'a>>),
}

#[derive(Debug, PartialEq, Serialize, Clone, PartialOrd, Eq, Ord)]
pub struct PropertySignature {
    pub computed: bool,
    pub optional: bool,
    pub readonly: bool,
    pub key: PropertyKey,
    pub type_annotation: Option<TSType>,
}

impl PropertySignature {
    pub fn new_from_key_and_type(key: PropertyKey, type_annotation: TSType) -> Self {
        Self {
            computed: false,
            optional: false,
            readonly: false,
            key,
            type_annotation: Some(type_annotation),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, From, Clone, Eq, PartialOrd, Ord)]
pub enum PropertyKey {
    StaticIdentifier(IdentifierName),
    PrivateIdentifier(PrivateIdentifier),
    // `Expression` variants added here by `inherit_variants!` macro
    // @inherit Expression
}

#[derive(Debug, PartialEq, Serialize, Clone, From, Eq, PartialOrd, Ord)]
pub struct IdentifierName(pub String);

#[derive(Debug, PartialEq, Serialize, Clone, Eq, PartialOrd, Ord)]
pub struct PrivateIdentifier(pub String);

impl Signature {
    pub fn extends(&self, other: &Self) -> bool {
        match (self, other) {
            (Signature::PropertySignature(s), Signature::PropertySignature(o)) => {
                s.key == o.key
                    && s.type_annotation
                        .as_ref()
                        .unwrap()
                        .extends(o.type_annotation.as_ref().unwrap())
            }
        }
    }
}

impl TSType {
    pub fn extends(&self, other: &Self) -> bool {
        if *other == TSType::Any {
            return true;
        }

        if *self == *other {
            return true;
        }

        if let TSType::Literal(literal) = self {
            let parent = match literal {
                Literal::Boolean(_) => TSType::Boolean,
                Literal::Numeric(_) => TSType::Number,
                Literal::String(_) => TSType::String,
                Literal::Array(_) => TSType::Array(TSType::Any.into()),
            };

            return parent == *other;
        }

        if matches!(self, TSType::TypeLiteral(_)) && other == &TSType::Object {
            return true;
        }

        if let TSType::TypeLiteral(child) = self
            && let TSType::TypeLiteral(parent) = other
        {
            return parent.iter().all(|x| child.iter().any(|c| c.extends(x)));
        }

        if let TSType::Union(union) = other {
            return union.iter().any(|x| self.extends(x));
        }

        if let TSType::Intersection(intersection) = other {
            return intersection.iter().all(|x| self.extends(x));
        }

        false
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Array {
    pub element_type: TSType,
}

#[derive(Debug, From, Serialize, Clone)]
pub enum Literal {
    Boolean(bool),
    Numeric(f64),
    // BigintLiteral(Box<'a, BigIntLiteral<'a>>),
    // RegExpLiteral(Box<'a, RegExpLiteral<'a>>),
    String(String),
    // TemplateLiteral(Box<'a, TemplateLiteral<'a>>),
    Array(BTreeSet<TSType>),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Boolean(a), Literal::Boolean(b)) => a == b,
            (Literal::Numeric(a), Literal::Numeric(b)) => a.to_bits() == b.to_bits(),
            (Literal::String(a), Literal::String(b)) => a == b,
            (Literal::Array(a), Literal::Array(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Literal::Boolean(a), Literal::Boolean(b)) => a.partial_cmp(b),
            (Literal::Numeric(a), Literal::Numeric(b)) => a.partial_cmp(b),
            (Literal::String(a), Literal::String(b)) => a.partial_cmp(b),
            (Literal::Array(a), Literal::Array(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl Ord for Literal {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub fn make_literal<T: Into<Literal>>(value: T) -> TSType {
    value.into().into()
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Union {
    pub types: Vec<TSType>,
}

pub fn oxc_to_local_type(typ: &oxc::ast::ast::TSType) -> TSType {
    use oxc::ast::ast::TSType::*;

    match typ {
        TSAnyKeyword(_) => TSType::Any,
        TSBigIntKeyword(_) => TSType::BigInt,
        TSBooleanKeyword(_) => TSType::Boolean,
        TSNeverKeyword(_) => TSType::Never,
        TSNullKeyword(_) => TSType::Null,
        TSNumberKeyword(_) => TSType::Number,
        TSObjectKeyword(_) => TSType::Object,
        TSStringKeyword(_) => TSType::String,
        TSSymbolKeyword(_) => TSType::Symbol,
        TSThisType(_) => TSType::This,
        TSUndefinedKeyword(_) => TSType::Undefined,
        TSUnknownKeyword(_) => TSType::Unknown,
        TSVoidKeyword(_) => TSType::Void,
        TSArrayType(array) => TSType::Array(Box::new(oxc_to_local_type(&array.element_type))),
        TSConditionalType(_) => todo!(),
        TSConstructorType(_) => todo!(),
        TSFunctionType(_) => todo!(),
        TSImportType(_) => todo!(),
        TSIndexedAccessType(_) => todo!(),
        TSInferType(_) => todo!(),
        TSIntersectionType(intersection) => {
            TSType::Intersection(intersection.types.iter().map(oxc_to_local_type).collect())
        }
        TSLiteralType(literal_type) => TSType::Literal(match &literal_type.literal {
            TSLiteral::BooleanLiteral(bool) => Literal::Boolean(bool.value),
            TSLiteral::NumericLiteral(num) => Literal::Numeric(num.value),
            TSLiteral::StringLiteral(string) => Literal::String(string.value.to_string()),
            _ => todo!(),
        }),
        TSMappedType(_) => todo!(),
        TSNamedTupleMember(_) => todo!(),
        TSQualifiedName(_) => todo!(),
        TSTemplateLiteralType(_) => todo!(),
        TSTupleType(_) => todo!(),
        TSTypeLiteral(lit) => {
            TSType::TypeLiteral(lit.members.iter().map(oxc_signature_to_local).collect())
        }
        TSTypeOperatorType(_) => todo!(),
        TSTypePredicate(_) => todo!(),
        TSTypeQuery(_) => todo!(),
        TSTypeReference(_) => todo!(),
        TSUnionType(union) => TSType::Union(union.types.iter().map(oxc_to_local_type).collect()),
        JSDocNullableType(_) => todo!(),
        JSDocUnknownType(_) => todo!(),
    }
}

pub fn oxc_signature_to_local(signature: &oxc::ast::ast::TSSignature) -> Signature {
    use oxc::ast::ast::TSSignature::*;

    match signature {
        TSIndexSignature(_) => todo!(),
        TSPropertySignature(sig) => PropertySignature {
            computed: sig.computed,
            optional: sig.optional,
            readonly: sig.readonly,
            key: oxc_property_key_to_local(&sig.key),
            type_annotation: sig
                .type_annotation
                .as_ref()
                .map(|x| oxc_to_local_type(&x.type_annotation)),
        }
        .into(),
        TSCallSignatureDeclaration(_) => todo!(),
        TSConstructSignatureDeclaration(_) => todo!(),
        TSMethodSignature(_) => todo!(),
    }
}

pub fn oxc_property_key_to_local(property: &oxc::ast::ast::PropertyKey) -> PropertyKey {
    use oxc::ast::ast::PropertyKey::*;

    match property {
        StaticIdentifier(p) => IdentifierName(p.name.to_string()).into(),
        PrivateIdentifier(p) => todo!(),
        BooleanLiteral(_) => todo!(),
        NullLiteral(_) => todo!(),
        NumericLiteral(_) => todo!(),
        BigintLiteral(_) => todo!(),
        RegExpLiteral(_) => todo!(),
        StringLiteral(_) => todo!(),
        TemplateLiteral(_) => todo!(),
        // Should be IdentifierReference??
        Identifier(p) => IdentifierName(p.name.to_string()).into(),
        _ => todo!(),
    }
}
