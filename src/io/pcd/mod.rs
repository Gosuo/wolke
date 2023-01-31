use thiserror::Error;

use crate::point::ViewPoint;

mod parser;

#[derive(Debug, Error)]
enum Error {
    #[error("Mismatched sizes in pcd file for FIELDS: {0}, TYPE: {1} and SIZE: {2}. Check the input file and see if the number of values of FIELDS, SIZE, TYPE and COUNT match.")]
    MismatchSchemaSizes(usize, usize, usize),
}

#[derive(Debug, PartialEq)]
enum Version {
    V0_7,
}

impl Version {
    fn from_bytes(input: &[u8]) -> Result<Self, ()> {
        match input {
            b".7" | b"0.7" => Ok(Self::V0_7),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
enum DataKind {
    Ascii,
    Binary,
    BinaryCompressed,
}

impl DataKind {
    fn from_bytes(input: &[u8]) -> Result<Self, ()> {
        match input {
            b"ascii" => Ok(Self::Ascii),
            b"binary" => Ok(Self::Binary),
            b"binary_compressed" => Ok(Self::BinaryCompressed),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum TypeKind {
    Signed,
    Unsigned,
    Float,
}

impl TypeKind {
    fn from_bytes(input: &[u8]) -> Result<Self, ()> {
        match input {
            b"I" => Ok(TypeKind::Signed),
            b"U" => Ok(TypeKind::Unsigned),
            b"F" => Ok(TypeKind::Float),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ValueKind {
    U8,
    U16,
    U32,
    I8,
    I16,
    I32,
    F32,
    F64,
}

impl Default for ValueKind {
    fn default() -> Self {
        ValueKind::F32
    }
}

impl From<(TypeKind, u64)> for ValueKind {
    fn from(value: (TypeKind, u64)) -> Self {
        Self::from_(value.0, value.1)
    }
}

impl ValueKind {
    fn from_(ty: TypeKind, size: u64) -> Self {
        match (ty, size) {
            (TypeKind::Unsigned, 1) => Self::U8,
            (TypeKind::Unsigned, 2) => Self::U16,
            (TypeKind::Unsigned, 4) => Self::U32,
            (TypeKind::Signed, 1) => Self::I8,
            (TypeKind::Signed, 2) => Self::I16,
            (TypeKind::Signed, 4) => Self::I32,
            (TypeKind::Float, 4) => Self::F32,
            (TypeKind::Float, 8) => Self::F64,
            (_, _) => todo!(),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
struct FieldDef {
    name: String,
    value_kind: ValueKind,
    count: u64,
}

impl From<(String, ValueKind, u64)> for FieldDef {
    fn from(value: (String, ValueKind, u64)) -> Self {
        Self {
            name: value.0,
            value_kind: value.1,
            count: value.2,
        }
    }
}

impl From<(String, (ValueKind, u64))> for FieldDef {
    fn from(value: (String, (ValueKind, u64))) -> Self {
        Self {
            name: value.0,
            value_kind: value.1 .0,
            count: value.1 .1,
        }
    }
}

#[derive(Debug, Default, PartialEq)]
struct Schema {
    fields: Vec<FieldDef>,
}

impl Schema {
    fn from_iters<S, V, U>(names: S, value_kinds: V, counts: U) -> Result<Self, Error>
    where
        S: Iterator<Item = String> + ExactSizeIterator,
        V: Iterator<Item = ValueKind> + ExactSizeIterator,
        U: Iterator<Item = u64> + ExactSizeIterator,
    {
        if names.len() != value_kinds.len() || names.len() != counts.len() {
            return Err(Error::MismatchSchemaSizes(
                names.len(),
                value_kinds.len(),
                counts.len(),
            ));
        }

        let fields = names
            .zip(value_kinds.zip(counts))
            .map(FieldDef::from)
            .collect::<Vec<_>>();

        Ok(Self { fields })
    }
}

#[derive(Debug, PartialEq)]
struct PcdHeader {
    version: Version,
    width: u64,
    height: u64,
    viewpoint: ViewPoint,
    num_points: u64,
    data: DataKind,
    schema: Schema,
}

#[cfg(test)]
mod tests {
    use crate::io::pcd::FieldDef;

    use super::{Schema, ValueKind};

    #[test]
    fn schema_from_iters_valid() {
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let values = vec![ValueKind::F32, ValueKind::F32, ValueKind::F32];
        let counts = vec![1, 1, 1];

        let schema = Schema::from_iters(names.into_iter(), values.into_iter(), counts.into_iter());
        assert!(schema.is_ok());
        let schema = schema.unwrap();

        let expected = Schema {
            fields: vec![
                FieldDef::from(("x".to_string(), ValueKind::F32, 1)),
                FieldDef::from(("y".to_string(), ValueKind::F32, 1)),
                FieldDef::from(("z".to_string(), ValueKind::F32, 1)),
            ],
        };

        assert_eq!(schema, expected);
    }

    #[test]
    fn schema_from_iters_invalid() {
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let values = vec![ValueKind::F32, ValueKind::F32];
        let counts = vec![1];

        let schema = Schema::from_iters(names.into_iter(), values.into_iter(), counts.into_iter());
    }
}
