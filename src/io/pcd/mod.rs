use std::collections::HashMap;

use smallvec::SmallVec;
use thiserror::Error;

use crate::point::ViewPoint;

mod parser;
mod serde;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Mismatched sizes in pcd file for FIELDS: {0}, TYPE: {1} and SIZE: {2}. Check the input file and see if the number of values of FIELDS, SIZE, TYPE and COUNT match.")]
    MismatchSchemaSizes(usize, usize, usize),
    #[error("Mismatched schemata, expected {:?} got {:?}", expected, found)]
    MismatchSchema {
        expected: Schema,
        found: Option<Schema>,
        additional: Option<String>,
    },
    // #[error("Underlying system Io error")]
    // IoError(crate::io::Error),
    #[error("A system IO Error occured: {}", 0)]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse {} in file at: {}", ty, pos)]
    ParseValueError {
        data: DataKind,
        ty: String,
        pos: usize,
    },
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::MismatchSchemaSizes(l0, l1, l2), Self::MismatchSchemaSizes(r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            (
                Self::MismatchSchema {
                    expected: l_expected,
                    found: l_found,
                    additional: l_additional,
                },
                Self::MismatchSchema {
                    expected: r_expected,
                    found: r_found,
                    additional: r_additional,
                },
            ) => l_expected == r_expected && l_found == r_found && l_additional == r_additional,
            (Self::IoError(l0), Self::IoError(r0)) => unimplemented!("Cannot compare IoErrors"),
            (
                Self::ParseValueError {
                    data: l_data,
                    ty: l_ty,
                    pos: l_pos,
                },
                Self::ParseValueError {
                    data: r_data,
                    ty: r_ty,
                    pos: r_pos,
                },
            ) => l_data == r_data && l_ty == r_ty && l_pos == r_pos,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DataKind {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl From<(TypeKind, &u64)> for ValueKind {
    fn from(value: (TypeKind, &u64)) -> Self {
        Self::from_(value.0, *value.1)
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

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
struct FieldDef {
    value_kind: ValueKind,
    count: u64,
}

impl FieldDef {
    fn new(value_kind: ValueKind, count: u64) -> Self {
        Self { value_kind, count }
    }
}

impl From<(ValueKind, u64)> for FieldDef {
    fn from(value: (ValueKind, u64)) -> Self {
        Self {
            value_kind: value.0,
            count: value.1,
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Schema {
    fields: HashMap<String, SmallVec<[FieldDef; 2]>>,
}

impl Schema {
    fn empty() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

    fn new(fields: HashMap<String, SmallVec<[FieldDef; 2]>>) -> Self {
        Self { fields }
    }

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

        let mut fields = HashMap::new();
        names.zip(value_kinds.zip(counts)).for_each(|(name, def)| {
            fields.insert(name, smallvec::smallvec![FieldDef::from(def)]);
        });

        Ok(Self { fields })
    }

    fn insert<S: Into<String>>(&mut self, name: S, def: &[FieldDef]) {
        self.fields.insert(name.into(), SmallVec::from_slice(def));
    }

    fn remove(&mut self, name: &str) {
        self.fields.remove(name);
    }

    fn is_subset(&self, rhs: &Self) -> bool {
        if self.fields.len() != rhs.fields.len()
            || !self.fields.keys().all(|k| rhs.fields.contains_key(k))
        {
            return false;
        }

        // This is horribly inefficient, but it _should_ be fine for our usecase, as
        // schemata shouldn't get to large. A better implementation is very welcome
        // TODO Benchmarks needed to gauge performance
        self.fields
            .iter()
            .all(|(name, defs)| defs.iter().all(|def| rhs.fields[name].contains(def)))
    }

    fn is_superset(&self, rhs: &Self) -> bool {
        rhs.is_subset(self)
    }
}

#[derive(Debug, PartialEq)]
struct PcdHeader {
    version: Version,
    width: u64,
    height: u64,
    viewpoint: ViewPoint<f32>, // TODO maybe make this generic? Adds a lot of complexity for little gain
    num_points: u64,
    data: DataKind,
    data_offset: usize,
    schema: Schema,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use smallvec::smallvec;

    use crate::io::pcd::{Error, FieldDef};

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
            fields: HashMap::from([
                (
                    "x".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "y".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "z".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
            ]),
        };

        assert_eq!(schema, expected);
    }

    #[test]
    fn schema_from_iters_invalid() {
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let values = vec![ValueKind::F32, ValueKind::F32];
        let counts = vec![1];

        let schema = Schema::from_iters(names.into_iter(), values.into_iter(), counts.into_iter());
        assert!(schema.is_err());
        let err = schema.unwrap_err();

        assert_eq!(err, Error::MismatchSchemaSizes(3, 2, 1));
    }

    #[test]
    fn schema_subset() {
        let lhs = Schema {
            fields: HashMap::from([
                (
                    "x".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "y".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "z".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
            ]),
        };
        let rhs = Schema {
            fields: HashMap::from([
                (
                    "x".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "y".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "z".to_string(),
                    smallvec![
                        FieldDef::from((ValueKind::F32, 1)),
                        FieldDef::from((ValueKind::U32, 2))
                    ],
                ),
            ]),
        };

        assert!(lhs.is_subset(&rhs));
        assert!(!rhs.is_subset(&lhs));
    }

    #[test]
    fn schema_insert() {
        let mut schema = Schema {
            fields: HashMap::from([
                (
                    "x".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "y".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "z".to_string(),
                    smallvec![
                        FieldDef::from((ValueKind::F32, 1)),
                        FieldDef::from((ValueKind::U32, 2))
                    ],
                ),
            ]),
        };

        schema.insert("rgba".to_string(), &[FieldDef::from((ValueKind::U8, 3))]);

        let expected = Schema {
            fields: HashMap::from([
                (
                    "x".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "y".to_string(),
                    smallvec![FieldDef::from((ValueKind::F32, 1))],
                ),
                (
                    "z".to_string(),
                    smallvec![
                        FieldDef::from((ValueKind::F32, 1)),
                        FieldDef::from((ValueKind::U32, 2))
                    ],
                ),
                (
                    "rgba".to_string(),
                    smallvec![FieldDef::from((ValueKind::U8, 3))],
                ),
            ]),
        };

        assert_eq!(schema, expected);
    }
}
