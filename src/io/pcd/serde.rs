use std::{any::type_name, collections::HashMap, io::BufRead, str::FromStr, ops::AddAssign};

use nalgebra::Scalar;
use num_traits::Zero;

use crate::{
    io::pcd::{Error, FieldDef, ValueKind},
    point::{Color, PointXYZRGBA},
};

use super::Schema;

pub trait PcdDeserialize: Sized {
    fn get_schema() -> Schema;
    fn read_line<R: BufRead>(
        reader: &mut R,
        schema: &Schema,
        line_count: &mut usize,
    ) -> Result<Self, Error>; // TODO correct Err variant
    fn read_bytes<R: BufRead>(
        reader: &mut R,
        schema: &Schema,
        byte_count: &mut usize,
    ) -> Result<Self, Error>; // TODO correct Err variant
}

impl<T: Scalar + Zero + FromStr> PcdDeserialize for PointXYZRGBA<T> {
    fn get_schema() -> Schema {
        use smallvec::smallvec;

        let value_kind = match type_name::<T>() {
            "f32" => ValueKind::F32,
            "f64" => ValueKind::F64,
            _ => panic!(), // TODO
        };

        let x = smallvec![FieldDef::from((value_kind, 1))];
        let y = smallvec![FieldDef::from((value_kind, 1))];
        let z = smallvec![FieldDef::from((value_kind, 1))];
        let rgba = smallvec![
            FieldDef::from((ValueKind::U32, 1)),
            FieldDef::from((ValueKind::F32, 1))
        ];
        let fields = HashMap::from([
            ("x".to_string(), x),
            ("y".to_string(), y),
            ("z".to_string(), z),
            ("rgba".to_string(), rgba),
        ]);
        Schema::new(fields)
    }

    fn read_line<R: BufRead>(
        reader: &mut R,
        schema: &Schema,
        line_count: &mut usize,
    ) -> Result<Self, Error> {
        if !schema.is_subset(&Self::get_schema()) {
            return Err(Error::MismatchSchema {
                expected: schema.clone(),
                found: Some(Self::get_schema()),
                additional: Some(format!(
                    "Schema of the file is not a subset of the point to be parsed (expected: {:?}, found: {:?}",
                    schema,
                    Self::get_schema(),
                )),
            });
        }

        let mut line = String::new();
        reader.read_line(&mut line)?;

        let tokens = line.split_ascii_whitespace().collect::<Vec<_>>();

        let build_error = |s: &str| Error::ParseValueError {
            data: super::DataKind::Ascii,
            ty: s.to_string(),
            pos: *line_count,
        };

        let x = tokens[0].parse::<T>().map_err(|_| build_error("float"))?;
        let y = tokens[1].parse::<T>().map_err(|_| build_error("float"))?;
        let z = tokens[2].parse::<T>().map_err(|_| build_error("float"))?;
        let color_kind = schema.fields["rgba"][0].value_kind;
        let rgba = match color_kind {
            ValueKind::U32 => Color::from(
                tokens[3]
                    .parse::<u32>()
                    .map_err(|_| build_error("integer"))?,
            ),
            ValueKind::F32 => {
                Color::from(tokens[3].parse::<f32>().map_err(|_| build_error("float"))?)
            }
            _ => unreachable!(
                "PointXYZRGBA should not have a color value kind different than U32 or F32"
            ),
        };

        let point = PointXYZRGBA::new_color(x, y, z, rgba);

        line_count.add_assign(1);

        Ok(point)
    }

    fn read_bytes<R: BufRead>(
        reader: &mut R,
        schema: &Schema,
        byte_count: &mut usize,
    ) -> Result<Self, Error> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, Cursor};

    use pretty_assertions::assert_eq;

    use crate::{
        io::pcd::{FieldDef, Schema, ValueKind},
        point::PointXYZRGBA,
    };

    use super::PcdDeserialize;

    #[test]
    fn parse_ascii_point_xyzrgba() {
        let bytes = b"1.0 2.0 3.0 4.2108e+06";
        let mut reader = BufReader::new(Cursor::new(bytes));
        let mut line_count = 1;
        let mut schema = Schema::empty();

        schema.insert("x", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("y", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("z", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("rgba", &[FieldDef::new(ValueKind::F32, 1)]);

        let point: Result<PointXYZRGBA<f32>, _> =
            PointXYZRGBA::read_line(&mut reader, &schema, &mut line_count);
        assert!(point.is_ok());
        let point = point.unwrap();

        let expected = PointXYZRGBA::new_color_float(1.0, 2.0, 3.0, 4.2108e+06);

        assert_eq!(point, expected);
    }
}
