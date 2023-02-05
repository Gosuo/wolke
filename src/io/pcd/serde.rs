use std::{collections::HashMap, io::BufRead, ops::AddAssign};

use byteorder::{LittleEndian, ReadBytesExt};

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

impl PcdDeserialize for PointXYZRGBA {
    fn get_schema() -> Schema {
        use smallvec::smallvec;

        let value_kind = ValueKind::F32;

        let x = smallvec![FieldDef::from((value_kind, 1))];
        let y = smallvec![FieldDef::from((value_kind, 1))];
        let z = smallvec![FieldDef::from((value_kind, 1))];
        let rgba = smallvec![
            FieldDef::from((ValueKind::U32, 1)),
            FieldDef::from((ValueKind::F32, 1)),
            FieldDef::from((ValueKind::U8, 4)),
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

        let x = tokens[0].parse::<f32>().map_err(|_| build_error("float"))?;
        let y = tokens[1].parse::<f32>().map_err(|_| build_error("float"))?;
        let z = tokens[2].parse::<f32>().map_err(|_| build_error("float"))?;
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

        let x = { reader.read_f32::<LittleEndian>()? };
        byte_count.add_assign(4);
        let y = { reader.read_f32::<LittleEndian>()? };
        byte_count.add_assign(4);
        let z = { reader.read_f32::<LittleEndian>()? };
        byte_count.add_assign(4);
        let rgba = {
            let FieldDef { value_kind, count } = schema["rgba"][0];
            match (value_kind, count) {
                (ValueKind::F32, 1) => Color::from(reader.read_f32::<LittleEndian>()?),
                (ValueKind::U32, 1) => Color::from(reader.read_u32::<LittleEndian>()?),
                (ValueKind::U8, 4) => {
                    let (r, g, b, a) = (
                        reader.read_u8()?,
                        reader.read_u8()?,
                        reader.read_u8()?,
                        reader.read_u8()?,
                    );
                    Color::new(r, g, b, a)
                }
                _ => {
                    return Err(Error::ParseValueError {
                        data: super::DataKind::Binary,
                        ty: "Color (rgba)".to_string(),
                        pos: *byte_count,
                    })
                }
            }
        };
        byte_count.add_assign(4);

        let point = PointXYZRGBA::new_color(x, y, z, rgba);

        Ok(point)
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

        let point = PointXYZRGBA::read_line(&mut reader, &schema, &mut line_count);
        assert!(point.is_ok());
        let point = point.unwrap();

        let expected = PointXYZRGBA::new_color_float(1.0, 2.0, 3.0, 4.2108e+06);

        assert_eq!(point, expected);
    }

    #[test]
    fn parse_binary_point_xyzrgba() {
        let x = 1.0_f32.to_le_bytes();
        let y = 2.0_f32.to_le_bytes();
        let z = 3.0_f32.to_le_bytes();
        let rgba = 4.2108e+06_f32.to_le_bytes();
        let mut point_bytes = x.to_vec();
        point_bytes.extend_from_slice(&y);
        point_bytes.extend_from_slice(&z);
        point_bytes.extend_from_slice(&rgba);

        let mut reader = BufReader::new(Cursor::new(point_bytes));

        let mut schema = Schema::empty();

        schema.insert("x", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("y", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("z", &[FieldDef::new(ValueKind::F32, 1)]);
        schema.insert("rgba", &[FieldDef::new(ValueKind::F32, 1)]);

        let mut byte_count = 0;
        let point = PointXYZRGBA::read_bytes(&mut reader, &schema, &mut byte_count);
        assert!(point.is_ok());
        let point = point.unwrap();

        let expected = PointXYZRGBA::new_color_float(1.0, 2.0, 3.0, 4.2108e+06);

        assert_eq!(point, expected);
    }
}
