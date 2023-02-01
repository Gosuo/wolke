use std::str::FromStr;

use nom::{
    bytes::complete::{tag, take_while},
    character::complete::line_ending,
    combinator::map_res,
    multi::separated_list0,
    number::complete::float,
    sequence::{preceded, terminated},
    IResult,
};

use crate::{io::Error, point::ViewPoint};

use super::{DataKind, PcdHeader, Schema, TypeKind, ValueKind, Version};

fn parse_header(input: &[u8]) -> Result<PcdHeader, Error> {
    let total_size = input.len();
    // TODO Do this better, there has to be a nom way
    let (input, _) = if input[0] == b'#' {
        skip_line(input)?
    } else {
        (input, &b""[..])
    };

    let (input, version) = terminated(parse_version, line_ending)(input)?;
    let (input, fields) = terminated(parse_fields, line_ending)(input)?;
    let (input, sizes) = terminated(parse_size, line_ending)(input)?;
    let (input, types) = terminated(parse_types, line_ending)(input)?;
    let (input, counts) = terminated(parse_count, line_ending)(input)?;
    let (input, width) = terminated(parse_width, line_ending)(input)?;
    let (input, height) = terminated(parse_height, line_ending)(input)?;
    let (input, viewpoint) = terminated(parse_viewpoint, line_ending)(input)?;
    let (input, num_points) = terminated(parse_points, line_ending)(input)?;
    let (input, data) = terminated(parse_data, line_ending)(input)?;

    let value_kinds = types.into_iter().zip(sizes.iter()).map(ValueKind::from);
    // .collect::<Vec<_>>();

    let schema = Schema::from_iters(fields.into_iter(), value_kinds, counts.into_iter()).unwrap(); // TODO Remove unwrap

    let data_offset = total_size - input.len();

    let header = PcdHeader {
        version,
        width,
        height,
        viewpoint,
        num_points,
        data,
        data_offset,
        schema,
    };

    Ok(header)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], Version> {
    map_res(
        preceded(tag(b"VERSION "), take_while(|c| c != b'\n')),
        Version::from_bytes,
    )(input)
}

// TODO Write unit tests
fn parse_fields(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let (input, fields) = map_res(
        preceded(tag(b"FIELDS "), take_while(|c| c != b'\n')),
        std::str::from_utf8,
    )(input)?;

    let fields = fields
        .split_ascii_whitespace()
        .map(|s| s.to_string())
        .collect();

    Ok((input, fields))
}

// TODO Write unit tests
fn parse_size(input: &[u8]) -> IResult<&[u8], Vec<u64>> {
    let (input, sizes) = preceded(tag(b"SIZE "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, sizes) = separated_list0(tag(b" "), nom::character::complete::u64)(sizes)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
    );

    Ok((input, sizes))
}

// TODO Write more unit tests
fn parse_types(input: &[u8]) -> IResult<&[u8], Vec<TypeKind>> {
    let (input, types) = preceded(tag(b"TYPE "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, types) = separated_list0(
        tag(b" "),
        map_res(
            nom::bytes::complete::take_till(|c| c == b' '),
            TypeKind::from_bytes,
        ),
    )(types)?;
    assert!(
        remainder.is_empty(),
        "While parsing the types of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
    );

    Ok((input, types))
}

// TODO Write unit tests
fn parse_count(input: &[u8]) -> IResult<&[u8], Vec<u64>> {
    let (input, counts) = preceded(tag(b"COUNT "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, counts) = separated_list0(tag(b" "), nom::character::complete::u64)(counts)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
    );

    Ok((input, counts))
}

fn parse_width(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, width) = preceded(
        tag(b"WIDTH "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            u64::from_str,
        ),
    )(input)?;
    Ok((input, width))
}

// TODO Write unit tests
fn parse_height(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, height) = preceded(
        tag(b"HEIGHT "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            u64::from_str,
        ),
    )(input)?;
    Ok((input, height))
}

fn parse_viewpoint(input: &[u8]) -> IResult<&[u8], ViewPoint> {
    let (input, points) = preceded(tag(b"VIEWPOINT "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, points) = separated_list0(tag(b" "), float)(points)?;
    assert!(remainder.is_empty());

    let viewpoint = ViewPoint::from(&points[..]);

    Ok((input, viewpoint))
}

fn parse_points(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, points) = preceded(
        tag(b"POINTS "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            u64::from_str,
        ),
    )(input)?;
    Ok((input, points))
}

fn parse_data(input: &[u8]) -> IResult<&[u8], DataKind> {
    let (input, point_type) = map_res(
        preceded(tag(b"DATA "), take_while(|c| c != b'\n')),
        DataKind::from_bytes,
    )(input)?;

    Ok((input, point_type))
}

fn skip_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, _) = take_while(|c| c != b'\n')(input)?;
    tag(b"\n")(input)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        io::pcd::{
            parser::{
                parse_data, parse_header, parse_size, parse_types, parse_version, parse_viewpoint,
                parse_width,
            },
            DataKind, FieldDef, PcdHeader, Schema, TypeKind, ValueKind, Version,
        },
        point::ViewPoint,
    };

    #[test]
    fn test_parse_version_valid_input() {
        let input = b"VERSION .7";

        let (remainder, result) = parse_version(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, Version::V0_7);
    }

    #[test]
    fn test_parse_version_invalid_version() {
        let input = b"VERSION .11";

        let result = parse_version(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_version_missing_version_number() {
        let input = b"VERSION ";

        let result = parse_version(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_version_invalid_input() {
        let input = b"VERSI";

        let result = parse_version(input);
        assert_eq!(
            result,
            Err(nom::Err::Error(nom::error::Error::new(
                &[86, 69, 82, 83, 73][..],
                nom::error::ErrorKind::Tag
            )))
        );
    }

    #[test]
    fn test_parse_size_valid_input() {
        let input = b"SIZE 4 4 4 4";

        let (remainder, result) = parse_size(input).unwrap();
        assert!(remainder.is_empty());

        assert_eq!(result, vec![4, 4, 4, 4]);
    }

    #[test]
    fn test_parse_type_valid_input() {
        let input = b"TYPE F F F F";

        let (remainder, result) = parse_types(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            vec![
                TypeKind::Float,
                TypeKind::Float,
                TypeKind::Float,
                TypeKind::Float,
            ]
        );

        let input = b"TYPE F F I U";

        let (remainder, result) = parse_types(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            vec![
                TypeKind::Float,
                TypeKind::Float,
                TypeKind::Signed,
                TypeKind::Unsigned,
            ]
        );
    }

    #[test]
    fn test_parse_width_valid_input() {
        let input = b"WIDTH 231";
        let (remainder, result) = parse_width(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, 231);

        let input = b"WIDTH 2";
        let (remainder, result) = parse_width(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, 2);

        let input = b"WIDTH 0\nremainder";
        let (remainder, result) = parse_width(input).unwrap();
        assert!(!remainder.is_empty()); // We check here wether we get the correct remainder. '\nremainder' should be returned
        assert_eq!(result, u64::MIN);

        let max_bytes = u64::MAX.to_string().as_bytes().to_vec();
        let input = [b"WIDTH ".to_vec(), max_bytes].concat();
        let (remainder, result) = parse_width(&input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, u64::MAX);
    }

    #[test]
    fn test_parse_width_invalid_usize() {
        let input = b"WIDTH -1";
        let result = parse_width(input);
        assert_eq!(
            result,
            Err(nom::Err::Error(nom::error::Error::new(
                &[45, 49][..],
                nom::error::ErrorKind::MapRes
            )))
        );

        let input = b"WIDTH -100";
        let result = parse_width(input);
        assert_eq!(
            result,
            Err(nom::Err::Error(nom::error::Error::new(
                &[45, 49, 48, 48][..],
                nom::error::ErrorKind::MapRes
            )))
        );
    }

    #[test]
    fn test_parse_viewpoint_valid_input() {
        let input = b"VIEWPOINT 0 0 0 1 0 0 0";
        let (remainder, result) = parse_viewpoint(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            ViewPoint::from(&[0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0][..])
        );

        let input = b"VIEWPOINT 1 0 100 1 0 2.0 2.3";
        let (remainder, result) = parse_viewpoint(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            ViewPoint::from(&[1.0, 0.0, 100.0, 1.0, 0.0, 2.0, 2.3][..])
        );

        let input = b"VIEWPOINT -1 -10 100 1 -0 2.0 2.3\nremainder";
        let (remainder, result) = parse_viewpoint(input).unwrap();
        assert!(!remainder.is_empty()); // We check here wether we get the correct remainder. '\nremainder' should be returned
        assert_eq!(remainder, b"\nremainder");
        assert_eq!(
            result,
            ViewPoint::from(&[-1.0, -10.0, 100.0, 1.0, -0.0, 2.0, 2.3][..])
        );
    }

    #[test]
    fn test_parse_data_types_valid_input() {
        let input = b"DATA ascii";
        let (remainder, result) = parse_data(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, DataKind::Ascii);

        let input = b"DATA binary\nremainder";
        let (remainder, result) = parse_data(input).unwrap();
        assert!(!remainder.is_empty());
        assert_eq!(remainder, b"\nremainder");
        assert_eq!(result, DataKind::Binary);

        let input = b"DATA binary_compressed";
        let (remainder, result) = parse_data(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, DataKind::BinaryCompressed);
    }

    #[test]
    fn parse_header_valid_input() {
        let input = include_bytes!("../../../data/table_scene_mug_stereo_textured.pcd");

        let result = parse_header(input);
        assert!(result.is_ok());
        let result = result.unwrap();

        let expected = PcdHeader {
            version: Version::V0_7,
            width: 640,
            height: 480,
            viewpoint: ViewPoint::from(&[0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0][..]),
            num_points: 307200,
            data: DataKind::BinaryCompressed,
            data_offset: 195,
            schema: Schema {
                fields: vec![
                    FieldDef::from(("x".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("y".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("z".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("rgba".to_string(), ValueKind::U32, 1)),
                ],
            },
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_header_valid_input2() {
        let input = include_bytes!("../../../data/example.pcd");

        let result = parse_header(input);
        assert!(result.is_ok());
        let result = result.unwrap();

        let expected = PcdHeader {
            version: Version::V0_7,
            width: 213,
            height: 1,
            viewpoint: ViewPoint::from(&[0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0][..]),
            num_points: 213,
            data: DataKind::Ascii,
            data_offset: 175,
            schema: Schema {
                fields: vec![
                    FieldDef::from(("x".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("y".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("z".to_string(), ValueKind::F32, 1)),
                    FieldDef::from(("rgb".to_string(), ValueKind::F32, 1)),
                ],
            },
        };

        assert_eq!(result, expected);
    }
}
