use nom::{
    bytes::complete::{tag, take_while},
    character::complete::line_ending,
    combinator::map_res,
    multi::separated_list0,
    number::complete::float,
    sequence::{preceded, terminated},
    IResult,
};
use std::str::FromStr;

use crate::point::{PointXYZ, PointXYZRGBA, ViewPoint};

pub struct PcdReader {}

#[derive(Debug, PartialEq)]
pub struct PcdHeader {
    version: PcdVersion,
    fields: PcdField,
    size: Vec<u8>,
    types: Vec<PcdType>,
    counts: Vec<u8>,
    width: usize,
    height: usize,
    viewpoint: ViewPoint,
    points: usize,
    data_type: PcdDataType,
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum PcdVersion {
    V0_7,
}

impl FromStr for PcdVersion {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            ".7" => Ok(PcdVersion::V0_7),
            unknown => Err(format!("Unknown version string: {unknown}")),
        }
    }
}

impl PcdVersion {
    #[inline]
    fn from_bytes(input: &[u8]) -> Result<Self, String> {
        match input {
            b".7" => Ok(PcdVersion::V0_7),
            unknown => Err(format!(
                "Unknown version byte string: {}",
                std::str::from_utf8(unknown).unwrap() // TODO remove unwrap
            )),
        }
    }
}

// TODO!: Find all possible field types in the spec and add them here and the match arm in PcdFields::from_bytes
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PcdField {
    XYZ,
    XYZ_RGB,
    XYZ_RGBA,
    XYZ_NXNYNZ,
}

impl PcdField {
    #[inline]
    fn from_bytes(input: &[u8]) -> Result<Self, String> {
        match input {
            b"x y z" => Ok(PcdField::XYZ),
            b"x y z rgb" => Ok(PcdField::XYZ_RGBA),
            b"x y z rgba" => Ok(PcdField::XYZ_RGBA),
            b"x y z normal_x normal_y normal_z" => Ok(PcdField::XYZ_NXNYNZ),
            unknown => Err(format!(
                "Unknown fields type: {}",
                std::str::from_utf8(unknown).unwrap() // TODO remove unwrap
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PcdDataType {
    Ascii,
    Binary,
    BinaryCompressed,
}

impl PcdDataType {
    #[inline]
    fn from_bytes(input: &[u8]) -> Result<Self, String> {
        match input {
            b"ascii" => Ok(PcdDataType::Ascii),
            b"binary" => Ok(PcdDataType::Binary),
            b"binary_compressed" => Ok(PcdDataType::BinaryCompressed),
            unknown => Err(format!(
                "Unknown data byte string: {}",
                std::str::from_utf8(unknown).unwrap() // TODO remove unwrap
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PcdType {
    Signed,
    Unsigned,
    Float,
}

impl PcdType {
    #[inline]
    fn from_bytes(input: &[u8]) -> Result<Self, String> {
        match input {
            b"I" => Ok(PcdType::Signed),
            b"U" => Ok(PcdType::Unsigned),
            b"F" => Ok(PcdType::Float),
            unknown => Err(format!(
                "Unknown type byte string: {}",
                std::str::from_utf8(unknown).unwrap()
            )),
        }
    }
}

fn skip_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, _) = take_while(|c| c != b'\n')(input)?;
    tag(b"\n")(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], PcdHeader> {
    // TODO Do this better, there has to be a nom way
    let (input, _) = if input[0] == b'#' {
        skip_line(input)?
    } else {
        (input, &b""[..])
    };

    let (input, version) = terminated(parse_version, line_ending)(input)?;
    let (input, fields) = terminated(parse_fields, line_ending)(input)?;
    let (input, size) = terminated(parse_size, line_ending)(input)?;
    let (input, types) = terminated(parse_type, line_ending)(input)?;
    let (input, counts) = terminated(parse_count, line_ending)(input)?;
    let (input, width) = terminated(parse_width, line_ending)(input)?;
    let (input, height) = terminated(parse_height, line_ending)(input)?;
    let (input, viewpoint) = terminated(parse_viewpoint, line_ending)(input)?;
    let (input, points) = terminated(parse_points, line_ending)(input)?;
    let (input, data_type) = terminated(parse_data, line_ending)(input)?;

    let header = PcdHeader {
        version,
        fields,
        size,
        types,
        counts,
        width,
        height,
        viewpoint,
        points,
        data_type,
    };

    Ok((input, header))
}

fn parse_version(input: &[u8]) -> IResult<&[u8], PcdVersion> {
    let (input, version) = map_res(
        preceded(tag(b"VERSION "), take_while(|c| c != b'\n')),
        PcdVersion::from_bytes,
    )(input)?;

    // let version = PcdVersion::from_bytes(version_bytes); // TODO maybe do not map_res directly as we lose the error of the from_bytes function
    Ok((input, version))
}

fn parse_fields(input: &[u8]) -> IResult<&[u8], PcdField> {
    let (input, fields) = map_res(
        preceded(tag(b"FIELDS "), take_while(|c| c != b'\n')),
        PcdField::from_bytes,
    )(input)?;

    // let field = PcdFields::from_bytes(fields); // TODO maybe do not map_res directly as we lose the error of the from_bytes function
    Ok((input, fields))
}

// TODO Write unit tests
fn parse_size(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, sizes) = preceded(tag(b"SIZE "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, sizes) = separated_list0(tag(b" "), nom::character::complete::u8)(sizes)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap() // TODO remove unwrap
    );

    Ok((input, sizes))
}

// TODO Write more unit tests
fn parse_type(input: &[u8]) -> IResult<&[u8], Vec<PcdType>> {
    let (input, types) = preceded(tag(b"TYPE "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, types) = separated_list0(
        tag(b" "),
        map_res(
            nom::bytes::complete::take_till(|c| c == b' '),
            PcdType::from_bytes,
        ),
    )(types)?;
    assert!(
        remainder.is_empty(),
        "While parsing the types of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap() // TODO remove unwrap
    );

    Ok((input, types))
}

// TODO Write unit tests
fn parse_count(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, counts) = preceded(tag(b"COUNT "), take_while(|c| c != b'\n'))(input)?;

    let (remainder, counts) = separated_list0(tag(b" "), nom::character::complete::u8)(counts)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap() // TODO remove unwrap
    );

    Ok((input, counts))
}

fn parse_width(input: &[u8]) -> IResult<&[u8], usize> {
    let (input, width) = preceded(
        tag(b"WIDTH "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            usize::from_str,
        ),
    )(input)?;
    Ok((input, width))
}

// TODO Write unit tests
fn parse_height(input: &[u8]) -> IResult<&[u8], usize> {
    let (input, height) = preceded(
        tag(b"HEIGHT "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            usize::from_str,
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

fn parse_points(input: &[u8]) -> IResult<&[u8], usize> {
    let (input, points) = preceded(
        tag(b"POINTS "),
        map_res(
            map_res(take_while(|c| c != b'\n'), std::str::from_utf8),
            usize::from_str,
        ),
    )(input)?;
    Ok((input, points))
}

fn parse_data(input: &[u8]) -> IResult<&[u8], PcdDataType> {
    let (input, point_type) = map_res(
        preceded(tag(b"DATA "), take_while(|c| c != b'\n')),
        PcdDataType::from_bytes,
    )(input)?;

    // let point_type = PcdDataType::from_bytes(point_type);
    Ok((input, point_type))
}

fn parse_point_xyz(input: &[u8]) -> IResult<&[u8], PointXYZ> {
    let (input, points) = take_while(|c| c != b'\n')(input)?;

    let (remainder, points) = separated_list0(tag(b" "), nom::number::complete::float)(points)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap() // TODO remove unwrap
    );

    let point = PointXYZ::from(&points[..]);

    Ok((input, point))
}

fn parse_point_xyzrgba(input: &[u8]) -> IResult<&[u8], PointXYZRGBA> {
    let (input, points) = take_while(|c| c != b'\n')(input)?;

    let (remainder, points) = separated_list0(tag(b" "), nom::number::complete::float)(points)?;
    assert!(
        remainder.is_empty(),
        "While parsing the sizes of the PCD file, something wasn't parsed to the end and this remained: [{}]",
        std::str::from_utf8(remainder).unwrap() // TODO remove unwrap
    );

    let point = PointXYZRGBA::try_from(&points[..]).unwrap();

    Ok((input, point))
}

fn parse_points_xyzrgba(input: &[u8]) -> IResult<&[u8], Vec<PointXYZRGBA>> {
    todo!()
}

#[cfg(test)]
mod tests {

    use crate::{
        io::parse_point_xyzrgba,
        point::{PointXYZ, PointXYZRGBA, ViewPoint},
    };

    use super::{
        parse_data, parse_fields, parse_header, parse_point_xyz, parse_size, parse_type,
        parse_version, parse_viewpoint, parse_width, PcdDataType, PcdField, PcdHeader, PcdType,
        PcdVersion,
    };

    #[test]
    fn test_parse_version_valid_input() {
        let input = b"VERSION .7";

        let (remainder, result) = parse_version(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, PcdVersion::V0_7);
    }

    #[test]
    fn test_parse_version_invalid_version() {
        let input = b"VERSION .11";

        let result = parse_version(input);
        assert!(result.is_err());
        // assert_eq!(
        //     result,
        //     Err(String::from("Unknown version byte string: .11"))
        // );
    }

    #[test]
    fn test_parse_version_missing_version_number() {
        let input = b"VERSION ";

        let result = parse_version(input);
        assert!(result.is_err());
        // assert_eq!(result, Err(String::from("Unknown version byte string: ")));
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
    fn test_parse_fields_valid_input_xyz() {
        let input = b"FIELDS x y z";

        let (remainder, result) = parse_fields(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, PcdField::XYZ);
    }

    #[test]
    fn test_parse_fields_valid_input_xyzrgb() {
        let input = b"FIELDS x y z rgb\nremainder";

        let (remainder, result) = parse_fields(input).unwrap();
        assert!(!remainder.is_empty()); // We check here wether we get the correct remainder. '\nremainder' should be returned
        assert_eq!(remainder, b"\nremainder");
        assert_eq!(result, PcdField::XYZ_RGBA);
    }

    #[test]
    fn test_parse_fields_valid_input_xyzrgba() {
        let input = b"FIELDS x y z rgba";

        let (remainder, result) = parse_fields(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, PcdField::XYZ_RGBA);
    }

    #[test]
    fn test_parse_fields_valid_input_xyznormals() {
        let input = b"FIELDS x y z normal_x normal_y normal_z";

        let (remainder, result) = parse_fields(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, PcdField::XYZ_NXNYNZ);
    }

    #[test]
    fn test_parse_fields_missing_desc() {
        let input = b"FIELDS ";

        let result = parse_fields(input);
        assert!(result.is_err());
        // assert!(remainder.is_empty());
        // assert_eq!(result, Err(String::from("Unknown fields type: ")));
    }

    #[test]
    fn test_parse_fields_invalid_input() {
        let input = b"FIEL";

        let result = parse_fields(input);
        assert_eq!(
            result,
            Err(nom::Err::Error(nom::error::Error::new(
                &[70, 73, 69, 76][..],
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

        let (remainder, result) = parse_type(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            vec![
                PcdType::Float,
                PcdType::Float,
                PcdType::Float,
                PcdType::Float,
            ]
        );

        let input = b"TYPE F F I U";

        let (remainder, result) = parse_type(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(
            result,
            vec![
                PcdType::Float,
                PcdType::Float,
                PcdType::Signed,
                PcdType::Unsigned,
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
        assert_eq!(result, usize::MIN);

        // TODO This is very probably wrong for non 64-bit systems
        let input = b"WIDTH 18446744073709551615";
        let (remainder, result) = parse_width(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, usize::MAX);
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
        assert_eq!(result, PcdDataType::Ascii);

        let input = b"DATA binary\nremainder";
        let (remainder, result) = parse_data(input).unwrap();
        assert!(!remainder.is_empty());
        assert_eq!(remainder, b"\nremainder");
        assert_eq!(result, PcdDataType::Binary);

        let input = b"DATA binary_compressed";
        let (remainder, result) = parse_data(input).unwrap();
        assert!(remainder.is_empty());
        assert_eq!(result, PcdDataType::BinaryCompressed);
    }

    #[test]
    fn test_parse_header_valid_input() {
        let input = b"# .PCD v.7 - Point Cloud Data file format\nVERSION .7\nFIELDS x y z rgb\nSIZE 4 4 4 4\nTYPE F F F F\nCOUNT 1 1 1 1\nWIDTH 213\nHEIGHT 1\nVIEWPOINT 0 0 0 1 0 0 0\nPOINTS 213\nDATA ascii\n";

        let result = parse_header(input);
        assert!(result.is_ok());
        let (remainder, header) = result.unwrap();

        let expected = PcdHeader {
            version: PcdVersion::V0_7,
            fields: PcdField::XYZ_RGBA,
            size: vec![4, 4, 4, 4],
            types: vec![
                PcdType::Float,
                PcdType::Float,
                PcdType::Float,
                PcdType::Float,
            ],
            counts: vec![1, 1, 1, 1],
            width: 213,
            height: 1,
            viewpoint: ViewPoint::from(&[0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0][..]),
            points: 213,
            data_type: PcdDataType::Ascii,
        };
        assert_eq!(header, expected);
    }

    #[test]
    fn test_parse_point_xyz_valid_input() {
        let input = b"0.93773 0.33763 0";

        let result = parse_point_xyz(input);
        assert!(result.is_ok());
        let (remainder, point) = result.unwrap();
        assert!(remainder.is_empty());

        assert_eq!(point, PointXYZ::new(0.93773, 0.33763, 0.0))
    }

    #[test]
    fn test_parse_point_xyzrgba_valid_input() {
        let input = b"0.93773 0.33763 0 4.2108e+06";

        let result = parse_point_xyzrgba(input);
        assert!(result.is_ok());
        let (remainder, point) = result.unwrap();
        assert!(remainder.is_empty());

        assert_eq!(point, PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06))
    }
}