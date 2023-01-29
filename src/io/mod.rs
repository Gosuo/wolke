use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while},
    character::complete::line_ending,
    combinator::{eof, map_res},
    multi::{many0, separated_list0},
    number::complete::float,
    sequence::{preceded, terminated},
    IResult,
};

use std::{
    path::{Path, PathBuf},
    str::FromStr,
};
use thiserror::Error;

use crate::point::{PointCloud, PointCloudType, PointXYZ, PointXYZRGBA, ViewPoint};

#[derive(Debug, Error)]
pub enum PcdParseError {
    #[error("Empty buffer in PcdReader, did you call PcdReader::read?")]
    EmptyBuffer,
    #[error("Nom parsing error {err} -> {error_kind}")]
    NomError { err: String, error_kind: String },
}

impl From<nom::Err<nom::error::Error<&[u8]>>> for PcdParseError {
    fn from(value: nom::Err<nom::error::Error<&[u8]>>) -> Self {
        Self::NomError {
            err: value.to_string(),
            error_kind: value.to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PcdReader {
    path: PathBuf,
    bytes: Vec<u8>,
}

impl PcdReader {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
            bytes: vec![],
        }
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        Self {
            path: PathBuf::new(),
            bytes,
        }
    }

    pub fn parse(&self) -> Result<PointCloudType, PcdParseError> {
        if self.bytes.is_empty() {
            return Err(PcdParseError::EmptyBuffer);
        }

        let (input, header) = parse_header(&self.bytes)?;

        match header.data_type {
            PcdDataType::Ascii => match header.fields {
                PcdField::XYZ => todo!(),
                PcdField::XYZ_RGB => todo!(),
                PcdField::XYZ_RGBA => {
                    let (eof, points) = parse_all_xyzrgba_ascii(input)?;

                    let cloud = PointCloud::new(points, header.width, header.height);
                    Ok(PointCloudType::XYZ_RGBA(cloud))
                }
                PcdField::XYZ_NXNYNZ => todo!(),
            },
            PcdDataType::Binary => todo!(),
            PcdDataType::BinaryCompressed => todo!(),
        }
    }

    pub fn read(&mut self) -> Result<(), std::io::Error> {
        self.bytes = std::fs::read(&self.path)?;
        Ok(())
    }
}

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
    data_offset: usize,
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
                std::str::from_utf8(unknown).unwrap_or("Unable to parse byte string")
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
                std::str::from_utf8(unknown).unwrap_or("Unable to parse byte string")
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
                std::str::from_utf8(unknown).unwrap_or("Unable to parse byte string")
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
    // Save the original length of the input, to calculate the data offset
    let original_length = input.len();

    // TODO Do this better, there has to be a nom way
    let (input, _) = if input[0] == b'#' {
        skip_line(input)?
    } else {
        (input, &b""[..])
    };

    let (input, version) = terminated(parse_version, line_ending)(input)?;
    let (input, fields) = terminated(parse_fields, line_ending)(input)?;
    let (input, size) = terminated(parse_size, line_ending)(input)?;
    let (input, types) = terminated(parse_types, line_ending)(input)?;
    let (input, counts) = terminated(parse_count, line_ending)(input)?;
    let (input, width) = terminated(parse_width, line_ending)(input)?;
    let (input, height) = terminated(parse_height, line_ending)(input)?;
    let (input, viewpoint) = terminated(parse_viewpoint, line_ending)(input)?;
    let (input, points) = terminated(parse_points, line_ending)(input)?;
    let (input, data_type) = terminated(parse_data, line_ending)(input)?;

    // Calculate the data offset here so we can easily start parsing the points from that point on. Important for binary parsing
    let data_offset = original_length - input.len();

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
        data_offset,
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
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
    );

    Ok((input, sizes))
}

// TODO Write more unit tests
fn parse_types(input: &[u8]) -> IResult<&[u8], Vec<PcdType>> {
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
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
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
        std::str::from_utf8(remainder).unwrap_or("Unable to parse byte string")
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

fn parse_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    // terminated(take_till(|c| c == b'\n'), line_ending)(input)
    terminated(is_not("\n"), alt((tag(b"\n"), eof)))(input)
}

fn parse_all_lines(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
    many0(parse_line)(input)
}

macro_rules! parse_ascii_impl {
    ($point_type:ty, $parser:expr, $count:expr, $fn_name:ident) => {
        fn $fn_name(input: &[u8]) -> IResult<&[u8], $point_type> {
            let (input, point_data) = take_while(|c| c != b'\n')(input)?;

            let (remainder, point_data) = separated_list0(tag(b" "), $parser)(point_data)?;
            assert!(point_data.len() == $count);

            let point = <$point_type>::try_from(&point_data[..]).unwrap();
            Ok((input, point))
        }
    };
}

macro_rules! parse_multi_ascii_impl {
    ($point_type:ty, $parser:expr, $fn_name:ident) => {
        fn $fn_name(input: &[u8]) -> IResult<&[u8], Vec<$point_type>> {
            let (input, points) = many0(map_res(parse_line, $parser))(input)?;

            let points = points
                .into_iter()
                .map(|(remain, point)| point)
                .collect::<Vec<_>>();

            Ok((input, points))
        }
    };
}

parse_ascii_impl!(PointXYZ, float, 3, parse_point_xyz_ascii);
parse_ascii_impl!(PointXYZRGBA, float, 4, parse_point_xyzrgba_ascii);

parse_multi_ascii_impl!(
    PointXYZRGBA,
    parse_point_xyzrgba_ascii,
    parse_all_xyzrgba_ascii
);

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::point::{PointCloud, PointCloudType, PointXYZRGBA, ViewPoint};

    use super::{
        parse_data, parse_fields, parse_header, parse_line, parse_point_xyzrgba_ascii, parse_size,
        parse_types, parse_version, parse_viewpoint, parse_width, PcdDataType, PcdField, PcdHeader,
        PcdReader, PcdType, PcdVersion,
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

        let (remainder, result) = parse_types(input).unwrap();
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

        let (remainder, result) = parse_types(input).unwrap();
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

        let max_bytes = usize::MAX.to_string().as_bytes().to_vec();
        let input = [b"WIDTH ".to_vec(), max_bytes].concat();
        let (remainder, result) = parse_width(&input).unwrap();
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
            data_offset: 175,
        };
        assert_eq!(header, expected);
    }

    #[test]
    fn test_parse_point_ascii_valid_input() {
        let input = b"0.93773 0.33763 0 4.2108e+06";
        let pcd_point_type = PcdField::XYZ_RGBA;

        let result = parse_point_xyzrgba_ascii(input);
        assert!(result.is_ok());
        let (remainder, point) = result.unwrap();
        assert!(remainder.is_empty());

        assert_eq!(point, PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06));

        let input = b"0.93773 0.33763 0 5";
        let pcd_point_type = PcdField::XYZ;

        let result = parse_point_xyzrgba_ascii(input);
        assert!(result.is_ok());
        let (remainder, point) = result.unwrap();
        assert!(remainder.is_empty());

        assert_eq!(point, PointXYZRGBA::new(0.93773, 0.33763, 0.0, 5.0));
    }

    #[test]
    fn test_parse_line_valid_input() {
        let input = b"line1\n";

        let result = parse_line(input);
        assert!(result.is_ok());
        let (remainder, line) = result.unwrap();

        assert_eq!(line, b"line1");
        assert!(remainder.is_empty());
    }

    #[test]
    fn test_parse_pcd_valid_input() {
        let input = b"# .PCD v.7 - Point Cloud Data file format\nVERSION .7\nFIELDS x y z rgb\nSIZE 4 4 4 4\nTYPE F F F F\nCOUNT 1 1 1 1\nWIDTH 3\nHEIGHT 1\nVIEWPOINT 0 0 0 1 0 0 0\nPOINTS 3\nDATA ascii\n0.93773 0.33763 0 4.2108e+06\n0.93773 0.33763 0 4.2108e+06\n0.93773 0.33763 0 4.2108e+06";

        let reader = PcdReader::from_bytes(input.to_vec());

        let cloud = dbg!(reader.parse());
        assert!(cloud.is_ok());
        let cloud = cloud.unwrap();

        let points = vec![
            PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06),
        ];

        let cloud = match cloud {
            PointCloudType::XYZ_RGBA(c) => c,
            _ => panic!("PointCloudType should be XYZ_RGBA"),
        };

        assert_eq!(cloud, PointCloud::new(points, 3, 1));
    }

    #[test]
    fn test_parse_pcd_valid_ascii_input() {
        let input = include_bytes!("../../data/example.pcd");

        let reader = PcdReader::from_bytes(input.to_vec());
        let cloud = reader.parse();
        assert!(cloud.is_ok());

        let cloud = cloud.unwrap();

        let cloud = match cloud {
            PointCloudType::XYZ_RGBA(c) => c,
            _ => panic!("PointCloudType should be XYZ_RGBA"),
        };

        let expected_points = vec![
            PointXYZRGBA::new(0.93773, 0.33763, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.90805, 0.35641, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.81915, 0.32, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.97192, 0.278, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.944, 0.29474, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.98111, 0.24247, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.93655, 0.26143, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.91631, 0.27442, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.81921, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.90701, 0.24109, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.83239, 0.23398, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.99185, 0.2116, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.89264, 0.21174, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.85082, 0.21212, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.81044, 0.32222, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.74459, 0.32192, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.69927, 0.32278, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.8102, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.75504, 0.29765, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.8102, 0.24399, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.74995, 0.24723, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.68049, 0.29768, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.66509, 0.29002, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.69441, 0.2526, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.62807, 0.22187, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.58706, 0.32199, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.52125, 0.31955, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.49351, 0.32282, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.44313, 0.32169, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.58678, 0.2929, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.53436, 0.29164, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.59308, 0.24134, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.5357, 0.2444, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.50043, 0.31235, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.44107, 0.29711, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.50727, 0.22193, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.43957, 0.23976, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.8105, 0.21112, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.73555, 0.2114, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.69907, 0.21082, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.63327, 0.21154, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.59165, 0.21201, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.52477, 0.21491, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.49375, 0.21006, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.4384, 0.19632, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.43425, 0.16052, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.3787, 0.32173, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.33444, 0.3216, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.23815, 0.32199, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.3788, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.33058, 0.31073, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.3788, 0.24399, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.30249, 0.29189, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.23492, 0.29446, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.29465, 0.24399, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.23514, 0.24172, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.18836, 0.32277, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.15992, 0.32176, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.08642, 0.32181, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.039994, 0.32283, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.20039, 0.31211, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.1417, 0.29506, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.20921, 0.22332, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.13884, 0.24227, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.085123, 0.29441, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.048446, 0.31279, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.086957, 0.24399, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.3788, 0.21189, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.29465, 0.19323, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.23755, 0.19348, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.29463, 0.16054, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.23776, 0.16054, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.19016, 0.21038, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.15704, 0.21245, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.08678, 0.21169, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.012746, 0.32168, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.075715, 0.32095, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10622, 0.32304, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.16391, 0.32118, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.00088411, 0.29487, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.057568, 0.29457, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.0034333, 0.24399, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.055185, 0.24185, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10983, 0.31352, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.15082, 0.29453, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.11534, 0.22049, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.15155, 0.24381, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.1912, 0.32173, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.281, 0.3185, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.30791, 0.32307, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.33854, 0.32148, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.21248, 0.29805, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.26372, 0.29905, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.22562, 0.24399, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.25035, 0.2371, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.29941, 0.31191, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.35845, 0.2954, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.29231, 0.22236, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.36101, 0.24172, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.0034393, 0.21129, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.07306, 0.21304, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10579, 0.2099, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.13642, 0.21411, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.22562, 0.19323, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.24439, 0.19799, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.22591, 0.16041, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.23466, 0.16082, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.3077, 0.20998, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.3413, 0.21239, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.40551, 0.32178, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.50568, 0.3218, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.41732, 0.30844, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.44237, 0.28859, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.41591, 0.22004, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.44803, 0.24236, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.50623, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.50916, 0.24296, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.57019, 0.22334, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.59611, 0.32199, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.65104, 0.32199, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.72566, 0.32129, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.75538, 0.32301, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.59653, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.65063, 0.29315, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.59478, 0.24245, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.65063, 0.24399, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.70618, 0.29525, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.76203, 0.31284, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.70302, 0.24183, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.77062, 0.22133, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.41545, 0.21099, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.45004, 0.19812, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.4475, 0.1673, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.52031, 0.21236, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.55182, 0.21045, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.5965, 0.21131, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.65064, 0.2113, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.72216, 0.21286, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.7556, 0.20987, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.78343, 0.31973, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.87572, 0.32111, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.90519, 0.32263, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.95526, 0.34127, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.79774, 0.29271, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.85618, 0.29497, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.79975, 0.24326, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.8521, 0.24246, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.91157, 0.31224, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.95031, 0.29572, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.92223, 0.2213, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.94979, 0.24354, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.78641, 0.21505, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.87094, 0.21237, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.90637, 0.20934, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.93777, 0.21481, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.22244, -0.0296, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.2704, -0.078167, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.24416, -0.056883, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.27311, -0.10653, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.26172, -0.10653, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.2704, -0.1349, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.24428, -0.15599, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.19017, -0.025297, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.14248, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.19815, -0.037432, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.14248, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.093313, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.044144, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.093313, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.044144, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.21156, -0.17357, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.029114, -0.12594, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.036583, -0.15619, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.22446, -0.20514, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.2208, -0.2369, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.2129, -0.208, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.19316, -0.25672, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.14497, -0.27484, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.030167, -0.18748, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.1021, -0.27453, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.1689, -0.2831, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.13875, -0.28647, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.086993, -0.29568, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.044924, -0.3154, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.0066125, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.057362, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.0066125, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.057362, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10653, -0.02428, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.15266, -0.025282, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10653, -0.03515, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.16036, -0.037257, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.0083286, -0.1259, 0.0, 4.2108e+06),
            PointXYZRGBA::new(0.0007442, -0.15603, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.1741, -0.17381, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.18502, -0.02954, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.20707, -0.056403, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.23348, -0.07764, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.2244, -0.10653, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.23604, -0.10652, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.20734, -0.15641, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.23348, -0.13542, 0.0, 4.808e+06),
            PointXYZRGBA::new(0.0061083, -0.18729, 0.0, 4.2108e+06),
            PointXYZRGBA::new(-0.066235, -0.27472, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.17577, -0.20789, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10861, -0.27494, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.15584, -0.25716, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.0075775, -0.31546, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.050817, -0.29595, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.10306, -0.28653, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.1319, -0.2831, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.18716, -0.20571, 0.0, 4.808e+06),
            PointXYZRGBA::new(-0.18369, -0.23729, 0.0, 4.808e+06),
        ];

        let expected = PointCloud::new(expected_points, 213, 1);
        assert_eq!(cloud, expected);
    }
}
