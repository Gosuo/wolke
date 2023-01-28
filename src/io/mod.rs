use nom::{
    bytes::complete::{tag, take_while},
    sequence::preceded,
    IResult,
};
use std::str::FromStr;

pub struct PcdReader {}

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
pub enum PcdFields {
    XYZ,
    XYZ_RGB,
    XYZ_RGBA,
    XYZ_NXNYNZ,
}

impl PcdFields {
    #[inline]
    fn from_bytes(input: &[u8]) -> Result<Self, String> {
        match input {
            b"x y z" => Ok(PcdFields::XYZ),
            b"x y z rgb" => Ok(PcdFields::XYZ_RGB),
            b"x y z rgba" => Ok(PcdFields::XYZ_RGBA),
            b"x y z normal_x normal_y normal_z" => Ok(PcdFields::XYZ_NXNYNZ),
            unknown => Err(format!("Unknown fields type: {}", std::str::from_utf8(unknown).unwrap())) // TODO remove unwrap
        }
    }
}

fn parse_version(input: &[u8]) -> IResult<&[u8], Result<PcdVersion, String>> {
    let (input, version_bytes) = preceded(tag(b"VERSION "), take_while(|c| c != b'\n'))(input)?;
    let version = PcdVersion::from_bytes(version_bytes);
    Ok((input, version))
}

fn parse_fields(input: &[u8]) -> IResult<&[u8], Result<PcdFields, String>> {
    let (input, fields) = preceded(
        tag(b"FIELDS "),
        take_while(|c| c != b'\n'),
    )(input)?;

    let field = PcdFields::from_bytes(fields);
    Ok((input, field))
}

#[cfg(test)]
mod tests {
    use super::{parse_fields, parse_version, PcdFields, PcdVersion};

    #[test]
    fn test_parse_version_valid_input() {
        let input = b"VERSION .7";

        let (_, result) = parse_version(input).unwrap();
        assert_eq!(result, Ok(PcdVersion::V0_7));
    }

    #[test]
    fn test_parse_version_invalid_version() {
        let input = b"VERSION .11";

        let (_, result) = parse_version(input).unwrap();
        assert_eq!(
            result,
            Err(String::from("Unknown version byte string: .11"))
        );
    }

    #[test]
    fn test_parse_version_missing_version_number() {
        let input = b"VERSION ";

        let (_, result) = parse_version(input).unwrap();
        assert_eq!(result, Err(String::from("Unknown version byte string: ")));
    }

    #[test]
    fn test_parse_version_invalid_input() {
        let input = b"VERSI";

        let result = parse_version(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_fields_valid_input_xyz() {
        let input = b"FIELDS x y z";

        let (_, result) = parse_fields(input).unwrap();
        assert_eq!(result, Ok(PcdFields::XYZ));
    }

    #[test]
    fn test_parse_fields_valid_input_xyzrgb() {
        let input = b"FIELDS x y z rgb";

        let (_, result) = parse_fields(input).unwrap();
        assert_eq!(result, Ok(PcdFields::XYZ_RGB));
    }

    #[test]
    fn test_parse_fields_valid_input_xyzrgba() {
        let input = b"FIELDS x y z rgba";

        let (_, result) = parse_fields(input).unwrap();
        assert_eq!(result, Ok(PcdFields::XYZ_RGBA));
    }

    #[test]
    fn test_parse_fields_valid_input_xyznormals() {
        let input = b"FIELDS x y z normal_x normal_y normal_z";

        let (_, result) = parse_fields(input).unwrap();
        assert_eq!(result, Ok(PcdFields::XYZ_NXNYNZ));
    }

    #[test]
    fn test_parse_fields_missing_desc() {
        let input = b"FIELDS ";

        let (_, result) = parse_fields(input).unwrap();
        assert_eq!(result, Err(String::from("Unknown fields type: ")));
    }

    #[test]
    fn test_parse_fields_invalid_input() {
        let input = b"FIEL";

        let result = parse_fields(input);
        assert!(result.is_err());
    }
}

// fn parse_width_height(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
//     let (input, (_, width, _, height, _)) = tuple((
//         tag(b"WIDTH "),
//         map_res(
//             nom::bytes::complete::take_while(|c| c != b'\n'),
//             u32::from_str,
//         ),
//         tag(b"\nHEIGHT "),
//         map_res(
//             nom::bytes::complete::take_while(|c| c != b'\n'),
//             u32::from_str,
//         ),
//         tag(b"\n"),
//     ))(input)?;
//     Ok((input, (width, height)))
// }

// fn parse_point(input: &[u8]) -> IResult<&[u8], (f32, f32, f32)> {
//     let (input, (x, y, z)) = tuple((
//         map_res(
//             nom::bytes::complete::take_while(|c| c != b' '),
//             f32::from_str,
//         ),
//         map_res(
//             nom::bytes::complete::take_while(|c| c != b' '),
//             f32::from_str,
//         ),
//         map_res(
//             nom::bytes::complete::take_while(|c| c != b'\n'),
//             f32::from_str,
//         ),
//     ))(input)?;
//     Ok((input, (x, y, z)))
// }

// fn parse_points(input: &
