use std::ops::{Add, AddAssign};

pub trait Point {}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum PointType {
    XYZ(PointXYZ),
    XYZ_RGBA(PointXYZRGBA),
}

#[repr(align(16))]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct PointXYZ {
    x: f32,
    y: f32,
    z: f32,
}
impl PointXYZ {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
}

impl Point for PointXYZ {}

// TODO!: This is terrible, fix it
impl From<&[f32]> for PointXYZ {
    fn from(value: &[f32]) -> Self {
        assert!(
            value.len() >= 3,
            "Creating a PointXYZ with less than 3 values is impossible"
        );
        Self {
            x: value[0],
            y: value[1],
            z: value[2],
        }
    }
}

#[repr(align(4))]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

impl Color {
    pub fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    pub fn r(&self) -> u8 {
        self.r
    }

    pub fn g(&self) -> u8 {
        self.g
    }

    pub fn b(&self) -> u8 {
        self.b
    }

    pub fn a(&self) -> u8 {
        self.a
    }
}

impl From<u32> for Color {
    fn from(value: u32) -> Self {
        Self {
            r: ((value >> 24) & 0xFF) as u8,
            g: ((value >> 16) & 0xFF) as u8,
            b: ((value >> 8) & 0xFF) as u8,
            a: (value & 0xFF) as u8,
        }
    }
}

impl From<f32> for Color {
    fn from(value: f32) -> Self {
        assert!(!value.is_nan(), "Trying to convert a NaN-float to a color");
        assert!(
            !value.is_subnormal(),
            "Trying to convert a subnormal-float to a color"
        );

        // Raw transmute instead of casting, we want to preserve the bits, not the numerical value
        // Check the documentation for f32::to_bits for more information
        let value = value.to_bits();
        Self::from(value)
    }
}

#[repr(align(16))]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct PointXYZRGBA {
    x: f32,
    y: f32,
    z: f32,
    color: Color,
}

impl Point for PointXYZRGBA {}

impl PointXYZRGBA {
    pub fn new(x: f32, y: f32, z: f32, rgba: f32) -> Self {
        Self {
            x,
            y,
            z,
            color: Color::from(rgba),
        }
    }

    pub fn x(&self) -> f32 {
        self.x
    }

    pub fn y(&self) -> f32 {
        self.y
    }

    pub fn z(&self) -> f32 {
        self.z
    }

    pub fn r(&self) -> u8 {
        self.color.r
    }

    pub fn g(&self) -> u8 {
        self.color.g
    }

    pub fn b(&self) -> u8 {
        self.color.b
    }

    pub fn a(&self) -> u8 {
        self.color.a
    }

    pub fn color(&self) -> &Color {
        &self.color
    }

    pub fn mut_x(&mut self) -> &mut f32 {
        &mut self.x
    }

    pub fn mut_y(&mut self) -> &mut f32 {
        &mut self.y
    }

    pub fn mut_z(&mut self) -> &mut f32 {
        &mut self.z
    }

    pub fn color_mut(&mut self) -> &mut Color {
        &mut self.color
    }
}

impl Add<&PointXYZRGBA> for PointXYZRGBA {
    type Output = Self;

    fn add(self, rhs: &PointXYZRGBA) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
            color: self.color,
        }
    }
}

impl AddAssign<&PointXYZRGBA> for PointXYZRGBA {
    fn add_assign(&mut self, rhs: &PointXYZRGBA) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

impl From<&[f32; 4]> for PointXYZRGBA {
    fn from(value: &[f32; 4]) -> Self {
        let color = Color::from(value[3]);
        Self {
            x: value[0],
            y: value[1],
            z: value[2],
            color,
        }
    }
}

// impl TryFrom<&[f32]> for PointXYZRGBA {

// }

impl TryFrom<&[f32]> for PointXYZRGBA {
    type Error = String;

    fn try_from(value: &[f32]) -> Result<Self, Self::Error> {
        if value.len() < 4 {
            Err(format!(
                "Creating a PointXYZRGBA from less than 4 floats is not possible"
            ))
        } else {
            let input = [value[0], value[1], value[2], value[3]];
            Ok(Self::from(&input))
        }
    }
}

#[repr(align(16))]
#[derive(Debug, Default, Clone, PartialEq)]
struct Quaternion {
    vector: PointXYZ,
    scalar: f32,
}

// TODO!: This is terrible, fix it
impl From<&[f32]> for Quaternion {
    fn from(value: &[f32]) -> Self {
        assert!(value.len() >= 4);
        let vector = PointXYZ::from(&value[..3]);

        Self {
            vector,
            scalar: value[3],
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ViewPoint {
    point: PointXYZ,
    orientation: Quaternion,
}

// TODO!: This is terrible, fix it
impl From<&[f32]> for ViewPoint {
    fn from(value: &[f32]) -> Self {
        assert!(value.len() >= 7);
        let point = PointXYZ::from(&value[..3]);
        let orientation = Quaternion::from(&value[3..]);

        Self { point, orientation }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum PointCloudType {
    XYZ(PointCloud<PointXYZ>),
    XYZ_RGBA(PointCloud<PointXYZRGBA>),
}

#[derive(Debug, Default, Clone)]
pub struct PointCloudHeader {
    sequence: u32,
    timestamp: u64,
    frame_id: String,
}

#[derive(Debug, Default)]
pub struct PointCloud<P>
where
    P: Point,
{
    header: Option<PointCloudHeader>,
    width: usize,
    height: usize,
    points: Vec<P>,
}

impl<P: Point> PointCloud<P> {
    pub fn new(points: Vec<P>, width: usize, height: usize) -> Self {
        Self {
            header: None,
            width,
            height,
            points,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pointxyz_size() {
        assert_eq!(std::mem::size_of::<PointXYZ>(), 16)
    }

    #[test]
    fn pointxyz_default() {
        let point = PointXYZ::default();
        assert_eq!(point.x, 0_f32);
        assert_eq!(point.y, 0_f32);
        assert_eq!(point.z, 0_f32);
    }

    #[test]
    fn pointxyz_from_slice() {
        let point = PointXYZ::from(&[1.0, 1.0, 1.0][..]);
        assert_eq!(point.x, 1_f32);
        assert_eq!(point.y, 1_f32);
        assert_eq!(point.z, 1_f32);
    }

    #[test]
    fn quaternion_from_slice() {
        let quaternion = Quaternion::from(&[1.0, 1.0, 1.0, 1.0][..]);
        assert_eq!(quaternion.vector, PointXYZ::from(&[1.0, 1.0, 1.0][..]));
        assert_eq!(quaternion.scalar, 1_f32);
    }

    #[test]
    fn viewpoint_from_slice() {
        let viewpoint = ViewPoint::from(&[0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0][..]);
        assert_eq!(viewpoint.point, PointXYZ::from(&[0.0, 0.0, 0.0][..]));
        assert_eq!(
            viewpoint.orientation,
            Quaternion::from(&[1.0, 0.0, 0.0, 0.0][..])
        );
    }
}
