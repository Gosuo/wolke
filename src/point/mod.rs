pub trait Point {}

#[repr(align(16))]
#[derive(Debug, Default, Clone)]
pub struct PointXYZ {
    x: f32,
    y: f32,
    z: f32,
}

impl Point for PointXYZ {}

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
    header: PointCloudHeader,
    width: usize,
    height: usize,
    points: Vec<P>,
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
}
