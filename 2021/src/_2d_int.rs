use core::ops::{Add, Div, Mul, Sub};
use core::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Hash)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
}

pub enum ParsePointError<T: FromStr> {
    InnerType(<T as FromStr>::Err),
    PointFormat(&'static str),
}

impl<T: FromStr> FromStr for Point<T> {
    type Err = ParsePointError<T>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut part_iter = s
            .trim()
            .split(',')
            .map(|substr| T::from_str(substr.trim()).map_err(ParsePointError::InnerType));

        let x = part_iter
            .next()
            .ok_or(ParsePointError::PointFormat("no items in string"))??;
        let y = part_iter.next().ok_or(ParsePointError::PointFormat(
            "only 1 item in string; requires 2",
        ))??;

        match part_iter.next() {
            None => Ok(Self { x, y }),
            _ => Err(ParsePointError::PointFormat("more than 2 items in string")),
        }
    }
}

impl<T, U> Sub<Point<U>> for Point<T>
where
    T: Sub<U>,
{
    type Output = Vector<<T as Sub<U>>::Output>;

    fn sub(self, v: Point<U>) -> Self::Output {
        Self::Output {
            x: self.x - v.x,
            y: self.y - v.y,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Hash)]
pub struct Vector<T> {
    pub x: T,
    pub y: T,
}

impl<T, U> Add<Vector<U>> for Vector<T>
where
    T: Add<U>,
{
    type Output = Vector<<T as Add<U>>::Output>;

    fn add(self, other: Vector<U>) -> Self::Output {
        Self::Output {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<T, U> Mul<U> for Vector<T>
where
    T: Mul<U>,
    U: Copy,
{
    type Output = Vector<<T as Mul<U>>::Output>;

    fn mul(self, scalar: U) -> Self::Output {
        Self::Output {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
}

impl<T, U> Div<U> for Vector<T>
where
    T: Div<U>,
    U: Copy,
{
    type Output = Vector<<T as Div<U>>::Output>;

    fn div(self, scalar: U) -> Self::Output {
        Self::Output {
            x: self.x / scalar,
            y: self.y / scalar,
        }
    }
}

impl<T> Vector<T> {
    pub fn ortho(&self) -> Self
    where
        T: Copy + std::ops::Neg<Output = T>,
    {
        Self {
            x: -self.y,
            y: self.x,
        }
    }

    pub fn inner_product<U: Mul<T>>(
        self,
        other: Vector<U>,
    ) -> <<U as Mul<T>>::Output as Add>::Output
    where
        <U as Mul<T>>::Output: Add,
    {
        other.x * self.x + other.y * self.y
    }
}

impl<T, U> Add<Vector<U>> for Point<T>
where
    T: Add<U>,
{
    type Output = Point<<T as Add<U>>::Output>;

    fn add(self, v: Vector<U>) -> Self::Output {
        Self::Output {
            x: self.x + v.x,
            y: self.y + v.y,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Hash)]
pub struct LineSegment<T> {
    pub p0: Point<T>,
    pub p1: Point<T>,
}

pub enum ParseLineSegmentError<T: FromStr> {
    InnerType(ParsePointError<T>),
    LineSegmentFormat(&'static str),
}

impl<T: FromStr> FromStr for LineSegment<T> {
    type Err = ParseLineSegmentError<T>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut part_iter = s.trim().split("->").map(|substr| {
            Point::<T>::from_str(substr.trim()).map_err(ParseLineSegmentError::InnerType)
        });

        let p0 = part_iter
            .next()
            .ok_or(ParseLineSegmentError::LineSegmentFormat(
                "no items in string",
            ))??;
        let p1 = part_iter
            .next()
            .ok_or(ParseLineSegmentError::LineSegmentFormat(
                "only 1 item in string; requires 2",
            ))??;

        match part_iter.next() {
            None => Ok(Self { p0, p1 }),
            _ => Err(ParseLineSegmentError::LineSegmentFormat(
                "more than 2 items in string",
            )),
        }
    }
}

impl<T> LineSegment<T> {
    pub fn new(p0: (T, T), p1: (T, T)) -> Self {
        Self {
            p0: Point { x: p0.0, y: p0.1 },
            p1: Point { x: p1.0, y: p1.1 },
        }
    }

    pub fn is_horiz(&self) -> bool
    where
        T: std::cmp::PartialEq + std::cmp::Eq,
    {
        self.p0.x == self.p1.x
    }

    pub fn is_vert(&self) -> bool
    where
        T: std::cmp::PartialEq + std::cmp::Eq,
    {
        self.p0.y == self.p1.y
    }

    pub fn orientation(&self) -> Vector<<T as Sub>::Output>
    where
        T: Copy + Sub,
    {
        self.p1 - self.p0
    }

    /*
    pub fn intersects(&self, &other: Self) -> bool {
        let self_v0 = self.p0 - Default::default();
        let self_v1 = self.p1 - Default::default();
        let other_v0 = other.v0 - Default::default();
        let other_v1 = other.v1 - Default::default();

        let self_orth = self.orientation().ortho();
        let other_orth = other.orientation().ortho();

        assert_eq!(self_ortho)()
    }
    */
}
