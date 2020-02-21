/// A color in the sRGB color space.
#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(missing_docs)]
pub struct Color {
    /// Red component, 0.0 - 1.0
    pub r: f32,
    /// Green component, 0.0 - 1.0
    pub g: f32,
    /// Blue component, 0.0 - 1.0
    pub b: f32,
    /// Transparency, 0.0 - 1.0
    pub a: f32,
}


impl Color {
    /// The black color.
    pub const BLACK: Color = Color {
        r: 0.0,
        g: 0.0,
        b: 0.0,
        a: 1.0,
    };

    /// The white color.
    pub const WHITE: Color = Color {
        r: 1.0,
        g: 1.0,
        b: 1.0,
        a: 1.0,
    };

    /// A color with no opacity.
    pub const TRANSPARENT: Color = Color {
        r: 0.0,
        g: 0.0,
        b: 0.0,
        a: 0.0,
    };

    /// Calmps a float value to the range [0.0, 1.0]
    pub fn clamp(v: f32) -> f32 {
        v.max(0.0f32).min(1.0f32)
    }

    /// Ensures RGBA values on the range [0.0, 1.0]
    pub fn check_rgba(r: f32, g: f32, b: f32, a:f32) -> Color {
        Color {
            r: Color::clamp(r),
            g: Color::clamp(g),
            b: Color::clamp(b),
            a: Color::clamp(a),
        }
    }

    /// Creates a [`Color`] from its RGB components.
    ///
    /// [`Color`]: struct.Color.html
    pub fn from_rgb(r: f32, g: f32, b: f32) -> Color {
        Color::check_rgba(r, g, b, 1.0)
    }

    /// Creates a [`Color`] from its RGB8 components.
    ///
    /// [`Color`]: struct.Color.html
    pub fn from_rgb8(r: u8, g: u8, b: u8) -> Color {
        Color::from_rgba8(r, g, b, 1.0)
    }

    /// Creates a [`Color`] from its RGB8 components and an alpha value.
    ///
    /// [`Color`]: struct.Color.html
    pub fn from_rgba8(r: u8, g: u8, b: u8, a: f32) -> Color {
        Color {
            r: f32::from(r) / 255.0,
            g: f32::from(g) / 255.0,
            b: f32::from(b) / 255.0,
            a: Color::clamp(a),
        }
    }

    /// Converts the [`Color`] into its linear values.
    ///
    /// [`Color`]: struct.Color.html
    pub fn into_linear(self) -> [f32; 4] {
        // As described in:
        // https://en.wikipedia.org/wiki/SRGB#The_reverse_transformation
        fn linear_component(u: f32) -> f32 {
            if u < 0.04045 {
                u / 12.92
            } else {
                ((u + 0.055) / 1.055).powf(2.4)
            }
        }

        [
            linear_component(self.r),
            linear_component(self.g),
            linear_component(self.b),
            self.a,
        ]
    }
}

impl From<[f32; 3]> for Color {
    fn from([r, g, b]: [f32; 3]) -> Self {
        Color { r, g, b, a: 1.0 }
    }
}

impl From<[f32; 4]> for Color {
    fn from([r, g, b, a]: [f32; 4]) -> Self {
        Color { r, g, b, a }
    }
}
