
use image;
use image::{DynamicImage, GenericImageView, Rgba};
use image::png::{PngDecoder};
use image::gif::{GifDecoder};
use image::{AnimationDecoder};
use std::fs::File;
use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use enum_iterator::IntoEnumIterator;

#[derive(Debug)]
pub enum IconError {
    InvalidIconSize(String, usize, usize),
    InvalidPaletteSize(String, usize, usize),
    FileLoadFailure(String, std::io::Error),
    ImageParseError(String, image::ImageError)
}

#[derive(Debug)]
pub enum ImageLoadError {
    FileLoadFailure(String, std::io::Error),
    ImageParseError(String, image::ImageError)
}

impl std::convert::From<ImageLoadError> for IconError {
    fn from(error: ImageLoadError) -> IconError {
        match error {
            ImageLoadError::FileLoadFailure(s, e) => IconError::FileLoadFailure(s, e),
            ImageLoadError::ImageParseError(s, e) => IconError::ImageParseError(s, e)
        }
    }
}

#[derive(Debug,Clone,Copy,Hash,Eq,PartialEq,Deserialize,Serialize,IntoEnumIterator)]
pub enum ImageFormat {
    Json,
    JsonPretty,
    Asm1Bit,
    Asm1BitMasked
}

impl ImageFormat {
    pub fn name(&self) -> &str {
        use ImageFormat::*;
        match self {
            Json => "json",
            JsonPretty => "json-pretty",
            Asm1Bit => "1bit-asm",
            Asm1BitMasked => "1bit-asm-masked"
        }
    }

    pub fn from(name: &str) -> Option<ImageFormat> {
        match name {
            "json" => Some(ImageFormat::Json),
            "json-pretty" => Some(ImageFormat::JsonPretty),
            "1bit-asm" => Some(ImageFormat::Asm1Bit),
            "1bit-asm-masked" => Some(ImageFormat::Asm1BitMasked),
            _ => None
        }
    }
}

#[derive(Debug,Clone,Copy,Hash,Eq,PartialEq,Deserialize,Serialize)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8
}

impl std::default::Default for Color {
    fn default() -> Self {
        Color {
            red: 0,
            green: 0,
            blue: 0,
            alpha: 0
        }
    }
}

impl Color {
    pub fn scale_channels(&self, max: u8) -> Color {
        let maxf = max as f64;
        Color {
            red: ((maxf * (self.red as f64)) / 255.0f64).round() as u8,
            green: ((maxf * (self.green as f64)) / 255.0f64).round() as u8,
            blue: ((maxf * (self.blue as f64)) / 255.0f64).round() as u8,
            alpha: ((maxf * (self.alpha as f64)) / 255.0f64).round() as u8
        }
    }

    pub fn to_palette_color(&self) -> i32 {
        (((self.alpha as i32) & 0xF) << 12)
            | (((self.red as i32) & 0xF) << 8)
            | (((self.green as i32) & 0xF) << 4)
            | ((self.blue as i32) & 0xF)
    }

    pub fn grayscale(&self) -> Color {
        let gray = ((0.3f64 * (self.red as f64))
                    + (0.59f64 * (self.green as f64))
                    + (0.11f64 * (self.blue as f64))).round() as u8;
        Color {
            red:  gray,
            green: gray,
            blue: gray,
            alpha: self.alpha
        }
    }

    // If transparent, return 1, else 0.
    pub fn transparency(&self) -> u8 {
        if self.alpha <= 127 { 1u8 } else { 0u8 }
    }
}

#[derive(Debug,Clone,Deserialize,Serialize)]
pub struct Frame {
    pub delay: (u32, u32),
    pub rows: Vec<Vec<Color>>
}

impl Frame {
    pub fn size(&self) -> (usize, usize) {
        let height = self.rows.len();
        let width = if height > 0 {
            self.rows[0].len()
        } else {
            0usize
        };

        (width, height)
    }

    pub fn to_1bit_asm(&self, masked: bool) -> crate::ast::Statements {
        use crate::expression::{Expr};
        use crate::ast::{Statement, Statements, ByteValue};

        let mut stmts = vec![];

        let (width, height) = self.size();

        stmts.push(Statement::comment(&format!("Width: {}, Height: {}", width, height)));
        stmts.push(Statement::byte(vec![
            ByteValue::Expr(Expr::decimal(width as i32)),
            ByteValue::Expr(Expr::decimal(height as i32))]));

        if masked {
            for row in &self.rows {
                let mut bytes: Vec<u8> = vec![];
                let mut idx: usize = 0;
                for color in row {
                    let bit = color.transparency();
                    if idx % 8 == 0 {
                        bytes.push(0b01111111 | bit << 7 as u8);
                    } else {
                        let last_idx = bytes.len() - 1;
                        let byte = bytes[last_idx];
                        let bit_pos = 7 - (idx % 8);
                        bytes[last_idx] = (byte & !(1 << bit_pos)) | (bit << bit_pos);
                    }
                    idx = idx + 1;
                }
                let byte_exprs: Vec<ByteValue> = bytes.iter()
                    .map(|b| ByteValue::Expr(Expr::binary(*b as i32)))
                    .collect();
                stmts.push(Statement::byte(byte_exprs));
            }
        }

        for row in &self.rows {
            let mut bytes: Vec<u8> = vec![];
            let mut idx: usize = 0;
            for color in row {
                let gray = color.grayscale();
                let bit = if gray.red <= 127 { 1 } else { 0 };
                if idx % 8 == 0 {
                    bytes.push(bit << 7 as u8);
                } else {
                    let last_idx = bytes.len() - 1;
                    let byte = bytes[last_idx];
                    bytes[last_idx] = byte | (bit << (7 - (idx % 8)));
                }
                idx = idx + 1;
            }
            let byte_exprs: Vec<ByteValue> = bytes.iter()
                .map(|b| ByteValue::Expr(Expr::binary(*b as i32)))
                .collect();
            stmts.push(Statement::byte(byte_exprs));
        }

        Statements::new(HashMap::new(), stmts)
    }

    pub fn is_size(&self, width: usize, height: usize) -> bool {
        if height != self.rows.len() {
            return false;
        }
        for row in &self.rows {
            if row.len() != width {
                return false;
            }
        }
        return true;
    }
}

#[derive(Debug,Clone,Deserialize,Serialize)]
pub struct Image {
    pub frames: Vec<Frame>
}

impl Image {
    pub fn as_still(&self) -> Image {
        Image {
            frames: vec![self.frames[0].clone()]
        }
    }

    pub fn scale_channels(&self, max: u8) -> Image {
        let mut frames = vec![];
        for frame in &self.frames {
            let mut rows = vec![];
            for row in &frame.rows {
                let mut new_row: Vec<Color> = vec![];
                for color in row {
                    new_row.push(color.scale_channels(max));
                }
                rows.push(new_row);
            }
            frames.push(Frame { delay: frame.delay, rows: rows })
        }
        Image { frames }
    }

    pub fn get_palette(&self) -> HashMap<Color,usize> {
        let mut index: usize = 0;
        let mut palette = HashMap::new();
        for frame in &self.frames {
            for row in &frame.rows {
                for color in row {
                    if !palette.contains_key(color) {
                        palette.insert(*color, index);
                        index = index + 1;
                    }
                }
            }
        }
        palette
    }

    pub fn to_frame_count_and_speed(&self, name: &str, speed: u16) -> crate::ast::Statements {
        use crate::expression::{Expr};
        use crate::ast::{Statements, Statement};
        let mut stmts = vec![];
        let frames = Expr::num(self.frames.len() as i32);
        let anim_speed = Expr::num(speed as i32);
        stmts.push(Statement::comment(&format!("\n\"{}\" Frames: {}, Speed: {}", name, self.frames.len(), speed)));
        stmts.push(Statement::word(vec![frames, anim_speed]));
        Statements::new(HashMap::new(), stmts)
    }

    pub fn to_1bit_asm(&self, masked: bool) -> crate::ast::Statements {
        use crate::ast::{Statements};

        let mut stmts = vec![];

        for frame in &self.frames {
            for stmt in frame.to_1bit_asm(masked).iter() {
                stmts.push(stmt.clone());
            }
        }

        Statements::new(HashMap::new(), stmts)
    }

    pub fn to_icon(&self, palette_map: &HashMap<Color,usize>, name: &str) -> crate::ast::Statements {
        use crate::expression::{Expr};
        use crate::ast::{Statements, Statement, ByteValue};

        let mut stmts = vec![];

        let mut palette = if palette_map.len() <= 16 {
            vec![Color::default(); 16]
        } else if palette_map.len() <= 256 {
            vec![Color::default(); 256]
        } else {
            vec![]
        };

        if palette.len() > 0 {
            // Build palette
            for key in palette_map.keys() {
                let idx = palette_map[key];
                palette[idx] = key.clone();
            }

            // Push palette
            stmts.push(Statement::comment(&format!("\nPalette for \"{}\"", name)));
            for y in 0..(palette.len() / 8) {
                let mut words = vec![];
                for x in 0..8 {
                    let idx = (y * 8) + x;
                    let color = palette[idx];
                    let palette_color = Expr::num(color.to_palette_color());
                    words.push(palette_color);
                }
                stmts.push(Statement::word(words));
            }

            // Push pixel data
            stmts.push(Statement::comment(&format!("\nPixel data for \"{}\"", name)));
            if palette.len() == 16 {
                let mut frame_idx = 1;
                for frame in &self.frames {
                    if self.frames.len() > 1 {
                        if frame_idx == 1 {
                            stmts.push(Statement::comment(&format!("Frame {}", frame_idx)));
                        } else {
                            stmts.push(Statement::comment(&format!("\nFrame {}", frame_idx)));
                        }
                    }
                    for row in &frame.rows {
                        let mut bytes = vec![];
                        for color_idx in (0..row.len()).step_by(2) {
                            let color1 = &row[color_idx];
                            let color2 = &row[color_idx + 1];
                            let index1 = palette_map[color1];
                            let index2 = palette_map[color2];
                            let value = ((index1 & 0xF) << 4) | (index2 & 0xF);
                            bytes.push(ByteValue::Expr(Expr::num(value as i32)));
                        }
                        stmts.push(Statement::byte(bytes));
                    }
                    frame_idx = frame_idx + 1;
                }
            } else {
                for frame in &self.frames {
                    for row in &frame.rows {
                        let mut bytes = vec![];
                        for color in row {
                            let index = palette_map[color] & 0xFF;
                            bytes.push(ByteValue::Expr(Expr::num(index as i32)));
                        }
                        stmts.push(Statement::byte(bytes));
                    }
                }
            }
        } else {
            for frame in &self.frames {
                for row in &frame.rows {
                    let mut words = vec![];
                    for color in row {
                        let value = color.to_palette_color();
                        words.push(Expr::num((value & 0xFFFF) as i32));
                    }
                    stmts.push(Statement::word(words));
                }
            }
        }

        Statements::new(HashMap::new(), stmts)
    }

    pub fn is_size(&self, width: usize, height: usize) -> bool {
        for frame in &self.frames {
            if !frame.is_size(width, height) {
                return false;
            }
        }
        return true;
    }
}

pub fn load_image(path: &str) -> Result<Image, ImageLoadError> {
    let lower_path = path.to_lowercase();
    if lower_path.ends_with(".png") {
        load_png(path)
    } else if lower_path.ends_with(".gif") {
        load_gif(path)
    } else {
        let image = image::open(path)
            .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
        Ok(to_image(&image))
    }
}

fn load_gif(path: &str) -> Result<Image, ImageLoadError> {
    let file_in = File::open(path)
        .map_err(|e| ImageLoadError::FileLoadFailure(path.to_string(), e))?;
    let decoder = GifDecoder::new(file_in)
        .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
    let frames = decoder.into_frames().collect_frames()
        .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
    let mut out_frames = vec![];
    for frame in frames {
        out_frames.push(to_frame(
            frame.delay().numer_denom_ms(),
            &frame.into_buffer()
        ));
    }
    Ok(Image { frames: out_frames })
}

fn load_png(path: &str) -> Result<Image, ImageLoadError> {
    let file_in = File::open(path)
        .map_err(|e| ImageLoadError::FileLoadFailure(path.to_string(), e))?;
    let decoder = PngDecoder::new(file_in)
        .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
    if decoder.is_apng() {
        let frames = decoder.apng().into_frames().collect_frames()
            .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
        let mut out_frames = vec![];
        for frame in frames {
            out_frames.push(to_frame(
                frame.delay().numer_denom_ms(),
                &frame.into_buffer()
            ));
        }
        Ok(Image { frames: out_frames })
    } else {
        let image = DynamicImage::from_decoder(decoder)
            .map_err(|e| ImageLoadError::ImageParseError(path.to_string(), e))?;
        Ok(to_image(&image))
    }
}

fn to_frame<T: GenericImageView<Pixel = Rgba<u8>>>(
    delay: (u32, u32),
    frame: &T
) -> Frame {
    let (width, height) = frame.dimensions();

    let mut rows = vec![];
    for y in 0..height {
        let mut row = vec![];
        for x in 0..width {
            let Rgba(ref parts) = frame.get_pixel(x, y);
            row.push(Color {
                red: parts[0],
                green: parts[1],
                blue: parts[2],
                alpha: parts[3]
            });
        }
        rows.push(row);
    }
    Frame { delay: delay, rows: rows }
}

fn to_image<T: GenericImageView<Pixel = Rgba<u8>>>(image: &T) -> Image {
    Image {
        frames: vec![
            to_frame((0, 0), image)
        ]
    }
}

fn eyecatch_type(palette: &Option<HashMap<Color, usize>>) -> u16 {
    if palette.is_none() {
        return 0;
    }
    let p = palette.as_ref().unwrap();
    let size = p.len();
    if size <= 16 {
        return 3;
    }
    if size <= 256 {
        return 2;
    }
    return 1;
}

pub fn to_icon(icon_path: &str, speed: Option<u16>, eyecatch_file: Option<&str>) -> Result<crate::ast::Statements, IconError> {
    use crate::expression::{Expr};
    use crate::ast::{Statement, ByteValue};

    let image = load_image(icon_path).unwrap().scale_channels(15);
    let palette = image.get_palette();
    if palette.len() > 16 {
        return Err(IconError::InvalidPaletteSize(icon_path.to_string(), palette.len(), 16));
    }
    if !image.is_size(32, 32) {
        return Err(IconError::InvalidIconSize(icon_path.to_string(), 32, 32));
    }

    let eyecatch = {
        if eyecatch_file.is_some() {
            Some(load_image(eyecatch_file.unwrap())?
                 .as_still()
                 .scale_channels(15))
        } else {
            None
        }
    };

    let eyecatch_palette = eyecatch.as_ref().map(|ref img| img.get_palette());
    
    let eyecatch_type = eyecatch_type(&eyecatch_palette);

    let mut stmts = image.to_frame_count_and_speed(&icon_path, speed.unwrap_or(10));
    let eyecatch_comment = match eyecatch_type {
        0 => "Eyecatch type is 0: there is no eyecatch image.",
        1 => "Eyecatch type is 1: the eyecatch is stored as a 16-bit true color image.",
        2 => "Eyecatch type is 2: the eyecatch image has a 256-color palette.",
        3 => "Eyecatch type is 3: the eyecatch image has a 16-color palette.",
        _ => panic!(format!("Unexpected eyecatch type: {}", eyecatch_type))
    };

    stmts.push(Statement::comment(&format!("\n{}", eyecatch_comment)));
    stmts.push(Statement::word(vec![Expr::num(eyecatch_type as i32)]));

    stmts.push(Statement::comment("\nPlaceholder for CRC checksum."));
    stmts.push(Statement::word(vec![Expr::num(0 as i32)]));

    stmts.push(Statement::comment("\nPlaceholder for file data size."));
    stmts.push(Statement::word(vec![Expr::num(0), Expr::num(0)]));

    stmts.push(Statement::comment("\nReserved bytes."));
    stmts.push(Statement::byte(vec![ByteValue::Expr(Expr::num(0)); 10]));
    stmts.push(Statement::byte(vec![ByteValue::Expr(Expr::num(0)); 10]));

    stmts.append(&image.to_icon(&palette, &icon_path).as_slice());

    if eyecatch.is_some() && eyecatch_palette.is_some() {
        let eyecatch_path = eyecatch_file.unwrap();
        if !eyecatch.as_ref().unwrap().is_size(72, 56) {
            return Err(IconError::InvalidIconSize(eyecatch_path.to_string(), 72, 56));
        }
        stmts.append(&eyecatch.unwrap().to_icon(&eyecatch_palette.unwrap(), eyecatch_path).as_slice());
    }
    Ok(stmts)
}
