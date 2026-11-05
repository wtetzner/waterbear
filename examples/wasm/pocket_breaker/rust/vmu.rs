use crate::waterbear_vmu;

pub const SCREEN_WIDTH: u8 = 48;
pub const SCREEN_HEIGHT: u8 = 32;
const BYTES_PER_ROW: u8 = SCREEN_WIDTH / 8;
const FRAMEBUFFER_BYTES: usize = SCREEN_HEIGHT as usize * BYTES_PER_ROW as usize;

#[derive(Clone, Copy)]
pub struct Buttons {
    pub left: bool,
    pub right: bool,
    pub a: bool,
}

#[inline(always)]
pub fn initialize() {
    const OCR_RC_DIVIDE_BY_6: u8 = 0x81;
    const MCR_GRAPHICS_DIRECT: u8 = 0x19;
    const PORT_3_PULL_UPS: u8 = 0xff;

    unsafe {
        waterbear_vmu::waterbear_vmu_sfr_ocr_write(OCR_RC_DIVIDE_BY_6);
        waterbear_vmu::waterbear_vmu_sfr_mcr_write(MCR_GRAPHICS_DIRECT);
        waterbear_vmu::waterbear_vmu_sfr_vccr_write(0x00);
        waterbear_vmu::waterbear_vmu_sfr_stad_write(0x00);
        waterbear_vmu::waterbear_vmu_sfr_p3_write(PORT_3_PULL_UPS);
    }
}

#[inline(always)]
pub fn read_buttons() -> Buttons {
    const BUTTON_LEFT: u8 = 1 << 2;
    const BUTTON_RIGHT: u8 = 1 << 3;
    const BUTTON_A: u8 = 1 << 4;

    let port = unsafe { waterbear_vmu::waterbear_vmu_sfr_p3_read() };
    Buttons {
        left: port & BUTTON_LEFT == 0,
        right: port & BUTTON_RIGHT == 0,
        a: port & BUTTON_A == 0,
    }
}

pub struct Screen {
    pixels: [u8; FRAMEBUFFER_BYTES],
    dirty_rows: [u8; SCREEN_HEIGHT as usize],
}

impl Screen {
    pub const fn new() -> Self {
        Self {
            pixels: [0; FRAMEBUFFER_BYTES],
            dirty_rows: [1; SCREEN_HEIGHT as usize],
        }
    }

    #[inline(never)]
    pub fn clear(&mut self) {
        let mut row = 0;
        while row < SCREEN_HEIGHT {
            let mut byte = 0;
            while byte < BYTES_PER_ROW {
                let index = usize::from(row * BYTES_PER_ROW + byte);
                if self.pixels[index] != 0 {
                    self.pixels[index] = 0;
                    self.dirty_rows[usize::from(row)] = 1;
                }
                byte += 1;
            }
            row += 1;
        }
    }

    #[inline(always)]
    pub fn fill_rect(&mut self, x: u8, y: u8, width: u8, height: u8) {
        self.update_rect(x, y, width, height, true);
    }

    #[inline(always)]
    pub fn clear_rect(&mut self, x: u8, y: u8, width: u8, height: u8) {
        self.update_rect(x, y, width, height, false);
    }

    #[inline(never)]
    pub fn present(&mut self) {
        const VCCR_LCD_ON: u8 = 0x80;

        let mut row = 0;
        while row < SCREEN_HEIGHT {
            if self.dirty_rows[usize::from(row)] != 0 {
                let bank_row = row & 0x0f;
                let bank = row / 16;
                let offset = bank_row / 2 * 0x10 + bank_row % 2 * BYTES_PER_ROW;
                let source = unsafe { self.pixels.as_ptr().add(usize::from(row * BYTES_PER_ROW)) };
                unsafe {
                    waterbear_vmu::waterbear_vmu_xram_copy_from_main(
                        bank,
                        offset,
                        source,
                        BYTES_PER_ROW,
                    );
                }
                self.dirty_rows[usize::from(row)] = 0;
            }
            row += 1;
        }

        unsafe { waterbear_vmu::waterbear_vmu_sfr_vccr_write(VCCR_LCD_ON) };
    }

    #[inline(never)]
    fn update_rect(&mut self, x: u8, y: u8, mut width: u8, mut height: u8, fill: bool) {
        if x >= SCREEN_WIDTH || y >= SCREEN_HEIGHT || width == 0 || height == 0 {
            return;
        }

        if width > SCREEN_WIDTH - x {
            width = SCREEN_WIDTH - x;
        }
        if height > SCREEN_HEIGHT - y {
            height = SCREEN_HEIGHT - y;
        }

        let right = x + width - 1;
        let first_byte = x / 8;
        let last_byte = right / 8;
        let mut row = y;
        while row < y + height {
            let mut byte = first_byte;
            while byte <= last_byte {
                let mut mask = 0xff_u8;
                if byte == first_byte {
                    mask >>= x & 7;
                }
                if byte == last_byte {
                    mask &= 0xff_u8 << (7 - (right & 7));
                }

                let index = usize::from(row * BYTES_PER_ROW + byte);
                let old_value = self.pixels[index];
                let new_value = if fill {
                    old_value | mask
                } else {
                    old_value & !mask
                };
                if new_value != old_value {
                    self.pixels[index] = new_value;
                    self.dirty_rows[usize::from(row)] = 1;
                }
                byte += 1;
            }
            row += 1;
        }
    }
}
