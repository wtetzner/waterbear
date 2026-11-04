//! Audited VMU limits shared by allocation, validation, and code generation.

pub const APPLICATION_RAM_BYTES: usize = 0x100;
pub const WORK_RAM_BYTES: usize = 0x200;

// 00h-0fh contain the four banks of indirect registers. The assembly backend
// uses bank zero's R2 and R3. 10h-1fh are backend scratch bytes.
pub const FIRST_ALLOCATABLE_APPLICATION_BYTE: usize = 0x20;

pub const VMS_HEADER_ADDRESS: usize = 0x0200;

pub const XRAM_BANK_LAST_OFFSET: u8 = 0x7b;
pub const XRAM_ICON_BANK_LAST_OFFSET: u8 = 0x05;

/// Bitmap XRAM banks occupy 180h through 1fbh, but the last four addresses in
/// each sixteen-address row are not implemented.
pub const fn xram_offset_is_addressable(offset: u8) -> bool {
    offset <= XRAM_BANK_LAST_OFFSET && offset & 0x0f < 0x0c
}

/// The icon bank implements only 180h through 185h.
pub const fn icon_xram_offset_is_addressable(offset: u8) -> bool {
    offset <= XRAM_ICON_BANK_LAST_OFFSET
}
