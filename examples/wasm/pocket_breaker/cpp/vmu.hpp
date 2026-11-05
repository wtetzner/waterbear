#pragma once

#include "waterbear_vmu.hpp"

namespace vmu {

inline constexpr uint8_t screen_width = 48;
inline constexpr uint8_t screen_height = 32;
inline constexpr uint8_t bytes_per_row = screen_width / 8;

struct Buttons {
    bool left;
    bool right;
    bool a;
};

inline void initialize() {
    constexpr uint8_t ocr_rc_divide_by_6 = 0x81;
    constexpr uint8_t mcr_graphics_direct = 0x19;
    constexpr uint8_t port_3_pull_ups = 0xff;

    waterbear_vmu_sfr_ocr_write(ocr_rc_divide_by_6);
    waterbear_vmu_sfr_mcr_write(mcr_graphics_direct);
    waterbear_vmu_sfr_vccr_write(0x00);
    waterbear_vmu_sfr_stad_write(0x00);
    waterbear_vmu_sfr_p3_write(port_3_pull_ups);
}

inline Buttons read_buttons() {
    constexpr uint8_t button_left = 1 << 2;
    constexpr uint8_t button_right = 1 << 3;
    constexpr uint8_t button_a = 1 << 4;

    const auto port = waterbear_vmu_sfr_p3_read();
    return {
        .left = (port & button_left) == 0,
        .right = (port & button_right) == 0,
        .a = (port & button_a) == 0,
    };
}

class Screen {
public:
    __attribute__((noinline)) Screen() {
        uint8_t row = 0;
        while (row < screen_height) {
            uint8_t byte = 0;
            while (byte < bytes_per_row) {
                pixels_[row * bytes_per_row + byte] = 0;
                ++byte;
            }
            dirty_rows_[row] = 1;
            ++row;
        }
    }

    __attribute__((noinline)) void clear() {
        uint8_t row = 0;
        while (row < screen_height) {
            uint8_t byte = 0;
            while (byte < bytes_per_row) {
                const auto index = static_cast<uint8_t>(row * bytes_per_row + byte);
                if (pixels_[index] != 0) {
                    pixels_[index] = 0;
                    dirty_rows_[row] = 1;
                }
                ++byte;
            }
            ++row;
        }
    }

    void fill_rect(
        uint8_t x,
        uint8_t y,
        uint8_t width,
        uint8_t height
    ) {
        update_rect(x, y, width, height, true);
    }

    void clear_rect(
        uint8_t x,
        uint8_t y,
        uint8_t width,
        uint8_t height
    ) {
        update_rect(x, y, width, height, false);
    }

    __attribute__((noinline)) void present() {
        constexpr uint8_t vccr_lcd_on = 0x80;

        uint8_t row = 0;
        while (row < screen_height) {
            if (dirty_rows_[row]) {
                const auto bank_row = static_cast<uint8_t>(row & 0x0f);
                const auto bank = static_cast<uint8_t>(row / 16);
                const auto offset = static_cast<uint8_t>(
                    bank_row / 2 * 0x10 + bank_row % 2 * bytes_per_row
                );
                waterbear_vmu_xram_copy_from_main(
                    bank,
                    offset,
                    &pixels_[row * bytes_per_row],
                    bytes_per_row
                );
                dirty_rows_[row] = 0;
            }
            ++row;
        }

        waterbear_vmu_sfr_vccr_write(vccr_lcd_on);
    }

private:
    void update_rect(
        uint8_t x,
        uint8_t y,
        uint8_t width,
        uint8_t height,
        bool fill
    ) {
        if (x >= screen_width || y >= screen_height || width == 0 || height == 0) {
            return;
        }

        if (width > screen_width - x) {
            width = static_cast<uint8_t>(screen_width - x);
        }
        if (height > screen_height - y) {
            height = static_cast<uint8_t>(screen_height - y);
        }

        const auto right = static_cast<uint8_t>(x + width - 1);
        const auto first_byte = static_cast<uint8_t>(x / 8);
        const auto last_byte = static_cast<uint8_t>(right / 8);
        uint8_t row = y;
        while (row < y + height) {
            uint8_t byte = first_byte;
            while (byte <= last_byte) {
                uint8_t mask = 0xff;
                if (byte == first_byte) {
                    mask = static_cast<uint8_t>(mask >> (x & 7));
                }
                if (byte == last_byte) {
                    mask = static_cast<uint8_t>(
                        mask & static_cast<uint8_t>(0xff << (7 - (right & 7)))
                    );
                }

                const auto index = static_cast<uint8_t>(row * bytes_per_row + byte);
                const auto old_value = pixels_[index];
                const auto new_value = fill
                    ? static_cast<uint8_t>(old_value | mask)
                    : static_cast<uint8_t>(old_value & static_cast<uint8_t>(~mask));
                if (new_value != old_value) {
                    pixels_[index] = new_value;
                    dirty_rows_[row] = 1;
                }
                ++byte;
            }
            ++row;
        }
    }

    uint8_t pixels_[screen_height * bytes_per_row];
    uint8_t dirty_rows_[screen_height];
};

} // namespace vmu
