#ifndef POCKET_BREAKER_VMU_H
#define POCKET_BREAKER_VMU_H

#include "waterbear_vmu.h"

enum {
    VMU_SCREEN_WIDTH = 48,
    VMU_SCREEN_HEIGHT = 32,
    VMU_SCREEN_BYTES_PER_ROW = VMU_SCREEN_WIDTH / 8,
};

typedef struct {
    uint8_t pixels[VMU_SCREEN_HEIGHT * VMU_SCREEN_BYTES_PER_ROW];
    uint8_t dirty_rows[VMU_SCREEN_HEIGHT];
} VmuScreen;

typedef struct {
    uint8_t left;
    uint8_t right;
    uint8_t a;
} VmuButtons;

static inline void vmu_initialize(void) {
    enum {
        OCR_RC_DIVIDE_BY_6 = 0x81,
        MCR_GRAPHICS_DIRECT = 0x19,
        PORT_3_PULL_UPS = 0xff,
    };

    waterbear_vmu_sfr_ocr_write(OCR_RC_DIVIDE_BY_6);
    waterbear_vmu_sfr_mcr_write(MCR_GRAPHICS_DIRECT);
    waterbear_vmu_sfr_vccr_write(0x00);
    waterbear_vmu_sfr_stad_write(0x00);
    waterbear_vmu_sfr_p3_write(PORT_3_PULL_UPS);
}

static inline VmuButtons vmu_read_buttons(void) {
    enum {
        BUTTON_LEFT = 1 << 2,
        BUTTON_RIGHT = 1 << 3,
        BUTTON_A = 1 << 4,
    };

    const uint8_t port = waterbear_vmu_sfr_p3_read();
    const VmuButtons buttons = {
        .left = (port & BUTTON_LEFT) == 0,
        .right = (port & BUTTON_RIGHT) == 0,
        .a = (port & BUTTON_A) == 0,
    };
    return buttons;
}

static __attribute__((noinline)) void vmu_screen_initialize(VmuScreen *screen) {
    uint8_t row = 0;
    while (row < VMU_SCREEN_HEIGHT) {
        uint8_t byte = 0;
        while (byte < VMU_SCREEN_BYTES_PER_ROW) {
            screen->pixels[row * VMU_SCREEN_BYTES_PER_ROW + byte] = 0;
            ++byte;
        }
        screen->dirty_rows[row] = 1;
        ++row;
    }
}

static __attribute__((noinline)) void vmu_screen_clear(VmuScreen *screen) {
    uint8_t row = 0;
    while (row < VMU_SCREEN_HEIGHT) {
        uint8_t byte = 0;
        while (byte < VMU_SCREEN_BYTES_PER_ROW) {
            const uint8_t index = (uint8_t)(row * VMU_SCREEN_BYTES_PER_ROW + byte);
            if (screen->pixels[index] != 0) {
                screen->pixels[index] = 0;
                screen->dirty_rows[row] = 1;
            }
            ++byte;
        }
        ++row;
    }
}

static inline void vmu_screen_update_rect(
    VmuScreen *screen,
    uint8_t x,
    uint8_t y,
    uint8_t width,
    uint8_t height,
    uint8_t fill
) {
    if (x >= VMU_SCREEN_WIDTH || y >= VMU_SCREEN_HEIGHT || width == 0 || height == 0) {
        return;
    }

    if (width > VMU_SCREEN_WIDTH - x) {
        width = (uint8_t)(VMU_SCREEN_WIDTH - x);
    }
    if (height > VMU_SCREEN_HEIGHT - y) {
        height = (uint8_t)(VMU_SCREEN_HEIGHT - y);
    }

    const uint8_t right = (uint8_t)(x + width - 1);
    const uint8_t first_byte = x / 8;
    const uint8_t last_byte = right / 8;
    uint8_t row = y;
    while (row < y + height) {
        uint8_t byte = first_byte;
        while (byte <= last_byte) {
            uint8_t mask = 0xff;
            if (byte == first_byte) {
                mask = (uint8_t)(mask >> (x & 7));
            }
            if (byte == last_byte) {
                mask = (uint8_t)(mask & (uint8_t)(0xff << (7 - (right & 7))));
            }

            const uint8_t index = (uint8_t)(row * VMU_SCREEN_BYTES_PER_ROW + byte);
            const uint8_t old_value = screen->pixels[index];
            const uint8_t new_value = fill
                ? (uint8_t)(old_value | mask)
                : (uint8_t)(old_value & (uint8_t)~mask);
            if (new_value != old_value) {
                screen->pixels[index] = new_value;
                screen->dirty_rows[row] = 1;
            }

            ++byte;
        }
        ++row;
    }
}

static inline void vmu_screen_set_pixel(VmuScreen *screen, uint8_t x, uint8_t y) {
    vmu_screen_update_rect(screen, x, y, 1, 1, 1);
}

static inline void vmu_screen_fill_rect(
    VmuScreen *screen,
    uint8_t x,
    uint8_t y,
    uint8_t width,
    uint8_t height
) {
    vmu_screen_update_rect(screen, x, y, width, height, 1);
}

static inline void vmu_screen_clear_rect(
    VmuScreen *screen,
    uint8_t x,
    uint8_t y,
    uint8_t width,
    uint8_t height
) {
    vmu_screen_update_rect(screen, x, y, width, height, 0);
}

static __attribute__((noinline)) void vmu_screen_present(VmuScreen *screen) {
    enum { VCCR_LCD_ON = 0x80 };

    uint8_t row = 0;
    while (row < VMU_SCREEN_HEIGHT) {
        if (screen->dirty_rows[row]) {
            const uint8_t bank_row = row & 0x0f;
            const uint8_t bank = row / 16;
            const uint8_t offset = (uint8_t)(
                bank_row / 2 * 0x10 + bank_row % 2 * VMU_SCREEN_BYTES_PER_ROW
            );
            waterbear_vmu_xram_copy_from_main(
                bank,
                offset,
                &screen->pixels[row * VMU_SCREEN_BYTES_PER_ROW],
                VMU_SCREEN_BYTES_PER_ROW
            );
            screen->dirty_rows[row] = 0;
        }
        ++row;
    }

    waterbear_vmu_sfr_vccr_write(VCCR_LCD_ON);
}

#endif
