#include "vmu.h"

enum {
    BRICK_COLUMNS = 6,
    BRICK_ROWS = 3,
    BRICK_COUNT = BRICK_COLUMNS * BRICK_ROWS,
    BRICK_TOP = 2,
    BRICK_ROW_STRIDE = 4,
    BRICK_WIDTH = 6,
    BRICK_HEIGHT = 2,
    PADDLE_WIDTH = 9,
    PADDLE_HEIGHT = 2,
    PADDLE_Y = 29,
    BALL_SIZE = 2,
};

typedef struct {
    /* One bit records each of the 18 bricks. */
    uint32_t bricks;
    uint8_t paddle_x;
    uint8_t ball_x;
    uint8_t ball_y;
    int8_t ball_dx;
    int8_t ball_dy;
    uint8_t playing;
} Game;

static void park_ball(Game *game) {
    game->ball_x = (uint8_t)(game->paddle_x + (PADDLE_WIDTH - BALL_SIZE) / 2);
    game->ball_y = PADDLE_Y - BALL_SIZE;
    game->ball_dx = 1;
    game->ball_dy = -1;
    game->playing = 0;
}

static void reset_game(Game *game) {
    game->bricks = (1UL << BRICK_COUNT) - 1;
    game->paddle_x = (VMU_SCREEN_WIDTH - PADDLE_WIDTH) / 2;
    park_ball(game);
}

static uint8_t bricks_remain(const Game *game) {
    return game->bricks != 0;
}

static uint8_t hit_brick(Game *game, uint8_t next_y) {
    if (next_y < BRICK_TOP) {
        return 0;
    }

    const uint8_t local_y = (uint8_t)(next_y - BRICK_TOP);
    const uint8_t row = local_y / BRICK_ROW_STRIDE;
    if (row >= BRICK_ROWS || local_y % BRICK_ROW_STRIDE >= BRICK_HEIGHT) {
        return 0;
    }

    /* Bricks occupy 8-by-4 cells, with a gap inside each cell. */
    const uint8_t column = (uint8_t)((game->ball_x + BALL_SIZE / 2) / 8);
    const uint8_t brick = (uint8_t)(row * BRICK_COLUMNS + column);
    const uint32_t mask = 1UL << brick;
    if ((game->bricks & mask) == 0) {
        return 0;
    }

    game->bricks &= ~mask;
    return 1;
}

static void move_paddle(Game *game, VmuButtons buttons) {
    if (buttons.left) {
        if (!buttons.right && game->paddle_x > 0) {
            --game->paddle_x;
        }
    } else if (buttons.right && game->paddle_x + PADDLE_WIDTH < VMU_SCREEN_WIDTH) {
        ++game->paddle_x;
    }
}

static void move_ball(Game *game) {
    if (game->ball_dx < 0) {
        if (game->ball_x == 0) {
            game->ball_dx = 1;
        } else {
            --game->ball_x;
        }
    } else if (game->ball_x + BALL_SIZE >= VMU_SCREEN_WIDTH) {
        game->ball_dx = -1;
    } else {
        ++game->ball_x;
    }

    if (game->ball_dy < 0 && game->ball_y == 0) {
        game->ball_dy = 1;
        return;
    }

    const uint8_t next_y = game->ball_dy < 0
        ? (uint8_t)(game->ball_y - 1)
        : (uint8_t)(game->ball_y + 1);

    const uint8_t crosses_paddle = game->ball_dy > 0
        && game->ball_y + BALL_SIZE <= PADDLE_Y
        && next_y + BALL_SIZE >= PADDLE_Y;
    const uint8_t overlaps_paddle = game->ball_x + BALL_SIZE > game->paddle_x
        && game->ball_x < game->paddle_x + PADDLE_WIDTH;
    if (crosses_paddle && overlaps_paddle) {
        game->ball_dy = -1;
        return;
    }

    if (next_y > VMU_SCREEN_HEIGHT - BALL_SIZE) {
        park_ball(game);
        return;
    }

    if (hit_brick(game, next_y)) {
        game->ball_dy = (int8_t)-game->ball_dy;
        if (!bricks_remain(game)) {
            reset_game(game);
        }
        return;
    }

    game->ball_y = next_y;
}

static void update_game(Game *game, VmuButtons buttons) {
    move_paddle(game, buttons);

    if (!game->playing) {
        game->ball_x = (uint8_t)(game->paddle_x + (PADDLE_WIDTH - BALL_SIZE) / 2);
        if (buttons.a) {
            game->playing = 1;
        }
        return;
    }

    move_ball(game);
}

static void draw_game(const Game *game, VmuScreen *screen) {
    vmu_screen_clear(screen);

    uint8_t row = 0;
    while (row < BRICK_ROWS) {
        uint8_t column = 0;
        while (column < BRICK_COLUMNS) {
            const uint8_t brick = (uint8_t)(row * BRICK_COLUMNS + column);
            if (game->bricks & (1UL << brick)) {
                vmu_screen_fill_rect(
                    screen,
                    (uint8_t)(column * 8 + 1),
                    (uint8_t)(BRICK_TOP + row * BRICK_ROW_STRIDE),
                    BRICK_WIDTH,
                    BRICK_HEIGHT
                );
            }
            ++column;
        }
        ++row;
    }

    vmu_screen_fill_rect(
        screen,
        game->paddle_x,
        PADDLE_Y,
        PADDLE_WIDTH,
        PADDLE_HEIGHT
    );
    vmu_screen_fill_rect(screen, game->ball_x, game->ball_y, BALL_SIZE, BALL_SIZE);
}

static void redraw_game(const Game *previous, const Game *game, VmuScreen *screen) {
    const uint32_t changed_bricks = previous->bricks ^ game->bricks;
    if (changed_bricks) {
        uint8_t row = 0;
        uint8_t brick = 0;
        while (row < BRICK_ROWS) {
            uint8_t column = 0;
            while (column < BRICK_COLUMNS) {
                const uint32_t mask = 1UL << brick;
                if (changed_bricks & mask) {
                    if (game->bricks & mask) {
                        vmu_screen_fill_rect(
                            screen,
                            (uint8_t)(column * 8 + 1),
                            (uint8_t)(BRICK_TOP + row * BRICK_ROW_STRIDE),
                            BRICK_WIDTH,
                            BRICK_HEIGHT
                        );
                    } else {
                        vmu_screen_clear_rect(
                            screen,
                            (uint8_t)(column * 8 + 1),
                            (uint8_t)(BRICK_TOP + row * BRICK_ROW_STRIDE),
                            BRICK_WIDTH,
                            BRICK_HEIGHT
                        );
                    }
                }
                ++brick;
                ++column;
            }
            ++row;
        }
    }

    const uint8_t objects_moved = previous->paddle_x != game->paddle_x
        || previous->ball_x != game->ball_x
        || previous->ball_y != game->ball_y;
    if (!objects_moved) {
        return;
    }

    vmu_screen_clear_rect(
        screen,
        previous->paddle_x,
        PADDLE_Y,
        PADDLE_WIDTH,
        PADDLE_HEIGHT
    );
    vmu_screen_clear_rect(
        screen,
        previous->ball_x,
        previous->ball_y,
        BALL_SIZE,
        BALL_SIZE
    );
    vmu_screen_fill_rect(
        screen,
        game->paddle_x,
        PADDLE_Y,
        PADDLE_WIDTH,
        PADDLE_HEIGHT
    );
    vmu_screen_fill_rect(screen, game->ball_x, game->ball_y, BALL_SIZE, BALL_SIZE);
}

__attribute__((export_name("main"), noreturn))
void pocket_breaker_main(void) {
    Game game;
    VmuScreen screen;

    vmu_initialize();
    vmu_screen_initialize(&screen);
    reset_game(&game);
    draw_game(&game, &screen);
    vmu_screen_present(&screen);

    for (;;) {
        const Game previous = game;
        update_game(&game, vmu_read_buttons());
        redraw_game(&previous, &game, &screen);
        vmu_screen_present(&screen);
    }
}
