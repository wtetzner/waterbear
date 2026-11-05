#include "vmu.hpp"

namespace {

constexpr uint8_t brick_columns = 6;
constexpr uint8_t brick_rows = 3;
constexpr uint8_t brick_count = brick_columns * brick_rows;
constexpr uint8_t brick_top = 2;
constexpr uint8_t brick_row_stride = 4;
constexpr uint8_t brick_width = 6;
constexpr uint8_t brick_height = 2;
constexpr uint8_t paddle_width = 9;
constexpr uint8_t paddle_height = 2;
constexpr uint8_t paddle_y = 29;
constexpr uint8_t ball_size = 2;

class Game {
public:
    Game() { reset(); }

    void update(vmu::Buttons buttons) {
        move_paddle(buttons);

        if (!playing_) {
            ball_x_ = paddle_x_ + (paddle_width - ball_size) / 2;
            if (buttons.a) {
                playing_ = true;
            }
            return;
        }

        move_ball();
    }

    void draw(vmu::Screen &screen) const {
        screen.clear();

        uint8_t row = 0;
        while (row < brick_rows) {
            uint8_t column = 0;
            while (column < brick_columns) {
                const auto brick = static_cast<uint8_t>(row * brick_columns + column);
                if (bricks_ & (uint32_t{1} << brick)) {
                    screen.fill_rect(
                        static_cast<uint8_t>(column * 8 + 1),
                        static_cast<uint8_t>(brick_top + row * brick_row_stride),
                        brick_width,
                        brick_height
                    );
                }
                ++column;
            }
            ++row;
        }

        screen.fill_rect(paddle_x_, paddle_y, paddle_width, paddle_height);
        screen.fill_rect(ball_x_, ball_y_, ball_size, ball_size);
    }

    void redraw(const Game &previous, vmu::Screen &screen) const {
        const auto changed_bricks = previous.bricks_ ^ bricks_;
        if (changed_bricks != 0) {
            uint8_t row = 0;
            uint8_t brick = 0;
            while (row < brick_rows) {
                uint8_t column = 0;
                while (column < brick_columns) {
                    const auto mask = uint32_t{1} << brick;
                    if (changed_bricks & mask) {
                        const auto x = static_cast<uint8_t>(column * 8 + 1);
                        const auto y = static_cast<uint8_t>(
                            brick_top + row * brick_row_stride
                        );
                        if (bricks_ & mask) {
                            screen.fill_rect(x, y, brick_width, brick_height);
                        } else {
                            screen.clear_rect(x, y, brick_width, brick_height);
                        }
                    }
                    ++brick;
                    ++column;
                }
                ++row;
            }
        }

        const bool objects_moved = previous.paddle_x_ != paddle_x_
            || previous.ball_x_ != ball_x_
            || previous.ball_y_ != ball_y_;
        if (!objects_moved) {
            return;
        }

        screen.clear_rect(
            previous.paddle_x_,
            paddle_y,
            paddle_width,
            paddle_height
        );
        screen.clear_rect(
            previous.ball_x_,
            previous.ball_y_,
            ball_size,
            ball_size
        );
        screen.fill_rect(paddle_x_, paddle_y, paddle_width, paddle_height);
        screen.fill_rect(ball_x_, ball_y_, ball_size, ball_size);
    }

private:
    void park_ball() {
        ball_x_ = paddle_x_ + (paddle_width - ball_size) / 2;
        ball_y_ = paddle_y - ball_size;
        ball_dx_ = 1;
        ball_dy_ = -1;
        playing_ = false;
    }

    void reset() {
        bricks_ = (uint32_t{1} << brick_count) - 1;
        paddle_x_ = (vmu::screen_width - paddle_width) / 2;
        park_ball();
    }

    bool bricks_remain() const {
        return bricks_ != 0;
    }

    bool hit_brick(uint8_t next_y) {
        if (next_y < brick_top) {
            return false;
        }

        const auto local_y = static_cast<uint8_t>(next_y - brick_top);
        const auto row = static_cast<uint8_t>(local_y / brick_row_stride);
        if (row >= brick_rows || local_y % brick_row_stride >= brick_height) {
            return false;
        }

        // Bricks occupy 8-by-4 cells, with a gap inside each cell.
        const auto column = static_cast<uint8_t>((ball_x_ + ball_size / 2) / 8);
        const auto brick = static_cast<uint8_t>(row * brick_columns + column);
        const auto mask = uint32_t{1} << brick;
        if ((bricks_ & mask) == 0) {
            return false;
        }

        bricks_ &= ~mask;
        return true;
    }

    void move_paddle(vmu::Buttons buttons) {
        if (buttons.left) {
            if (!buttons.right && paddle_x_ > 0) {
                --paddle_x_;
            }
        } else if (buttons.right && paddle_x_ + paddle_width < vmu::screen_width) {
            ++paddle_x_;
        }
    }

    void move_ball() {
        if (ball_dx_ < 0) {
            if (ball_x_ == 0) {
                ball_dx_ = 1;
            } else {
                --ball_x_;
            }
        } else if (ball_x_ + ball_size >= vmu::screen_width) {
            ball_dx_ = -1;
        } else {
            ++ball_x_;
        }

        if (ball_dy_ < 0 && ball_y_ == 0) {
            ball_dy_ = 1;
            return;
        }

        const auto next_y = ball_dy_ < 0
            ? static_cast<uint8_t>(ball_y_ - 1)
            : static_cast<uint8_t>(ball_y_ + 1);

        const bool crosses_paddle = ball_dy_ > 0
            && ball_y_ + ball_size <= paddle_y
            && next_y + ball_size >= paddle_y;
        const bool overlaps_paddle = ball_x_ + ball_size > paddle_x_
            && ball_x_ < paddle_x_ + paddle_width;
        if (crosses_paddle && overlaps_paddle) {
            ball_dy_ = -1;
            return;
        }

        if (next_y > vmu::screen_height - ball_size) {
            park_ball();
            return;
        }

        if (hit_brick(next_y)) {
            ball_dy_ = static_cast<int8_t>(-ball_dy_);
            if (!bricks_remain()) {
                reset();
            }
            return;
        }

        ball_y_ = next_y;
    }

    // One bit records each of the 18 bricks.
    uint32_t bricks_;
    uint8_t paddle_x_;
    uint8_t ball_x_;
    uint8_t ball_y_;
    int8_t ball_dx_;
    int8_t ball_dy_;
    bool playing_;
};

} // namespace

extern "C" __attribute__((export_name("main"), noreturn))
void pocket_breaker_main() {
    vmu::Screen screen;
    Game game;

    vmu::initialize();
    game.draw(screen);
    screen.present();

    while (true) {
        const Game previous = game;
        game.update(vmu::read_buttons());
        game.redraw(previous, screen);
        screen.present();
    }
}
