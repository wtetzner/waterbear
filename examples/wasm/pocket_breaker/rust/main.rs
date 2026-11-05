#![no_main]
#![no_std]

mod vmu;
#[allow(dead_code)]
mod waterbear_vmu;

use core::panic::PanicInfo;
use vmu::{Buttons, SCREEN_HEIGHT, SCREEN_WIDTH, Screen};

const BRICK_COLUMNS: u8 = 6;
const BRICK_ROWS: u8 = 3;
const BRICK_COUNT: u8 = BRICK_COLUMNS * BRICK_ROWS;
const BRICK_TOP: u8 = 2;
const BRICK_ROW_STRIDE: u8 = 4;
const BRICK_WIDTH: u8 = 6;
const BRICK_HEIGHT: u8 = 2;
const PADDLE_WIDTH: u8 = 9;
const PADDLE_HEIGHT: u8 = 2;
const PADDLE_Y: u8 = 29;
const BALL_SIZE: u8 = 2;

#[derive(Clone, Copy)]
struct Game {
    // One bit records each of the 18 bricks.
    bricks: u32,
    paddle_x: u8,
    ball_x: u8,
    ball_y: u8,
    ball_dx: i8,
    ball_dy: i8,
    playing: bool,
}

impl Game {
    #[inline(always)]
    fn new() -> Self {
        let mut game = Self {
            bricks: 0,
            paddle_x: 0,
            ball_x: 0,
            ball_y: 0,
            ball_dx: 1,
            ball_dy: -1,
            playing: false,
        };
        game.reset();
        game
    }

    #[inline(always)]
    fn update(&mut self, buttons: Buttons) {
        self.move_paddle(buttons);

        if !self.playing {
            self.ball_x = self.paddle_x + (PADDLE_WIDTH - BALL_SIZE) / 2;
            if buttons.a {
                self.playing = true;
            }
            return;
        }

        self.move_ball();
    }

    #[inline(always)]
    fn draw(&self, screen: &mut Screen) {
        screen.clear();

        let mut row = 0;
        while row < BRICK_ROWS {
            let mut column = 0;
            while column < BRICK_COLUMNS {
                let brick = row * BRICK_COLUMNS + column;
                if self.bricks & (1_u32 << brick) != 0 {
                    screen.fill_rect(
                        column * 8 + 1,
                        BRICK_TOP + row * BRICK_ROW_STRIDE,
                        BRICK_WIDTH,
                        BRICK_HEIGHT,
                    );
                }
                column += 1;
            }
            row += 1;
        }

        screen.fill_rect(self.paddle_x, PADDLE_Y, PADDLE_WIDTH, PADDLE_HEIGHT);
        screen.fill_rect(self.ball_x, self.ball_y, BALL_SIZE, BALL_SIZE);
    }

    #[inline(always)]
    fn redraw(&self, previous: Self, screen: &mut Screen) {
        let changed_bricks = previous.bricks ^ self.bricks;
        if changed_bricks != 0 {
            let mut row = 0;
            let mut brick = 0;
            while row < BRICK_ROWS {
                let mut column = 0;
                while column < BRICK_COLUMNS {
                    let mask = 1_u32 << brick;
                    if changed_bricks & mask != 0 {
                        let x = column * 8 + 1;
                        let y = BRICK_TOP + row * BRICK_ROW_STRIDE;
                        if self.bricks & mask != 0 {
                            screen.fill_rect(x, y, BRICK_WIDTH, BRICK_HEIGHT);
                        } else {
                            screen.clear_rect(x, y, BRICK_WIDTH, BRICK_HEIGHT);
                        }
                    }
                    brick += 1;
                    column += 1;
                }
                row += 1;
            }
        }

        let objects_moved = previous.paddle_x != self.paddle_x
            || previous.ball_x != self.ball_x
            || previous.ball_y != self.ball_y;
        if !objects_moved {
            return;
        }

        screen.clear_rect(previous.paddle_x, PADDLE_Y, PADDLE_WIDTH, PADDLE_HEIGHT);
        screen.clear_rect(previous.ball_x, previous.ball_y, BALL_SIZE, BALL_SIZE);
        screen.fill_rect(self.paddle_x, PADDLE_Y, PADDLE_WIDTH, PADDLE_HEIGHT);
        screen.fill_rect(self.ball_x, self.ball_y, BALL_SIZE, BALL_SIZE);
    }

    #[inline(always)]
    fn park_ball(&mut self) {
        self.ball_x = self.paddle_x + (PADDLE_WIDTH - BALL_SIZE) / 2;
        self.ball_y = PADDLE_Y - BALL_SIZE;
        self.ball_dx = 1;
        self.ball_dy = -1;
        self.playing = false;
    }

    #[inline(always)]
    fn reset(&mut self) {
        self.bricks = (1_u32 << BRICK_COUNT) - 1;
        self.paddle_x = (SCREEN_WIDTH - PADDLE_WIDTH) / 2;
        self.park_ball();
    }

    #[inline(always)]
    fn bricks_remain(&self) -> bool {
        self.bricks != 0
    }

    #[inline(always)]
    fn hit_brick(&mut self, next_y: u8) -> bool {
        if next_y < BRICK_TOP {
            return false;
        }

        let local_y = next_y - BRICK_TOP;
        let row = local_y / BRICK_ROW_STRIDE;
        if row >= BRICK_ROWS || local_y % BRICK_ROW_STRIDE >= BRICK_HEIGHT {
            return false;
        }

        // Bricks occupy 8-by-4 cells, with a gap inside each cell.
        let column = (self.ball_x + BALL_SIZE / 2) / 8;
        let brick = row * BRICK_COLUMNS + column;
        let mask = 1_u32 << brick;
        if self.bricks & mask == 0 {
            return false;
        }

        self.bricks &= !mask;
        true
    }

    #[inline(always)]
    fn move_paddle(&mut self, buttons: Buttons) {
        if buttons.left {
            if !buttons.right && self.paddle_x > 0 {
                self.paddle_x -= 1;
            }
        } else if buttons.right && self.paddle_x + PADDLE_WIDTH < SCREEN_WIDTH {
            self.paddle_x += 1;
        }
    }

    #[inline(always)]
    fn move_ball(&mut self) {
        if self.ball_dx < 0 {
            if self.ball_x == 0 {
                self.ball_dx = 1;
            } else {
                self.ball_x -= 1;
            }
        } else if self.ball_x + BALL_SIZE >= SCREEN_WIDTH {
            self.ball_dx = -1;
        } else {
            self.ball_x += 1;
        }

        if self.ball_dy < 0 && self.ball_y == 0 {
            self.ball_dy = 1;
            return;
        }

        let next_y = if self.ball_dy < 0 {
            self.ball_y - 1
        } else {
            self.ball_y + 1
        };

        let crosses_paddle = self.ball_dy > 0
            && self.ball_y + BALL_SIZE <= PADDLE_Y
            && next_y + BALL_SIZE >= PADDLE_Y;
        let overlaps_paddle =
            self.ball_x + BALL_SIZE > self.paddle_x && self.ball_x < self.paddle_x + PADDLE_WIDTH;
        if crosses_paddle && overlaps_paddle {
            self.ball_dy = -1;
            return;
        }

        if next_y > SCREEN_HEIGHT - BALL_SIZE {
            self.park_ball();
            return;
        }

        if self.hit_brick(next_y) {
            self.ball_dy = -self.ball_dy;
            if !self.bricks_remain() {
                self.reset();
            }
            return;
        }

        self.ball_y = next_y;
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo<'_>) -> ! {
    loop {}
}

#[unsafe(export_name = "main")]
pub extern "C" fn pocket_breaker_main() -> ! {
    let mut screen = Screen::new();
    let mut game = Game::new();

    vmu::initialize();
    game.draw(&mut screen);
    screen.present();

    loop {
        let previous = game;
        game.update(vmu::read_buttons());
        game.redraw(previous, &mut screen);
        screen.present();
    }
}
