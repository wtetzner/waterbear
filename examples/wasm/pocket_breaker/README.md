PocketBreaker
=============

PocketBreaker is a small Breakout-style game for the VMU. Move the paddle with
left and right, and press A to launch the ball. Missing the ball starts another
serve; clearing the wall starts a new wall.

The `c`, `cpp`, and `rust` directories contain equivalent implementations in C,
C++, and Rust. Each project includes generated Waterbear bindings, a small
language-appropriate VMU API, the game itself, and a Makefile that produces a
`pocket_breaker.vms` file in that project directory.
