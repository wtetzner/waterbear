#!/bin/bash

set -e

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE[0]}"))
cd "${SCRIPT_DIR}"

# rustup target add x86_64-unknown-linux-musl
# rustup target add x86_64-pc-windows-msvc
# rustup target add x86_64-pc-windows-gnu
# rustup target add aarch64-apple-darwin
# rustup target add x86_64-apple-darwin

# cross-rs docs:
# cargo install -q cross
# https://github.com/cross-rs/cross/blob/main/docs/getting-started.md

# Linux docker container
# docker pull --platform linux/x86_64 ghcr.io/cross-rs/x86_64-unknown-linux-musl:0.2.5

# Setup Windows docker containers
# git clone https://github.com/cross-rs/cross
# cd cross
# git submodule update --init --remote
# cargo build-docker-image x86_64-pc-windows-msvc-cross

# Make sure jq is installed
# brew install jq

# Make sure rust is up-to-date
# rustup -q toolchain install stable

echo "* Build for aarch64-apple-darwin"
RUSTFLAGS=-Awarnings cargo -q build -q --release --target aarch64-apple-darwin

echo "* Build for x86_64-apple-darwin"
RUSTFLAGS=-Awarnings cargo -q build -q --release --target x86_64-apple-darwin

# cross build --release --target x86_64-pc-windows-gnu

echo "* Build for x86_64-pc-windows-msvc"
cross build --release --target x86_64-pc-windows-msvc

echo "* Build for x86_64-unknown-linux-musl"
RUSTFLAGS='-C link-arg=-s' cross -q build -q --release --target x86_64-unknown-linux-musl

mkdir -p ./target/release-binaries
rm -rf ./target/release-binaries/*

echo "* Copy linux binary to $(realpath ./target/release-binaries)"
cp ./target/x86_64-unknown-linux-musl/release/waterbear ./target/release-binaries/waterbear-linux

echo "* Copy windos binary to $(realpath ./target/release-binaries)"
cp ./target/x86_64-pc-windows-msvc/release/waterbear.exe ./target/release-binaries/waterbear.exe

echo "* Create universal macos binary"
lipo -create -output ./target/release-binaries/waterbear-macos ./target/aarch64-apple-darwin/release/waterbear ./target/x86_64-apple-darwin/release/waterbear
