WATERBEAR ?= ../../../../target/release/waterbear
CARGO ?= cargo
TARGET := wasm32-unknown-unknown
WASM := target/$(TARGET)/release/pocket_breaker.wasm

.PHONY: all bindings clean

all: pocket_breaker.vms

pocket_breaker.vms: $(WASM) waterbear.toml
	$(WATERBEAR) wasm build $(WASM) --config waterbear.toml --out $@

$(WASM): Cargo.toml main.rs vmu.rs waterbear_vmu.rs
	$(CARGO) build --release --target $(TARGET)

bindings:
	$(WATERBEAR) wasm header --language rust --output waterbear_vmu.rs

clean:
	$(CARGO) clean
	rm -f pocket_breaker.vms
