WATERBEAR ?= ../../../../target/release/waterbear
LLVM_PREFIX := $(firstword $(wildcard /opt/homebrew/opt/llvm /usr/local/opt/llvm))
WASM_LD := $(firstword $(wildcard \
	/opt/homebrew/opt/lld/bin/wasm-ld \
	/usr/local/opt/lld/bin/wasm-ld \
	$(LLVM_PREFIX)/bin/wasm-ld))
CC = $(if $(LLVM_PREFIX),$(LLVM_PREFIX)/bin/clang,clang)

CFLAGS := --target=wasm32 -std=c11 -Oz -ffreestanding -fno-builtin -nostdlib \
	-Wall -Wextra
LDFLAGS := $(if $(WASM_LD),-fuse-ld=$(WASM_LD)) \
	-Wl,--no-entry -Wl,--export=main -Wl,--allow-undefined \
	-Wl,--gc-sections

.PHONY: all bindings clean

all: pocket_breaker.vms

pocket_breaker.vms: pocket_breaker.wasm waterbear.toml
	$(WATERBEAR) wasm build pocket_breaker.wasm --config waterbear.toml --out $@

pocket_breaker.wasm: main.c vmu.h waterbear_vmu.h
	@$(CC) --print-targets 2>/dev/null | grep -q 'wasm32' || { \
		echo 'error: $(CC) does not include the WebAssembly backend.'; \
		echo 'Install LLVM with "brew install llvm lld", or run make CC=/path/to/clang.'; \
		exit 1; \
	}
	@test -n "$(WASM_LD)" || command -v wasm-ld >/dev/null 2>&1 || { \
		echo 'error: the WebAssembly linker was not found.'; \
		echo 'Install LLD with "brew install lld".'; \
		exit 1; \
	}
	$(CC) $(CFLAGS) $(LDFLAGS) main.c -o $@

bindings:
	$(WATERBEAR) wasm header --language c --output waterbear_vmu.h

clean:
	rm -f pocket_breaker.wasm pocket_breaker.vms
