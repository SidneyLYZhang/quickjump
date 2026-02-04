.PHONY: all build compress install clean help

all: build compress

build:
	cabal build --enable-executable-stripping --disable-debug-info

compress:
	@echo "Compressing executable with UPX..."
	@EXE_PATH=$$(cabal list-bin quickjump); \
	if [ -f "$$EXE_PATH" ]; then \
		if command -v upx >/dev/null 2>&1; then \
			upx --best --lzma "$$EXE_PATH"; \
			echo "Compression complete: $$EXE_PATH"; \
		else \
			echo "UPX not found. Installing via npx..."; \
			npx -y @upx/upx@latest --best --lzma "$$EXE_PATH"; \
			echo "Compression complete: $$EXE_PATH"; \
		fi \
	else \
		echo "Error: Executable not found. Please run 'make build' first."; \
		exit 1; \
	fi

install:
	cabal install

clean:
	cabal clean

help:
	@echo "QuickJump Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all      - Build and compress the executable (default)"
	@echo "  build    - Build the executable with cabal"
	@echo "  compress - Compress the executable with UPX (uses npx if UPX not installed)"
	@echo "  install  - Install the executable with cabal"
	@echo "  clean    - Clean build artifacts"
	@echo "  help     - Show this help message"
