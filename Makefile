# Makefile for wedsa project

# Variables
HC = ghc
CABAL = cabal
CABAL_FLAGS =
BUILD_DIR = dist-newstyle
SOURCE_DIR = src
APP_DIR = app
TEST_DIR = test
EXECUTABLE = wedsa

# Default target: build everything
.PHONY: all
all: build

# Build the project
.PHONY: build
build:
	$(CABAL) build $(CABAL_FLAGS) all

# Clean build artifacts
.PHONY: clean
clean:
	$(CABAL) clean
	rm -f *.hi *.o

# Run tests
.PHONY: check
check:
	$(CABAL) test $(CABAL_FLAGS)

# Run the program with test code
.PHONY: run
run: build
	$(CABAL) run $(CABAL_FLAGS) $(EXECUTABLE)

# Generate documentation
.PHONY: docs
docs:
	$(CABAL) haddock $(CABAL_FLAGS) --haddock-all

# Format Haskell source code using stylish-haskell (if installed)
.PHONY: format
format:
	@which stylish-haskell > /dev/null && find $(SOURCE_DIR) $(APP_DIR) $(TEST_DIR) -name "*.hs" -exec stylish-haskell -i {} \; || echo "stylish-haskell not found, skipping formatting"

# Start a REPL with the library loaded
.PHONY: repl
repl:
	$(CABAL) repl $(CABAL_FLAGS) lib:wedsa

# Run tests with more verbose output
.PHONY: test-verbose
test-verbose:
	$(CABAL) test $(CABAL_FLAGS) --test-show-details=always

# Install in the user's environment
.PHONY: install
install:
	$(CABAL) install $(CABAL_FLAGS) --lib

# Generate a tags file for vim
.PHONY: tags
tags:
	hasktags -e $(SOURCE_DIR) $(APP_DIR) $(TEST_DIR)

# Only build the library component
.PHONY: lib
lib:
	$(CABAL) build $(CABAL_FLAGS) lib:wedsa

# Only build the executable component
.PHONY: exe
exe:
	$(CABAL) build $(CABAL_FLAGS) exe:wedsa

# Watch for changes and rebuild
.PHONY: watch
watch:
	$(CABAL) build $(CABAL_FLAGS) --enable-benchmarks --enable-tests all --file-watch

# Print project summary
.PHONY: info
info:
	@echo "Project: wedsa"
	@echo "Cabal version: $$($(CABAL) --version | head -n1)"
	@echo "GHC version: $$($(HC) --version | head -n1)"
	@echo "Components: lib:wedsa, exe:wedsa, test:cppparser-test"

# Help command
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  make           - Build the project (same as 'make build')"
	@echo "  make build     - Build all components"
	@echo "  make check     - Run the test suite"
	@echo "  make run       - Run the executable"
	@echo "  make clean     - Clean build artifacts"
	@echo "  make docs      - Generate documentation"
	@echo "  make format    - Format source code (requires stylish-haskell)"
	@echo "  make repl      - Start a REPL with the library loaded"
	@echo "  make tags      - Generate tags file for vim"
	@echo "  make watch     - Watch for changes and rebuild"
	@echo "  make info      - Show project information"
