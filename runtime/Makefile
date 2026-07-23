# Makefile for Coal Runtime

# Compiler and flags
CC = clang
CFLAGS = -std=c11 -Wall -Wextra -Wpedantic -Werror -Iinclude
LDFLAGS = -lgc -lgmp

# Directories
SRC_DIR = src
TEST_DIR = tests
BUILD_DIR = build
LIB_DIR = $(BUILD_DIR)/lib
OBJ_DIR = $(BUILD_DIR)/obj
TEST_BIN_DIR = $(BUILD_DIR)/tests

# Source files
SRCS = $(wildcard $(SRC_DIR)/*.c)
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Test files
TEST_SRCS = $(filter-out $(TEST_DIR)/test_readln_interactive.c, $(wildcard $(TEST_DIR)/*.c))
TEST_BINS = $(TEST_SRCS:$(TEST_DIR)/%.c=$(TEST_BIN_DIR)/%)

# Library
LIBRARY = $(LIB_DIR)/libcoal_runtime.a

# Default target
.PHONY: all
all: $(LIBRARY)

# Build library
$(LIBRARY): $(OBJS) | $(LIB_DIR)
	ar rcs $@ $^
	@echo "Built $(LIBRARY)"

# Compile source files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Build tests
.PHONY: tests
tests: $(TEST_BINS)

$(TEST_BIN_DIR)/%: $(TEST_DIR)/%.c $(LIBRARY) | $(TEST_BIN_DIR)
	$(CC) $(CFLAGS) $< $(LIBRARY) $(LDFLAGS) -o $@

# Run tests
.PHONY: test
test: tests
	@echo "Running tests..."
	@for test in $(TEST_BINS); do \
		echo "Running $$test..."; \
		$$test || exit 1; \
		echo ""; \
	done
	@echo "All tests passed!"

# Create directories
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(TEST_BIN_DIR):
	mkdir -p $(TEST_BIN_DIR)

# Clean
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)

# Format
.PHONY: format
format:
	find src include tests -type f \( -name "*.c" -o -name "*.h" \) -exec clang-format -i {} +

# Install
.PHONY: install
install: $(LIBRARY)
	install -d /usr/local/lib
	install -m 644 $(LIBRARY) /usr/local/lib/
	install -d /usr/local/include/coal
	install -m 644 include/coal/*.h /usr/local/include/coal/

# Help
.PHONY: help
help:
	@echo "Coal Runtime Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Build the runtime library"
	@echo "  make tests    - Build test executables"
	@echo "  make test     - Build and run all tests"
	@echo "  make clean    - Remove build artifacts"
	@echo "  make format   - Format all C/H files using clang-format"
	@echo "  make install  - Install library and headers"
	@echo "  make help     - Show this help message"
