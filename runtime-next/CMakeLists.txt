cmake_minimum_required(VERSION 3.10)
project(coal_runtime C)

# Set C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wpedantic -Werror")
set(CMAKE_C_FLAGS_DEBUG "-g -O0")
set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# Find required libraries
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(GMP REQUIRED gmp)

# Source files
set(RUNTIME_SOURCES
    src/runtime.c
    src/gc.c
    src/closure.c
    src/apply.c
    src/bignum.c
    src/string.c
    src/char.c
    src/io.c
    src/panic.c
    src/record.c
)

# Build static library
add_library(coal_runtime STATIC ${RUNTIME_SOURCES})

# Link libraries
target_include_directories(coal_runtime PUBLIC 
    ${GC_INCLUDE_DIRS}
    ${GMP_INCLUDE_DIRS}
)

target_link_libraries(coal_runtime
    ${GC_LIBRARIES}
    ${GMP_LIBRARIES}
)

# Enable testing
enable_testing()

# Test executables
set(TEST_SOURCES
    test_gc
    test_closure
    test_apply
    test_bignum
    test_string
    test_char
    test_io
    test_value
    test_record
)

foreach(test_name ${TEST_SOURCES})
    add_executable(${test_name} tests/${test_name}.c)
    target_link_libraries(${test_name} coal_runtime)
    add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()

# Install targets
install(TARGETS coal_runtime
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/coal
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)
