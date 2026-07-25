# Using the Coal compiler with Docker

This guide explains how to use Docker to compile and run Coal programs without needing to install Haskell, LLVM, or other dependencies locally.

## Prerequisites

- [Docker](https://docs.docker.com/get-docker/) installed and running on your machine
- A terminal with Docker access

## Available Docker images

Coal provides two official Docker images:

### `ghcr.io/laserpants/coal:latest`

The **recommended image for most users**. This image includes the complete Coal compiler toolchain with the `coal` binary pre-installed and ready to use. It's based on `coal-dev` and includes:

- The Coal compiler (`coal` CLI command)
- All runtime dependencies (LLVM, GHC, Stack, GMP, Boehm GC)
- Ubuntu 24.04 base system

Use this image if you want to **compile and run Coal programs** without building the compiler yourself.

### `ghcr.io/laserpants/coal-dev:latest`

The **development image for contributors**. This image includes only the build toolchain needed to compile Coal from source:

- Haskell toolchain (GHC 9.4.8, Stack)
- LLVM / Clang
- GCC / build-essential
- GMP and Boehm GC development libraries
- Node.js 22 LTS
- Ubuntu 24.04 base system

Use this image if you want to **contribute to the Coal project** or experiment with the compiler source code. You'll need to build the compiler yourself using the `coal-install` script from inside the container.

## Quick start

### Using the pre-built compiler image

The fastest way to compile Coal programs is using the `coal:latest` image:

Navigate to your Coal project directory and run:

```bash
docker run --rm \
  -v "$PWD:/src" \
  -w /src \
  ghcr.io/laserpants/coal:latest \
  compile -I. Main.coal -o dist
```

### Interactive use

For an interactive development workflow:

```bash
docker run --rm \
  -it \
  -v "$PWD:/src" \
  -w /src \
  --entrypoint bash \
  ghcr.io/laserpants/coal:latest
```

#### Command explanation

| Flag | Purpose |
|------|---------|
| `--rm` | Automatically remove the container when you exit |
| `-it` | Run interactively with a TTY (gives you a shell prompt) |
| `-v "$PWD:/src"` | Mount your current directory to `/src` in the container |
| `-w /src` | Set the working directory inside the container |
| `--entrypoint bash` | Start a bash shell (instead of the default `coal` command) |

**Important:** the `-v "$pwd:/src"` flag makes your local files accessible inside the container. any changes you make inside `/src` are immediately reflected in your local directory.

Once inside the container, the `coal` command is ready to use:

```bash
# Compile your program
coal compile -I. Main.coal -o dist

# Run it
./dist

# Show the help text
coal --help
```
