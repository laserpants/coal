# -----------------------------------------------------------------------------
# Coal toolchain
#
# Provides:
#   - Ubuntu 24.04
#   - Node.js 22 LTS
#   - LLVM / Clang
#   - GHC (via GHCup)
#   - Stack (via GHCup)
#   - GCC / build-essential
#   - GMP
#   - Boehm GC
#
# -----------------------------------------------------------------------------

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# -----------------------------------------------------------------------------
# System packages
# -----------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    gnupg \
    pkg-config \
    xz-utils \
    libffi-dev \
    libgmp-dev \
    libgc-dev \
    llvm \
    openssh-client \
    clang \
    && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# Install Node.js 22
# -----------------------------------------------------------------------------

RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash -

RUN apt-get update && apt-get install -y --no-install-recommends \
    nodejs \
    && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# Install GHCup
# -----------------------------------------------------------------------------

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

RUN curl --proto '=https' --tlsv1.2 -sSf \
    https://get-ghcup.haskell.org | sh

ENV PATH="/root/.ghcup/bin:${PATH}"

# -----------------------------------------------------------------------------
# Install GHC and Stack
#
# Adjust these versions as desired.
# -----------------------------------------------------------------------------

RUN ghcup install ghc 9.10.3
RUN ghcup set ghc 9.10.3

RUN ghcup install stack
RUN ghcup set stack

# -----------------------------------------------------------------------------
# Verify installation
# -----------------------------------------------------------------------------

RUN node --version

RUN ghc --version

RUN stack --version

RUN llvm-as --version

RUN llc --version

RUN clang --version

# -----------------------------------------------------------------------------
# Install helper scripts
# -----------------------------------------------------------------------------

COPY docker/scripts/coal-install.sh /usr/local/bin/coal-install

RUN chmod +x /usr/local/bin/coal-install

# -----------------------------------------------------------------------------

WORKDIR /workspace

CMD ["/bin/bash"]
