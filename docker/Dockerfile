FROM fpco/stack-build:lts-21.25

RUN apt-get update && apt-get install -y --no-install-recommends \
    llvm \
    clang \
    libgc-dev \
    libgmp-dev \
    build-essential \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

# Verify the toolchain during the image build.
RUN stack --version && \
    llvm-as --version && \
    llc --version && \
    clang --version

COPY scripts/coal-install.sh /usr/local/bin/coal-install

RUN chmod +x /usr/local/bin/coal-install

# Default working directory.
WORKDIR /workspace

# Add Stack's install location to PATH.
ENV PATH="/root/.local/bin:${PATH}"

CMD ["/bin/bash"]
