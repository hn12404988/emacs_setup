# Portable Emacs container with custom configuration
FROM ubuntu:24.04

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive
ENV TERM=xterm-256color

# Install Emacs and dependencies
RUN apt-get update && apt-get install -y \
    emacs-nox \
    git \
    curl \
    wget \
    # Spell checking (aspell preferred per init.el)
    aspell \
    aspell-en \
    # Markdown preview
    pandoc \
    # Build tools (needed for some Emacs packages)
    build-essential \
    cmake \
    libtool \
    libtool-bin \
    # For vterm/eat compilation
    libvterm-dev \
    # Fonts support
    fontconfig \
    unzip \
    # Useful utilities
    ripgrep \
    fd-find \
    && rm -rf /var/lib/apt/lists/*

# Install JetBrains Mono font
RUN mkdir -p /usr/share/fonts/truetype/jetbrains-mono && \
    cd /tmp && \
    wget -q https://github.com/JetBrains/JetBrainsMono/releases/download/v2.304/JetBrainsMono-2.304.zip && \
    unzip -q JetBrainsMono-2.304.zip -d jetbrains-mono && \
    cp jetbrains-mono/fonts/ttf/*.ttf /usr/share/fonts/truetype/jetbrains-mono/ && \
    fc-cache -f -v && \
    rm -rf /tmp/jetbrains-mono /tmp/JetBrainsMono-2.304.zip

# Install Rust (optional - comment out if not needed)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y && \
    . /root/.cargo/env && \
    rustup component add rust-analyzer clippy rustfmt

# Set up Emacs directory
RUN mkdir -p /root/.emacs.d

# Copy init.el
COPY init.el /root/.emacs.d/init.el

# Pre-install Emacs packages (speeds up first run)
# This runs Emacs in batch mode to bootstrap straight.el and install packages
RUN emacs --batch --load /root/.emacs.d/init.el 2>&1 || true

# Set up environment
ENV PATH="/root/.cargo/bin:${PATH}"

# Working directory
WORKDIR /workspace

# Default command - run Emacs
CMD ["emacs"]
