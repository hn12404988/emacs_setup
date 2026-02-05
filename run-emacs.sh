#!/bin/bash
# Run portable Emacs container
#
# Usage:
#   ./run-emacs.sh              # Interactive Emacs in current directory
#   ./run-emacs.sh /path/to/dir # Interactive Emacs in specified directory
#   ./run-emacs.sh --build      # Rebuild the image first

IMAGE_NAME="my-emacs"
WORKDIR="${1:-.}"

# Build if requested or image doesn't exist
if [[ "$1" == "--build" ]] || ! docker image inspect "$IMAGE_NAME" &>/dev/null; then
    echo "Building Emacs container..."
    docker build -t "$IMAGE_NAME" "$(dirname "$0")"
    [[ "$1" == "--build" ]] && WORKDIR="${2:-.}"
fi

# Run with current directory mounted
docker run -it --rm \
    -v "$(cd "$WORKDIR" && pwd)":/workspace \
    -e TERM="$TERM" \
    "$IMAGE_NAME" \
    emacs
