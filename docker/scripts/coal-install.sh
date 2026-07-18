#!/usr/bin/env bash

set -euo pipefail

REF="${1:-main}"

REPO="https://codeberg.org/laserpants/coal.git"
DIR="/tmp/coal"

rm -rf "$DIR"

git clone "$REPO" "$DIR"

cd "$DIR"

git checkout "$REF"

bash runtime/scripts/combine.sh

stack install --local-bin-path /usr/local/bin --copy-bins

coal --version
