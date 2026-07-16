#!/usr/bin/env bash
set -e

CMD="${1:-build}"

case "$CMD" in
  build | install | test | clean)
    runtime/scripts/combine.sh
    stack "$@"
    ;;
  *)
    stack "$@"
    ;;
esac
