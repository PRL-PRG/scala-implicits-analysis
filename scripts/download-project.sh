#!/bin/sh

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <url> <dir>"
    exit 1;
fi

REDO=${REDO:-0}
URL="$1"
DIR="$2"

[ $REDO -eq 0 ] || rm -fr "$DIR"

[ -d "$DIR" ] || GIT_TERMINAL_PROMPT=0 git clone "$URL" "$DIR"

cd "$DIR"

[ -f .gitmodules ] && git submodule update --init --recursive

