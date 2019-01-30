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

TAG=$(git log --tags --simplify-by-decoration --pretty="format:%ct %h %D" | grep 'tag:' | sort -k 1 -n -r | head -1 | cut -f 2 -d ' ')

[ -n $TAG ] && git checkout $TAG

