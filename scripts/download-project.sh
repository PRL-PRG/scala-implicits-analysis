#!/bin/sh

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <url> <dir>"
    exit 1;
fi

URL="$1"
DIR="$2"

[ -d "$DIR" ] && [ $REDO -ne 1 ] && exit 0

GIT_TERMINAL_PROMPT=0 git clone "$URL" "$DIR"

cd $DIR

git submodule update --init --recursive

# checkout the latest tag
if git describe --abbrev=0; then
    git checkout $(git describe --abbrev=0)
fi
