#!/bin/bash

set -e

path="$1"

mkdir -p "$1/_analysis_"
./generate-repo-metadata.sh "$path"
./generate-sloc.sh "$path"
