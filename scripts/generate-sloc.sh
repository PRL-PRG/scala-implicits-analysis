#!/bin/bash

set -e

SLOC_FILE="_analysis_/repo-sloc.csv"

path="$1"

# echo "files,language,blank,comment,code"
cloc -q --csv $path | tail +3 > "$path/$SLOC_FILE"
