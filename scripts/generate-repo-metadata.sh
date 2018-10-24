#!/bin/bash

set -e

METADATA_FILE="_analysis_/repo-metadata.csv"

path="$1"
git="git --git-dir=$path/.git"

project_id=$($git remote get-url origin | sed 's|http[s]\?://github.com/\(.*\)/\(.*\)\(.git\)\?|\1--\2|')
commit_count=$($git rev-list --count HEAD)
commit=$($git log --pretty=format:'%H' -n 1)
commit_date=$($git log --date=unix --pretty=format:'%cd' -n 1)
first_commit_date=$($git log --reverse --date=unix --pretty=format:'%cd' | sed -n 1p)
size=$(du -sb $path | awk '{print $1}')

# echo "project_id,commit_count,commit,first_commit_date,commit_date,size"
echo "$project_id,$commit_count,$commit,$first_commit_date,$commit_date,$size" > "$path/$METADATA_FILE"
