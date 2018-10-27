#!/bin/sh

set -e

scriptpath="$(cd "$(dirname "$0")" ; pwd -P )"
projectpath=$(dirname $scriptpath)

# if you get rsync: connection unexpectedly closed (8 bytes received so far) [sender]
#            rsync error: error in rsync protocol data stream (code 12) at /BuildRoot/Library/Caches/com.apple.xbs/Sources/rsync/rsync-52.200.1/rsync/io.c(453) [sender=2.6.9]
# run this:
# ssh prl4 mkdir -p .ivy2/local/cz.cvut.fit.prl.scala/sdb-reader_2.12

echo "** Syncing: $projectpath"
rsync \
    -avh \
    --exclude "logs" \
    --exclude ".ivy" \
    --exclude "corpora" \
    --exclude "projects*.txt" \
    --exclude "scala-projects*.csv" \
    --exclude "scripts/target" \
    --exclude "sbt-plugings/target" \
    --exclude "libs/target" \
    --exclude "libs/project/target" \
    --exclude "libs/project/project/target" \
    $projectpath/ pokhara:Research/Projects/scala-corpus


