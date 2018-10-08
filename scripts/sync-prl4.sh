#!/bin/sh

set -e

scriptpath="$(cd "$(dirname "$0")" ; pwd -P )"
projectpath=$(dirname $scriptpath)

# if you get rsync: connection unexpectedly closed (8 bytes received so far) [sender]
#            rsync error: error in rsync protocol data stream (code 12) at /BuildRoot/Library/Caches/com.apple.xbs/Sources/rsync/rsync-52.200.1/rsync/io.c(453) [sender=2.6.9]
# run this:
# ssh prl4 mkdir -p .ivy2/local/cz.cvut.fit.prl.scala/sdb-reader_2.12

echo "** Syncing: $projectpath"
rsync -avh --delete --exclude target $projectpath/ prl4:Research/Projects/scala-implicits/sdb-reader/scala
echo "** Syncing: $projectpath"
rsync -avh --delete ~/.ivy2/local/cz.cvut.fit.prl.scala.implicits/ prl4:.ivy2/local/cz.cvut.fit.prl.scala.implicits