#!/bin/sh -x

VERSIONS="0.13.10
0.13.11
0.13.12
0.13.13
0.13.14
0.13.15
0.13.16
0.13.17
0.13.18
1.2.8
1.2.7
1.2.6
1.2.4
1.2.3
1.2.1
1.2.0
1.1.6
1.1.5
1.1.4
1.1.2
1.1.1
1.1.0
1.0.4
1.0.3
1.0.2
1.0.1
1.0.0
"

for version in $VERSIONS; do
    sbt -sbt-version "$version" -sbt-boot cache/sbt/boot -sbt-dir cache/sbt -ivy cache/ivy 'show scalaVersion'
done
