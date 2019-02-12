#!/bin/sh

[ -d .ivy ] || ln -s ~/.ivy2 .ivy
sbt -ivy .ivy clean metadata semanticdb
amm ../../../scripts/merge-semanticdbs.sc
