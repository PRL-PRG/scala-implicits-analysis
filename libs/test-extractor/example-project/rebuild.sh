#!/bin/sh

sbt clean metadata semanticdb
amm ../../../scripts/merge-semanticdbs.sc
