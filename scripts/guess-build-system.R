#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 1)

dir <- args[1]

systems_def <- c(
    "sbt"="build.sbt",
    "sbt"="project/build.properties",
    "maven"="pom.xml",
    "mill"="build.sc",
    "graddle"="build.gradle",
    "cbt"="build/build.scala",
    "maven-scala"="pom.scala",
    "maven-groovy"="pom.groovy",
    "maven-yml"="pom.yml",
    "maven-clojure"="pom.clojure",
    "maven-ruby"="pom.ruby",
    "maven-java"="pom.java",
    "make"="Makefile"
)

systems <- names(systems_def)
system <- "NA"

for (i in seq_along(systems_def)) {
    if (file.exists(file.path(dir, systems_def[i]))) {
        system <- systems[i]
        break
    }
}

cat(paste0(system, "\n"))
