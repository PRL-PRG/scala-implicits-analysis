#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

dir <- args[1]
output <- args[2]

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
system <- NA
sbt_version <- NA

for (i in seq_along(systems_def)) {
    if (file.exists(file.path(dir, systems_def[i]))) {
        system <- systems[i]
        break
    }
}

if (!is.na(system) && system == "sbt") {
    if (file.exists(file.path(dir, "project", "build.properties"))) {
        props <- read.table(
            "project/build.properties",
            header=FALSE,
            sep="=",
            row.names=1,
            strip.white=TRUE,
            na.strings="NA",
            stringsAsFactors=FALSE
        )
        version <- props["sbt.version", 1]
        if (!is.null(version)) {
            sbt_version <- version
        }
    }
}

df <- tibble::tibble(build_system=system, sbt_version=sbt_version)

readr::write_csv(df, output)
