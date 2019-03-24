#!/usr/bin/env Rscript

library(tibble)
library(magrittr)
library(stringr)
library(readr)

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 3)

dir <- args[1]
output <- args[2]
output_sloc <- args[3]

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

size <- system2("du", c("-sb", "."), stdout = TRUE) %>% str_replace("(\\d+).*", "\\1") %>% as.integer()
commit_count <- system2("git", c("rev-list", "--count HEAD", "--"), stdout = TRUE) %>% as.integer()
commit <- system2("git", c("log", "--pretty=format:'%H'", "-n 1"), stdout = TRUE)
commit_dates <- system2("git", c("log", "--date=unix", "--pretty=format:'%cd'"), stdout = TRUE)
commit_date <- as.integer(commit_dates[1])
first_commit_date <- as.integer(commit_dates[length(commit_dates)])

sloc <- tryCatch({
    sloc_src <- system2("cloc", c("--follow-links", "--vcs=git", "-q", "--csv", "."), stdout = TRUE)[-1]
    sloc_src[1] <- "files,language,blank,comment,code"
    sloc <- read_csv(sloc_src, col_types="cciii")
}, error=function(e) {
    message("Unable to count SLOC: ", e$message)
    tibble(
        files=character(0),
        language=character(0),
        blank=integer(0),
        comment=integer(0),
        code=integer(0)
    )
})

for (i in seq_along(systems_def)) {
    if (file.exists(file.path(dir, systems_def[i]))) {
        system <- systems[i]
        break
    }
}

if (!is.na(system) && system == "sbt") {
    tryCatch({
        build_properties <- file.path(dir, "project", "build.properties")

        if (file.exists(build_properties)) {
            lines <- readLines(build_properties)
            pattern <- "^\\s*sbt\\.version\\s*=\\s*([^ ]+)"
            matches <- str_subset(lines, pattern)
            if (length(matches) > 0) {
                sbt_version <- str_replace(matches[length(matches)], pattern, "\\1")
            }
        }
    }, error=function(e) {
        message("Unable to guess SBT version: ", e$message)
    })
}

sloc_scala <- sloc[sloc$language=="Scala", ]
if (nrow(sloc_scala) == 0) {
    sloc_scala <- tibble(files=0, language="Scala", blank=0, comment=0, code=0)
}

df <- tibble(
  build_system=system, 
  sbt_version,
  size,
  commit_count,
  commit,
  commit_date,
  first_commit_date,
  scala_code=sloc_scala$code,
  scala_files=sloc_scala$files
)

write_csv(df, output)
write_csv(sloc, output_sloc)
