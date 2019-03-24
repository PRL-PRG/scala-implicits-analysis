#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glue))
library(fs)

versions <- list(
    c(0, 13, 5),
    c(1,  0, 0)
)

compatible_version <- Vectorize(function(version) {
    if (is.na(version)) return(FALSE)
    if (nchar(version) < 5) return(FALSE)

    version <- str_replace_all(version, "-.*", "")

    parsed <- suppressWarnings(as.integer(str_split(version, "\\.")[[1]]))

    if (any(is.na(parsed))) return(FALSE)
    if (length(parsed) != 3) return(FALSE)

    tryCatch({
    for (v in versions) {
        if (all(parsed >= v)) return(TRUE)
    }
    },warning=function(e) {
        message(version, parsed, e$message)
    })

    return(FALSE)
})

repo <- readr::read_csv("repo-metadata.csv")

compatible <- filter(repo, scala_code > 0, compatible_version(sbt_version))

writeLines(compatible$project_id, "sbt-projects.txt")

