#!/usr/bin/env Rscript

SCRIPTS_DIR <- local({
  options <- commandArgs(trailingOnly = FALSE)
  script <- sub("--file=(.*)", "\\1", grep("--file", options, value=TRUE))
  dir <- dirname(script)
  stopifnot(dir.exists(dir))
  dir
})

suppressPackageStartupMessages(library(tidyverse))
library(fs)

source(path(SCRIPTS_DIR, "inc", "paths.R"))
source(path(SCRIPTS_DIR, "inc", "functions.R"))

projects <- readr::read_csv(
    "repo-metadata.csv",
    col_types=cols(
        project_id = col_character(),
        build_system = col_character(),
        sbt_version = col_character(),
        size_repo = col_double(),
        size = col_double(),
        commit_count = col_double(),
        commit = col_character(),
        commit_date = col_double(),
        first_commit_date = col_double(),
        scala_code = col_double(),
        scala_files = col_double()
    )
)

scala_projects <- 
  projects %>%
  filter(scala_code > 0)

compatible_sbt_projects <- 
  scala_projects %>%
  filter(build_system=="sbt", is_compatible_sbt_version(sbt_version))

writeLines(compatible_sbt_projects$project_id, "sbt-projects.txt")
