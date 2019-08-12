#!/usr/bin/env Rscript

SCRIPTS_DIR <- local({
  options <- commandArgs(trailingOnly = FALSE)
  script <- sub("--file=(.*)", "\\1", grep("--file", options, value=TRUE))
  dir <- dirname(script)
  stopifnot(dir.exists(dir))
  dir
})

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
library(fs)
library(vroom)

source(path(SCRIPTS_DIR, "inc", "paths.R"))
source(path(SCRIPTS_DIR, "inc", "functions.R"))

args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) != 1)

ght_projects_file <- args[1]
stopifnot(file_exists(ght_projects_file))

# 1,"https://api.github.com/repos/tosch/ruote-kit",1,"ruote-kit","RESTful wrapper for ruote workflow engine","Ruby","2009-12-08 11:17:27",2,0,"2019-05-30 16:35:42",84984

ght_projects <- vroom::vroom(
    ght_projects_file,
    delim=",",
    col_names=c("id", "url", "owner_id", "name", "descriptor", "language", "created_at", "forked_from", "deleted", "updated_at", "unused"),
    col_types=list(
        id="i",
        url="c",
        owner_id="i",
        name="c",
        descriptor="c",
        language="c",
        created_at="D",
        forked_from="i",
        deleted="i",
        updated_at="D",
        unused="i"
    ),
    col_select=list(id, url, language, deleted, forked_from),
    trim_ws=TRUE,
    na="\\N"
)

ght_projects2 <- readr::read_delim(
    ght_projects_file,
    delim=",",
    col_names=c("id", "url", "owner_id", "name", "descriptor", "language", "created_at", "forked_from", "deleted", "updated_at", "unused"),
    col_types=list(
        id="i",
        url="c",
        owner_id="i",
        name="c",
        descriptor="c",
        language="c",
        created_at="c",
        forked_from="i",
        deleted="i",
        updated_at="c",
        unused="i"
    ),
    na="\\N",
    escape_double = FALSE,
    trim_ws=TRUE
)

scala_projects <- filter(ght_projects, language == "Scala")
projects <- str_replace(scala_projects$url, "^.*\/(.*)\/(.*)$", "\\1--\\2")

all_projects_file <- get_corpus_file(ALL_PROJECTS_FILE)
if (file_exists(all_projects_file)) {
    file_copy(all_projects_file, str_c(all_projects_file, ".backup"))
}
write_lines(projects, all_projects_file)
