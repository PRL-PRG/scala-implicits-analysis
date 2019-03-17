#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(fs)

args <- commandArgs(trailingOnly=TRUE)

stopifnot(length(args) == 1)

corpus_dir <- args[1]

setwd(corpus_dir)

PROJECTS_DIR <- "projects"
ANALYSIS_DIR <- "_analysis_"

PROJECTS_FILE <- "projects.txt"

DEJAVU_H2I <- "dejavu-files-hash-h2i.csv"
GITHUB_INFO <- "projects-github-info.csv"
SCALADEX <- "scaladex.txt"

METADATA_STATUS <- "metadata-status.csv"
METADATA_MODULES <- "metadata-modules.csv"
METADATA_SOURCEPATHS <- "metadata-sourcepaths.csv"
METADATA_LOG <- "metadata.log"

REPO_METADATA <- "repo-metadata.csv"
REPO_SLOC <- "repo-sloc.csv"

COMPILE_STATUS <- "compile-status.csv"
COMPILE_LOG <- "compile.log"

SDB_STATUS <- "semanticdb-status.csv"
SDB_STATS <- "semanticdb-stats.csv"
SDB_LOG <- "semanticdb.log"

IMPLICITS_STATUS <- "implicits-status.csv"
IMPLICITS_STATS <- "implicits-stats.csv"
IMPLICITS_EXCEPTIONS <- "implicits-exceptions.csv"
IMPLICITS_LOG <- "implicits.log"

# this is the result
CORPUS_CSV <- "corpus.csv"

add_prefix <- function(prefix) {
    function(x) str_c(prefix, '_', x)
}

merge_project_csvs <- function(project_ids, files, col_types) {
  stopifnot(length(project_ids) == length(files))

  existing_files <- file_exists(files)

  df <- map2_dfr(
    project_ids[existing_files],
    files[existing_files],
    ~mutate(read_csv(.y, col_types=col_types), project_id=.x)
  )

  if (nrow(df) == 0) {
    tibble(project_id=character())
  } else {
    select(df, project_id, everything())
  }
}

projects_file <- function(project, filename) {
  path(PROJECTS_DIR, project, ANALYSIS_DIR, filename)
}

# load data

project_ids <- read_lines(PROJECTS_FILE)
scaladex_ids <- read_lines(SCALADEX)

projects <- tibble(
    project_id=project_ids,
    origin=str_c("https://github.com/", str_replace(project_id, "--", "/")),
    project_path=path(getwd(), PROJECTS_DIR, project_id),
    scaladex=project_id %in% scaladex_ids
)

metadata_status <- read_csv(METADATA_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer()
))

metadata_modules <- read_csv(METADATA_MODULES, col_types=cols(
  project_id = col_character(),
  module_id = col_character(),
  group_id = col_character(),
  artifact_id = col_character(),
  version = col_character(),
  platform = col_character(),
  commit = col_character(),
  scala_version = col_character(),
  sbt_version = col_character(),
  updated_scala_version = col_character(),
  output_classpath = col_character(),
  output_test_classpath = col_character()
)) %>%
  group_by(project_id) %>%
  summarise(
    modules=n(),
    scala_version=scala_version[1],
    updated_scala_version=updated_scala_version[1]
  )

metadata_sourcepaths <- read_csv(METADATA_SOURCEPATHS, col_types=cols(
  project_id = col_character(),
  module_id = col_character(),
  scope = col_character(),
  managed = col_logical(),
  path = col_character(),
  files = col_integer(),
  language = col_character(),
  blank = col_integer(),
  comment = col_integer(),
  code = col_integer()
))

repo_metadata <- read_csv(REPO_METADATA, col_types=cols(
  project_id = col_character(),
  build_system = col_character(),
  sbt_version = col_character(),
  size = col_double(),
  commit_count = col_double(),
  commit = col_character(),
  commit_date = col_double(),
  first_commit_date = col_double(),
  scala_code = col_double(),
  scala_files = col_double()
))

repo_sloc <- read_csv(REPO_SLOC, col_types=cols(
  project_id = col_character(),
  files = col_integer(),
  language = col_character(),
  blank = col_integer(),
  comment = col_integer(),
  code = col_integer()
))

compile_status <- read_csv(COMPILE_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer(),
  classes = col_integer()
))

sdb_status <- read_csv(SDB_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer(),
  classes = col_integer()
))

sdb_stats <- read_csv(SDB_STATS, col_types=cols(
  project_id = col_character(),
  files = col_integer(),
  occurrences = col_integer(),
  synthetics = col_integer(),
  symbols = col_integer()
))

implicits_status <- read_csv(IMPLICITS_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer()
))

implicits_stats <- read_csv(IMPLICITS_STATS, col_types=cols(
  project_id = col_character(),
  failure = col_character(),
  declarations = col_integer(),
  callsites = col_integer(),
  implicit_declarations = col_integer(),
  implicit_callsites = col_integer(),
  failures = col_integer()
))

github_info <- read_csv(GITHUB_INFO, col_types=cols(
  project_id = col_character(),
  name = col_character(),
  stars = col_integer(),
  error = col_character()
))

dejavu_files_hash <- read_csv(DEJAVU_H2I, col_types=cols(
  project_id = col_character(),
  file_id = col_integer(),
  path = col_character(),
  hash = col_integer()
))

## process

dejavu_project_duplication <- local({

  dejavu_project_files_count <- count(dejavu_files_hash, project_id) %>% rename(n_files=n)

  dejavu_duplicated_hashes <-
      group_by(dejavu_files_hash, hash) %>%
      summarise(n_projects=length(unique(project_id)), n_files=n()) %>%
      filter(n_projects > 1)

  dejavu_project_duplicated_files_count <-
      semi_join(dejavu_files_hash, dejavu_duplicated_hashes, by="hash") %>%
      count(project_id) %>%
      rename(n_duplicated_files=n)

  duplication <-
    left_join(dejavu_project_files_count, dejavu_project_duplicated_files_count, by="project_id") %>%
    replace_na(list(n_duplicated_files=0)) %>%
      mutate(duplication=n_duplicated_files/n_files)

  duplication

})

# we need to keep the paths distinct since for example JS/JVM modules share code
scala_sloc <-
  filter(metadata_sourcepaths, language=="Scala") %>%
  select(project_id, path, files, code) %>%
  group_by(project_id) %>%
  distinct(path, .keep_all=TRUE) %>%
  select(-path) %>%
  summarise_all(sum) %>%
  rename_at(vars(-project_id), add_prefix("metadata_scala"))

repo_other_sloc <-
  repo_sloc %>%
  filter(language != "Scala") %>%
  select(project_id, files, code) %>%
  group_by(project_id) %>%
  summarise_all(sum) %>%
  rename_at(vars(-project_id), add_prefix("repo_other"))

projects <- local({
  joins <- list(
    projects,
    rename_at(metadata_status, vars(-project_id), add_prefix('metadata')),
    scala_sloc,
    metadata_modules,
    rename(repo_metadata, repo_scala_code=scala_code, repo_scala_files=scala_files),
    rename_at(compile_status, vars(-project_id), add_prefix('compile')),
    rename_at(sdb_status, vars(-project_id), add_prefix('semanticdb')),
    rename_at(sdb_stats, vars(-project_id), add_prefix('semanticdb')),
    rename_at(implicits_status, vars(-project_id), add_prefix('implicits')),
    select(
      implicits_stats,
      project_id,
      declarations,
      callsites,
      implicit_declarations,
      implicit_callsites,
      implicit_failure=failure,
      implicit_problems=failures
    ),
    select(github_info, project_id, github_stars=stars),
    select(
      dejavu_project_duplication,
      project_id,
      dejavu_files=n_files,
      dejavu_duplicated_files=n_duplicated_files,
      dejavu_duplication=duplication
    )
  )
  
  df <- joins[[1]]
  for (i in seq(2, length(joins))) {
    df <- left_join(df, joins[[i]], by="project_id")
  }
  df
})

write_csv(projects, CORPUS_CSV)
