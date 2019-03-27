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

source(path(SCRIPTS_DIR, "inc", "paths.R"))
source(path(SCRIPTS_DIR, "inc", "functions.R"))

threshold_duplication<- .75
threshold_lifespan_months <- 3
threshold_gh_stars <- 5
threshold_commit_count <- 2

project_status <-local({
  all_projects <- tibble(project_id=readLines(ALL_PROJECTS_FILE))
  sbt_projects <- tibble(project_id=readLines(SBT_PROJECTS_FILE), compatible=TRUE)

  all_projects %>%
    left_join(sbt_projects, by="project_id") %>%
    mutate(
      compatible=if_else(is.na(compatible), FALSE, TRUE),
      origin=str_c("https://github.com/", str_replace(project_id, "--", "/"))
    )
})

dejavu_project_duplication <- read_csv(DEJAVU_DUPLICATION, col_types=cols(
  project_id = col_character(),
  n_files = col_double(),
  n_duplicated_files = col_double(),
  duplication = col_double()
)) %>%
  # FIX dejavu bug which escapes the _ from some reason
  mutate(project_id=str_replace_all(project_id, "\\\\_", "_")) %>%
  rename_at(vars(-project_id), add_prefix("dejavu"))

github_project_info <- read_csv(GITHUB_INFO, col_types=cols(
  project_id = col_character(),
  name = col_character(),
  stars = col_double(),
  watchers = col_double(),
  created_at = col_datetime(format = ""),
  updated_at = col_datetime(format = ""),
  pushed_at = col_datetime(format = ""),
  fork = col_logical(),
  archived = col_logical(),
  error = col_character()
)) %>%
  rename_at(vars(-project_id), add_prefix("gh"))

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
)) %>%
  mutate(
    commit_date=as_datetime(commit_date),
    first_commit_date=as_datetime(first_commit_date)
  )

scaladex <- tolower(read_lines(SCALADEX))

message("All projects: ", nrow(project_status))
message("GitHub info: ", nrow(github_project_info))

corpus <-
  project_status %>%
  left_join(repo_metadata, by="project_id") %>%
  left_join(dejavu_project_duplication, by="project_id") %>%
  left_join(github_project_info, by="project_id") %>%
  # exclude projects for which we do not have any data from github
  # there should be non, but just in case
  filter(is.na(gh_error)) %>%
  select(-gh_error) %>%
  # the reason we cannot simply join is that we do not have normalized project_id (TODO: cf #53)
  mutate(scaladex=tolower(project_id) %in% scaladex)

message("Corpus: ", nrow(corpus))
message("GitHub info empty: ", nrow(filter(corpus, is.na(gh_stars))))
message("Dejavu info empty: ", nrow(filter(corpus, compatible, is.na(dejavu_duplication))))

write_csv(corpus, CORPUS_STAGE1)

stage2_projects <- 
  filter(corpus, 
         compatible,
         commit_count >= threshold_commit_count,
         gh_pushed_at-gh_created_at > months(threshold_lifespan_months),
         dejavu_duplication < threshold_duplication | gh_stars >= threshold_gh_stars
  )

message("Thresholds:")
message("- commits: ", threshold_commit_count)
message("- duplication: ", threshold_duplication)
message("- lifespan:", threshold_lifespan_months)
message("- stars: ", threshold_gh_stars)

message("Filtered projects: ", nrow(stage2_projects))

write_lines(stage2_projects, PROJECTS_FILE)