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
source(path(SCRIPTS_DIR, "inc", "latextags.R"))


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
  size_repo = col_double(),
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

message("\n")
message("====================")
message("STAGE 1")
message("====================")

stage1_projects <-
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

overview(
  r("All projects: ",      project_status),
  r("GitHub info: ",       github_project_info),
  r("GitHub info empty: ", filter(stage1_projects, is.na(gh_stars))),
  r("Dejavu info empty: ", filter(stage1_projects, compatible, is.na(dejavu_duplication))),
  
  overview_projects("Stage 1", stage1_projects)
) %>% knitr::kable(format = "markdown")

write_csv(stage1_projects, CORPUS_STAGE1)

stage2_projects <-
  stage1_projects %>%
  filter_stage_2_projects()

message("\n\n")
message("====================")
message("STAGE 2")
message("====================")
overview(
  r("crit min commits",         s2_min_commit_count),
  r("crit min lifespan",        s2_min_lifespan_months),
  r("crit max duplication one", percent(s2_max_duplication1)),
  r("crit min stars one",       s2_min_gh_stars1),
  r("crit max duplication two", percent(s2_max_duplication2)),
  r("crit min stars two",       s2_min_gh_stars2),
  overview_projects("stage 2", stage2_projects)
) %>% knitr::kable(format = "markdown")

write_lines(stage2_projects$project_id, PROJECTS_FILE)
