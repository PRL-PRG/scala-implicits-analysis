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

all_projects <- tibble(project_id=read_lines(ALL_PROJECTS_FILE))
github_info <- read_csv(PROJECTS_GH_INFO)

projects <- 
  semi_join(github_info, all_projects, by="project_id") %>%
  filter(is.na(error))

message(ALL_PROJECTS_FILE, ": ", nrow(all_projects))
message(PROJECTS_GH_INFO, ": ", nrow(github_info))
message("projects: ", nrow(projects))

unique_projects <-
  projects %>%
  filter(!fork) %>%
  mutate(
    norm_name=tolower(name), 
    name_from_project_id=tolower(str_replace(project_id, "--", "/"))
  ) %>%
  group_by(name) %>%
  filter(norm_name==name_from_project_id) %>%
  ungroup() %>%
  select(-norm_name, -name_from_project_id)

message("non-forked, single name projects: ", nrow(unique_projects))

file_copy(ALL_PROJECTS_FILE, ALL_PROJECTS_FILE_ORIG)

write_lines(unique_projects$project_id, ALL_PROJECTS_FILE)
