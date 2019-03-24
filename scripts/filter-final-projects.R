#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glue))
library(fs)

so_projects <- tibble(project_id=read_lines("so-projects.txt"))

dejavu_project_duplication <- read_csv("dejavu-duplication.csv") %>% semi_join(so_projects, by="project_id")

github_info <- read_csv("projects-github-info.csv") %>% semi_join(so_projects, by="project_id")

repo_metadata <- read_csv("repo-metadata.csv") %>% semi_join(so_projects, by="project_id")

duplicates <- filter(dejavu_project_duplication, duplication==1)

duplicates_info <- left_join(github_info, repo_metadata, by="project_id") %>% semi_join(duplicates, by="project_id")

to_keep <- filter(duplicates_info, scala_code >= 100, stars >= 2) %>%
    group_by(scala_code) %>%
    top_n(1, -as.numeric(updated_at)) %>%
    ungroup() %>%
    .$project_id

projects <- c(
  setdiff(so_projects$project_id, duplicates$project_id),
  to_keep
)

write_lines(projects, "projects.txt")
