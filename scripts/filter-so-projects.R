#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glue))
library(fs)

info <- readr::read_csv(
    "projects-github-info.csv",
    col_types=cols(
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
    )
) %>%
  filter(!is.na(name)) %>%
  mutate(corpus_name=sapply(strsplit(name, "/"), paste0, collapse="--"))

message(glue("github-info: {nrow(info)}"))

forks <- filter(info, fork)
message(glue("forks: {nrow(forks)}\n"))

info <- filter(info, !fork)

same_names <- filter(info, project_id==corpus_name)
message(glue("same names: {nrow(same_names)}\n"))

others <- anti_join(info, same_names, by="name") %>% distinct(name, .keep_all = T)

message(glue("others: {nrow(others)}\n"))

final <- bind_rows(same_names,others)

message(glue("final: {nrow(final)}\n"))

stopifnot(sum(duplicated(final$name)) == 0)
stopifnot(sum(duplicated(final$project_id)) == 0)

sbt_projects <- read_lines("sbt-projects.txt")
message(glue("sbt-projects: {length(sbt_projects)}\n"))

projects <- intersect(final$project_id, sbt_projects)
message(glue("projects: {length(projects)}\n"))

write_lines(projects, "so-projects.txt")
