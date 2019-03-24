#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

files_hash_output <- args[1]
duplication_output <- args[2]

files <- read_csv("dejavu/files.csv.h2i", col_names=c("file_id","project_id","path","hash"), col_types="iici")
projects <- read_csv("dejavu/projects.csv", col_names=c("project_id", "local_path", "name"), col_types="icc")
dejavu_files_hash <- left_join(files, projects, by="project_id") %>%
    select(project_id=name, file_id, path, hash) %>%
    mutate(project_id=str_replace(project_id, "/", "--"))

write_csv(dejavu_files_hash, files_hash_output)

dejavu_project_files_count <- count(dejavu_files_hash, project_id) %>% rename(n_files=n)

dejavu_duplicated_hashes <-
    group_by(dejavu_files_hash, hash) %>%
    summarise(n_projects=length(unique(project_id)), n_files=n()) %>%
    filter(n_projects > 1)

dejavu_project_duplicated_files_count <-
    semi_join(dejavu_files_hash, dejavu_duplicated_hashes, by="hash") %>%
    count(project_id) %>%
    rename(n_duplicated_files=n)

dejavu_project_duplication <-
    left_join(dejavu_project_files_count, dejavu_project_duplicated_files_count, by="project_id") %>%
    replace_na(list(n_duplicated_files=0)) %>%
    mutate(duplication=n_duplicated_files/n_files)

write_csv(dejavu_project_duplication, duplication_output)

