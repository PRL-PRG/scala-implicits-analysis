#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 1)

output <- args[1]

files <- read_csv("dejavu/files.csv.h2i", col_names=c("file_id","project_id","path","hash"), col_types="iici")
projects <- read_csv("dejavu/projects.csv", col_names=c("project_id", "local_path", "name"), col_types="icc")
df <- left_join(files, projects, by="project_id") %>%
    select(project_id=name, file_id, path, hash) %>%
    mutate(project_id=str_replace(project_id, "/", "--"))

write_csv(df, output)
