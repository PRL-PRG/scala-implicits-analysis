#!/usr/bin/env Rscript

library(fs)
library(tidyverse)

args <- commandArgs(trailingOnly=T)
projects_file <- args[1]
file <- args[2]
col_types <- args[3]
output_file <- args[4]

project_ids <- read_lines(projects_file)
files <- path("projects", project_ids, file)

existing_files <- file_exists(files)

df <- map2_dfr(
    project_ids[existing_files],
    files[existing_files],
    ~mutate(read_csv(.y, col_types=col_types), project_id=.x)
)

df <- if (nrow(df) == 0) {
    data_frame(project_id=character())
} else {
    select(df, project_id, everything())
}

write_csv(df, output_file)
