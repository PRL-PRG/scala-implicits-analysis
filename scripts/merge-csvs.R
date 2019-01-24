#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 5)

projects_file <- args[1]
projects_dir <- args[2]
file <- args[3]
col_types <- args[4]
output_file <- args[5]

project_ids <- read_lines(projects_file)
files <- path(projects_dir, project_ids, file)

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

message("Merged ", sum(existing_files), "/", length(files), " into ", output_file)
for (file in files[!existing_files]) {
  message("Missing: ", file)
}
