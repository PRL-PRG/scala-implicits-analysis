#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

projects_file <- args[1]
output_file <- args[2]

projects <- read_lines(projects_file)

df <- data_frame(
    id=1:length(projects),
    url=str_c("\"https://api.github.com/repos", str_replace(projects, "--", "/"),"\""),
    owner_id=1,
    name=projects,
    description="\"description\"",
    language="\"Scala\"",
    created_at="\"2018-02-01 9:00:00\"",
    forked_from='\\N',
    deleted=0,
    updated_at="\"2018-02-01 9:00:00\"",
    unknown='\\N'
)

write_csv(df, output_file, col_names=F)
