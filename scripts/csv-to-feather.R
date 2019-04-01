#!/usr/bin/env Rscript

SCRIPTS_DIR <- local({
  options <- commandArgs(trailingOnly = FALSE)
  script <- sub("--file=(.*)", "\\1", grep("--file", options, value=TRUE))
  dir <- dirname(script)
  stopifnot(dir.exists(dir))
  dir
})

library(readr)
library(feather)
library(fs)

args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 1)

input <- args[1]
output <- paste0(tools::file_path_sans_ext(input), ".feather")

message("Converting ", input, " to ", output)

df <- suppressMessages(read_csv(input))
write_feather(df, output)
system2("gzip", c("-f", output))
