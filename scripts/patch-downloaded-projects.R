#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(fs)
library(withr)

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

input <- args[1]
projects_dir <- args[2]

patch <- read_csv(input)

if (nrow(patch) > 1) {
    for (i in 1:nrow(patch)) {
        dir <- path(projects_dir, patch[[i,1]])
        if (dir_exists(dir)) {
            command <- patch[[i,2]]
            cat('>> Running', command, 'in', dir, '\n')
            with_dir(dir, tryCatch({
                system(command)
                cat('>> Command', command, 'in', dir, 'OK\n')
            }, error=function(e) {
                cat('>> Command', command, 'in', dir, 'FAILED\n')
            }))
        } else {
            cat('>> Dir', dir, 'SKIPPED\n')
        }
    }
}
