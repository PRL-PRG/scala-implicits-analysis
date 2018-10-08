#!/usr/bin/env Rscript

library(fs)
library(tidyverse)

args <- commandArgs(trailingOnly=T)
logfile <- args[1]

if (!file_exists(logfile)) {
    message("Unable to read file: ", logfile)
    q(status=1)
}

scala_projects <- read_csv("scala-projects.csv")$project
jobs <- read_tsv(logfile) %>% mutate(project=str_replace(Command, "make -C projects/(.*) -f .*", "\\1"))

setwd(path_dir(logfile))

missing <- setdiff(scala_projects, jobs$project)
write_lines(missing, "missing.txt")

done <- filter(jobs, Exitval == 0)$project
write_lines(done, "done.txt")

timeouted <- filter(jobs, Exitval == -1, Signal == 15)$project
write_lines(timeouted, "timeouted.txt")

failed <- filter(jobs, Exitval != 0, Signal != 15)$project
write_lines(failed, "failed.txt")
