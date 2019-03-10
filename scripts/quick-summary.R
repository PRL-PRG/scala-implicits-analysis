#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(fs)
library(knitr)

status <- read_csv("implicits-status.csv")
stats <- read_csv("implicits-stats.csv")

cat("Implicits summary:\n")
status %>% count(exit_code) %>% kable()

cat("\nStats:\n")
stats %>% 
  mutate(failure=if_else(is.na(failure), 0, 1)) %>% 
  summarise_at(vars(-project_id), sum) %>% 
  gather() %>% 
  kable()

cat("\nDuration:\n")
tribble(
  ~key, ~value, 
  "Mean", mean(status$duration),
  "Median", median(status$duration),
  "Sum", sum(status$duration)
) %>% kable()
