#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(fs)
library(knitr)

# TODO: this should go into functions.R
phase_status <- function(phase) {
  read_csv(str_c(phase, "-status.csv")) %>%
    count(exit_code) %>%
    transmute(
      phase=phase,
      status=case_when(
        exit_code ==  0 ~ "success",
        exit_code == -1 ~ "not_run",
        exit_code ==  1 ~ "failed",
        exit_code >= 130 ~ "timed_out"
      ),
      count=n
    ) %>%
    group_by(phase, status) %>%
    summarise(count=sum(count)) %>%
    ungroup() %>%
    spread(status, count, fill=0) %>%
    mutate(all = rowSums(.[-1])) %>%
    select(phase, all, everything())
}

phases <- c("metadata", "compile", "semanticdb", "implicits")
status <- map_dfr(phases, ~phase_status(.)) %>% mutate_at(vars(-phase), function(x) coalesce(x, 0))
status %>% kable()

stats <- read_csv("implicits-stats.csv")

cat("\nStats:\n")
stats %>% 
  mutate(failure=if_else(is.na(failure), 0, 1)) %>% 
  summarise_at(vars(-project_id), sum) %>% 
  gather() %>% 
  kable()
