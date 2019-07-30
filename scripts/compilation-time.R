#!/usr/bin/env Rscript

SCRIPTS_DIR <- local({
  options <- commandArgs(trailingOnly = FALSE)
  script <- sub("--file=(.*)", "\\1", grep("--file", options, value=TRUE))
  dir <- dirname(script)
  stopifnot(dir.exists(dir))
  dir
})

suppressPackageStartupMessages(library(tidyverse))
library(fs)

source(path(SCRIPTS_DIR, "inc", "paths.R"))

COMPILE_STATUS <- "_analysis_/compile-status.csv"
COMPILE_LOG <- "_analysis_/compile.log"

error <- NA

if (!file_exists(COMPILE_STATUS)) {
    error <- str_c("No ", COMPILE_STATUS)
}

if (!file_exists(COMPILE_LOG)) {
    error <- str_c("No ", COMPILE_LOG)
}

compile_status <- read_csv(COMPILE_STATUS)

if (compile_status$exit_code != 0) {
    error <- str_c("Compilation ", compile_status$exit_code)
}

#[info] #total compile time           : 1 spans, ()556.099ms
TCT_PATTERN <- "^\\[info\\]\\s+#total compile time\\s+: (\\d+) spans, [()]+? ([0-9.]+)ms$"
#[info]   typer                       : 1 spans, ()180.176ms (32.4%)
#[info]   xsbt-api                    : 1 spans, ()107.104ms (19.3%)
#[info] time spent in implicits       : 31 spans, ()61.223ms (34.0%)
#[info] time spent in macroExpand     : 11 spans, ()36.905ms (20.5%)

phase_pattern <- function(phase) {
  str_glue("^\\[info\\]\\s+{phase}\\s+: (\\d+) spans,\\s*[()]*\\s*([0-9.]+)ms.*")
}

PHASES <- c(
    "#total compile time",
    "typer",
    "xsbt-api",
    "xsbt-dependency",
    "xsbt-analyzer",
    "time spent in implicits",
    "time spent in macroExpand"
)

extract_pattern <- function(log, pattern) {
    res <- str_subset(log, pattern)
    if (length(res) > 0) {
        spans <- str_replace_all(res, pattern, "\\1")
        time <- str_replace_all(res, pattern, "\\2")
        list(spans=spans, time=time)
    } else {
        list(spans=NA, time=NA)
    }
}

extract_phase <- function(log, phase) {
    res <- extract_pattern(log, phase_pattern(phase))

    spans <- as.numeric(res$spans)
    time <- as.numeric(res$time)

    tibble(phase=phase, spans=sum(spans), time=sum(time))
}

if (is.na(error)) {
    log <- read_lines(COMPILE_LOG)
    phases <- map_dfr(PHASES, ~extract_phase(log, .))


    r <- extract_pattern(log, "^\\[success\\] (T)otal time: (\\d+) s.*")$time
    if (!any(is.na(r))) {
        time <- sum(as.numeric(r))
        phases <<- add_row(phases, phase="sbt_compile", spans=1, time=time*1000)
    }

    phases <- add_row(phases,
        phase="n_typers",
        spans=length(str_subset(log, "Cumulative statistics at phase typer")),
        time=NA
    )

    write_csv(phases, "_analysis_/compilation-time.csv")
}

