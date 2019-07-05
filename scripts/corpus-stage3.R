#!/usr/bin/env Rscript

SCRIPTS_DIR <- local({
  options <- commandArgs(trailingOnly = FALSE)
  script <- sub("--file=(.*)", "\\1", grep("--file", options, value=TRUE))
  dir <- dirname(script)
  stopifnot(dir.exists(dir))
  dir
})

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
library(fs)
library(pbapply)

pboptions(type="txt")

source(path(SCRIPTS_DIR, "inc", "paths.R"))
source(path(SCRIPTS_DIR, "inc", "functions.R"))

# load data

final_projects <- tibble(project_id=read_lines(PROJECTS_FILE))

projects <-
  read_csv(CORPUS_STAGE1, col_types=cols(
    project_id = col_character(),
    compatible = col_logical(),
    origin = col_character(),
    build_system = col_character(),
    sbt_version = col_character(),
    size_repo = col_double(),
    size = col_double(),
    commit_count = col_integer(),
    commit = col_character(),
    commit_date = col_datetime(format = ""),
    first_commit_date = col_datetime(format = ""),
    scala_code = col_integer(),
    scala_files = col_integer(),
    dejavu_n_files = col_integer(),
    dejavu_n_duplicated_files = col_integer(),
    dejavu_duplication = col_double(),
    gh_name = col_character(),
    gh_stars = col_integer(),
    gh_watchers = col_integer(),
    gh_created_at = col_datetime(format = ""),
    gh_updated_at = col_datetime(format = ""),
    gh_pushed_at = col_datetime(format = ""),
    gh_fork = col_logical(),
    gh_archived = col_logical(),
    scaladex = col_logical()
  )) %>%
  semi_join(final_projects, by="project_id")

stopifnot(setequal(final_projects$project_id, projects$project_id))

metadata_status <- read_csv(GLOBAL_METADATA_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer()
))

metadata_modules <- read_csv(GLOBAL_METADATA_MODULES, col_types=cols(
  project_id = col_character(),
  module_id = col_character(),
  group_id = col_character(),
  artifact_id = col_character(),
  version = col_character(),
  platform = col_character(),
  commit = col_character(),
  scala_version = col_character(),
  sbt_version = col_character(),
  updated_scala_version = col_character(),
  output_classpath = col_character(),
  output_test_classpath = col_character()
))

metadata_sourcepaths <- read_csv(GLOBAL_METADATA_SOURCEPATHS, col_types=cols(
  project_id = col_character(),
  module_id = col_character(),
  scope = col_character(),
  managed = col_logical(),
  path = col_character(),
  files = col_integer(),
  language = col_character(),
  blank = col_integer(),
  comment = col_integer(),
  code = col_integer()
))

compile_status <- read_csv(GLOBAL_COMPILE_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer(),
  classes = col_integer()
))

semanticdb_status <- read_csv(GLOBAL_SEMANTICDB_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer(),
  classes = col_integer()
))

semanticdb_stats <- read_csv(GLOBAL_SEMANTICDB_STATS, col_types=cols(
  project_id = col_character(),
  files = col_integer(),
  occurrences = col_integer(),
  synthetics = col_integer(),
  symbols = col_integer()
))

implicits_status <- read_csv(GLOBAL_IMPLICITS_STATUS, col_types=cols(
  project_id = col_character(),
  exit_code = col_integer(),
  duration = col_integer()
))

implicits_stats <- read_csv(GLOBAL_IMPLICITS_STATS, col_types=cols(
  project_id = col_character(),
  failure = col_character(),
  declarations = col_integer(),
  implicit_declarations = col_integer(),
  implicit_local_declarations = col_integer(),
  callsites = col_integer(),
  test_callsites = col_integer(),
  implicit_callsites = col_integer(),
  failures = col_integer()
)) %>% rename(
  implicit_extraction_errors=failures,
  implicit_failure=failure
)

## process

# we need to keep the paths distinct since for example JS/JVM modules share code
scala_sloc <-
  metadata_sourcepaths %>%
  filter(language=="Scala") %>%
  select(project_id, path, scope, code, managed) %>%
  replace_na(list(code=0)) %>%
  mutate(managed=if_else(managed, "managed", "unmanaged")) %>%
  group_by(project_id, scope, managed) %>%
  distinct(path, .keep_all=TRUE) %>%
  summarise(code=sum(code)) %>%
  unite(temp, scope, managed) %>%
  spread(temp, code)

# it is possible that there will not be any tests
if (!("test_managed" %in% names(scala_sloc))) {
  scala_sloc <- mutate(scala_sloc, test_managed=0)
}
if (!("test_unmanaged" %in% names(scala_sloc))) {
  scala_sloc <- mutate(scala_sloc, test_unmanaged=0)
}

scala_sloc <-
  scala_sloc %>%
  transmute(
      code_test_managed=replace_na(test_managed, 0),
      code_test=code_test_managed + replace_na(test_unmanaged, 0),
      code_compile_managed=replace_na(compile_managed, 0),
      code_compile=code_compile_managed + replace_na(compile_unmanaged, 0),
      code=code_test + code_compile
  ) %>%
  ungroup()

modules <-
  metadata_modules %>%
  group_by(project_id) %>%
  select(-commit) %>%
  summarise(
    modules=n(),
    scala_version=scala_version[1],
    updated_scala_version=updated_scala_version[1]
  )

projects <- local({
  joins <- list(
    projects,
    rename_at(metadata_status, vars(-project_id), add_prefix('metadata')),
    rename_at(scala_sloc, vars(-project_id), add_prefix("metadata_scala")),
    modules,
    rename_at(compile_status, vars(-project_id), add_prefix('compile')),
    rename_at(semanticdb_status, vars(-project_id), add_prefix('semanticdb')),
    rename_at(semanticdb_stats, vars(-project_id), add_prefix('semanticdb')),
    rename_at(implicits_status, vars(-project_id), add_prefix('implicits')),
    implicits_stats
  )

  df <- joins[[1]]
  for (i in seq(2, length(joins))) {
    df <- left_join(df, joins[[i]], by="project_id")
  }
  df
}) %>%
  select(-compatible)

message("Loading phases errors")

for (phase in c("metadata", "compile", "semanticdb")) {

  message("Phase: ", phase)

  phase_problems <-
    filter(projects, (!!sym(str_c(phase, "_exit_code")))==1) %>%
    mutate(log_file=projects_file(project_id, str_c(phase, ".log"))) %>%
    phase_failure_cause() %>%
    group_by(project_id) %>%
    summarise(cause=cause[1], detail=detail[1]) %>%
    ungroup() %>%
    select(
      project_id,
      !!sym(str_c(phase, "_failure")):=cause,
      !!sym(str_c(phase, "_failure_detail")):=detail
    )

  projects <-
    left_join(
      projects,
      phase_problems,
      by="project_id"
    )

  rm(phase_problems)
}

write_csv(projects, CORPUS_STAGE3)
