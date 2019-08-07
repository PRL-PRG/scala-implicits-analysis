library(assertthat)
library(dplyr)
library(DT)
library(fs)
library(fst)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# reads a file written by write_fst, converts it to a tibble
read_data <- function(path) {
  read_fst(path) %>% 
    as_tibble()
}

# for the final corpus, we only consider projects that have some Scala code and for which the implicit extractor does run successfully
filter_final_corpus <- function(corpus) {
  filter(corpus, implicits_exit_code==0, metadata_scala_code > 0)
}

parse_sbt_version <- Vectorize(function(version) {
  if (is.na(version)) return(NA)
  if (nchar(version) < 5) return(NA)

  version <- str_replace_all(version, "-.*", "")

  parsed <- suppressWarnings(as.integer(str_split(version, "\\.")[[1]]))

  if (any(is.na(parsed))) return(NA)
  if (length(parsed) != 3) return(NA)

  return(parsed)
}, USE.NAMES = FALSE, SIMPLIFY=FALSE)

is_compatible_sbt_version <- function(sbt_version) {
  compatible <- function(version) {
    all(version >= c(1,  0, 0)) | all(version >= c(0, 13, 5))
  }

  sbt_version_parsed <- parse_sbt_version(sbt_version)

  map_lgl(sbt_version_parsed, ~compatible(.))
}

merge_project_csvs <- function(project_ids, files, col_types) {
  stopifnot(length(project_ids) == length(files))

  existing_files <- file_exists(files)

  df <- map2_dfr(
    project_ids[existing_files],
    files[existing_files],
    ~mutate(read_csv(.y, col_types=col_types), project_id=.x)
  )

  if (nrow(df) == 0) {
    tibble(project_id=character())
  } else {
    select(df, project_id, everything())
  }
}

add_prefix <- function(prefix) {
  function(x) str_c(prefix, '_', x)
}

is_outlier_min <- function(x) quantile(x, 0.25) - 1.5 * IQR(x)

is_outlier_max <- function(x) quantile(x, 0.75) + 1.5 * IQR(x)

is_outlier <- function(x) {
  (x < is_outlier_min(x)) | (x > is_outlier_max(x))
}

make_corpus_link <- Vectorize(function(file, text=basename(file)) {
  url <- if (startsWith(file, "/")) {
    file
  } else {
    path(corpus_dir, "all-projects", file)
  }

  rel <- path_rel(url, corpus_dir)
  url <- str_c(corpus_dir, "/", basename(corpus_dir), "/", rel)

  make_link(url, text)
}, USE.NAMES = FALSE)

make_link <- Vectorize(function(url, text) {
  glue('<a href="{url}">{text}</a>')
}, USE.NAMES = FALSE)

overview_table <- function(...) {
  overview(...) %>%
    my_datatable(page_size=Inf)
}

my_datatable <- function(df, page_size=20, round=TRUE, ...) {
  options <- if (nrow(df) < page_size) {
    list(paging=FALSE, searching=FALSE, info=FALSE)
  } else {
    list(paging=TRUE, searching=TRUE, info=TRUE, pageLength=page_size)
  }

  table <- datatable(df, options=options, ...)
  if (round) {
    table <- formatRound(table, sapply(df, is.numeric), 2)
  }
  table
}

phase_status <- function(phase, df) {
  df %>%
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

str_subset_multiple <- function(string, patterns) {
  for (p in patterns) {
    m <- str_subset(string, p)
    if (length(m)) return(list(pattern=p, matches=m))
  }

  list(pattern=character(0), matches=character(0))
}

guess_failure_cause <- function(df, tail_lines=20) {
  detect_missing_dependencies <- function(lines) {
    patterns <- c(
      "\\[error\\] \\([^)]+\\) sbt.ResolveException: unresolved dependency: ([^:]+):.*",
      "\\[error\\] \\([^)]+\\) sbt.librarymanagement.ResolveException: unresolved dependency: ([^:]+):.*"
    )
    r <- str_subset_multiple(lines, patterns)
    if (length(r$matches) > 0) {
      str_replace_all(r$matches, r$pattern, "\\1")
    } else {
      idx <- str_which(lines, fixed("coursier.ResolutionException"))
      if (length(idx) > 0) {
        lines[idx[1]+1] %>%
          str_replace_all(fixed("[error]"), "") %>%
          trimws("both")
      } else {
        character(0)
      }
    }
  }

  if (!file_exists(df$log_file)) {
    return(tibble(project_id=df$project_id, log_file=df$log_file, cause="log-file-does-not-exist", detail=NA))
  }

  causes <- list(
    `sbt-class-not-found`=function(lines) {
      pattern <- "\\[error\\] java.lang.ClassNotFoundException: \\$.*"
      str_subset(lines, pattern)
    },
    `missing-dependencies`=detect_missing_dependencies,
    `out-of-memory`=function(lines) {
      patterns <- c("Error during sbt execution: java.lang.OutOfMemoryError", "java.lang.OutOfMemoryError")
      str_subset_multiple(lines, patterns)$matches
    },
    `compilation-failed`=function(lines) {
      pattern <- "\\[error\\] \\([^)]+\\) Compilation failed"
      str_subset(lines, pattern)
    },
    `java-exception`=function(lines) {
      pattern <- "\\[error\\] \\([^)]+\\) (.*Exception.*)"
      matches <- str_subset(lines, pattern)
      str_replace_all(matches, pattern, "\\1")
    },
    `java-error`=function(lines) {
      pattern <- "\\[error\\] \\([^)]+\\) (.*Error.*)"
      matches <- str_subset(lines, pattern)
      str_replace_all(matches, pattern, "\\1")
    },
    `project-loading-failed`=function(lines) {
      pattern <- "^Project loading failed:"
      str_subset(lines, pattern)
    },
    `unknown-metadata-command`=function(lines) {
      pattern <- "\\[error\\] Not a valid command: metadata"
      str_subset(lines, pattern)
    },
    `unknown-semanticdb-command`=function(lines) {
      pattern <- "\\[error\\] Not a valid command: semanticdb"
      str_subset(lines, pattern)
    }
  )

  lines <- tail(read_lines(df$log_file), tail_lines)

  for (cause in names(causes)) {
    cause_fun <- causes[[cause]]
    res <- cause_fun(lines)
    if (length(res) > 0) {
      res <- res[1]
      return(tibble(project_id=df$project_id, log_file=df$log_file, cause=cause, detail=res))
    }
  }

  last_error <- str_subset(lines, "\\[error\\]")
  last_error <- if (length(last_error) > 0) {
    str_c(last_error, collapse="\n")
  } else {
    NA
  }

  tibble(project_id=df$project_id, log_file=df$log_file, cause="unknown", detail=last_error)
}

phase_failure_cause <- function(df) {
  if (nrow(df) > 0) {
    df %>%
      rowwise() %>%
      do(guess_failure_cause(.)) %>%
      ungroup()
  } else {
    tibble(project_id=character(), log_file=character(), cause=character(), detail=character())
  }
}

phase_failure_cause_from_status <- function(status, log_filename) {
  filter(status, exit_code==1) %>%
    select(project_id) %>%
    mutate(
      log_file=path("projects", project_id, "_analysis_", log_filename)
    ) %>%
    rowwise() %>%
    do(guess_failure_cause(.)) %>%
    ungroup()
}

remove_version_from_module_id <- function(df) {
  mutate(df, module_id=str_replace(module_id, "([^:]+)::([^:]+):([^:]+):[^:]+:([^:]+)","\\1::\\2:\\3:\\4"))
}

expand_scope <- function(df) {
  df %>%
    mutate(
      is_in_main=str_detect(location_scope, "compile"),
      is_in_test=str_detect(location_scope, "test")
    )
}

expand_location <- function(df) {
  df %>%
    mutate(
      #is_transitive=str_detect(location_scope, "transitive"),
      # this is to fix scala--scala which will define all as transitive, yet thanks to groupId and artifactId they will
      # appear as part of the project
      #is_external=is_transitive|str_detect(location_scope, "dependency"),
      is_external=str_detect(location_scope, "dependency"),
      is_local=!is_external,
      is_managed=str_detect(location_scope, "managed"),

      is_external_test=is_external & is_in_test,
      is_local_test=is_local & is_in_test,
      is_managed_test=is_managed & is_in_test
    )
}

expand_access_info <- function(df) {
  df %>%
    mutate(
      is_access_specified=access!="NOT_SPECIFIED",
      is_public=access=="PUBLIC",
      is_private=startsWith(access, "PRIVATE"),
      is_protected=startsWith(access, "PROTECTED")
    ) %>%
    select(-access)
}

is_from_scala <- function(df) {
  df %>%
    mutate(
      is_from_scala=is_external & startsWith(def_group_id, "org.scala-lang"),
      is_from_scala_test=is_from_scala & is_in_test
    )
}

# stage 2 projects thresholds
s2_min_commit_count <- 2
s2_min_lifespan_months <- 2
s2_max_duplication1 <- .75
s2_min_gh_stars1 <- 5
s2_max_duplication2 <- .8
s2_min_gh_stars2 <- 500

filter_stage_2_projects <- function(projects) {
  filter(
    projects, 
    compatible,     
    commit_count >= s2_min_commit_count,
    gh_pushed_at-gh_created_at >= months(s2_min_lifespan_months),
    scaladex | gh_stars >= s2_min_gh_stars2 | dejavu_duplication < s2_max_duplication2,
    scaladex | gh_stars >= s2_min_gh_stars1 | dejavu_duplication < s2_max_duplication1
  )
}

overview_projects <- function(name, df, code_column=scala_code) {
  code_column <- enquo(code_column)
  summary <- 
    summarise(
      df, 
      `projects`=n(),
      `projects code`=sum(!!code_column, na.rm=TRUE),
      `projects stars`=sum(gh_stars, na.rm=TRUE),
      `projects commits`=sum(commit_count, na.rm=TRUE)
    ) %>%
    mutate_all(fmt)
  
  colnames(summary) <- str_c(name, " ",colnames(summary))
  summary %>% gather("name", "value")
}
