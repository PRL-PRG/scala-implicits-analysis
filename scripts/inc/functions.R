library(dplyr)
library(stringr)

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

parse_sbt_version <- Vectorize(function(version) {
  if (is.na(version)) return(NA)
  if (nchar(version) < 5) return(NA)
  
  version <- str_replace_all(version, "-.*", "")
  
  parsed <- suppressWarnings(as.integer(str_split(version, "\\.")[[1]]))
  
  if (any(is.na(parsed))) return(NA)
  if (length(parsed) != 3) return(NA)
  
  return(parsed)
}, USE.NAMES = FALSE)

format_size <- Vectorize(function(x) {
  if (is.na(x)) return(x)
  
  units <- c("B", "kB", "MB", "GB", "TB", "PB")
  
  fmt <- function(x, i=1) {
    xx <- x / 1024
    if (abs(xx) > 1 && i < length(units)) {
      fmt(xx, i+1)
    } else {
      sprintf("%.2f %s", x, units[i])
    }
  }
  
  fmt(x)
})

fmt <- function(x, prefix="", suffix="", ...) {
  if (is.na(x)) {
    NA
  } else if (is.null(x)) {
    NULL
  } else {
    v <- .fmt(x, ...)
    str_c(prefix, v, suffix)
  }
}

.fmt <- function(x) {
  UseMethod(".fmt", x)  
}

.fmt.default <- function(x) {
  x
}

.fmt.integer <- function(x) {
  format(x, big.mark=",", small.mark=".")
}

.fmt.double <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  if (floor) x <- floor(x)
  if (ceiling) x <- ceiling(x)
  format(round(x, digits), big.mark=",")
}

percent <- function(x, ...) my_format(x, s="%", ...)

round_down_nice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * (nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]-1]])
}

my_stat_fmt <- function(x) {
  s <- sd(x, na.rm = T)
  m <- median(x, na.rm = T)
  str_c('(s=', fmt(s, floor=s > 1e3), ' m=', fmt(m, floor=m > 1e3), ')')
}

add_nrow <- function(key, df) {
  add_num(key, nrow(df))
}

add_num <- function(key, v) {
  stat <- function(fun) if (length(v) > 1) fun(v, na.rm=TRUE) else NA
  
  tibble(
    key, 
    sum=sum(v, na.rm = TRUE),
    n=length(v),
    mean=stat(mean), 
    median=stat(median), 
    sd=stat(sd),
    min=stat(min),
    max=stat(max)
  ) %>%
    mutate_at(vars(-key), fmt)
}

make_stats <- function(...) {
  all <- list(...)
  df <- bind_rows(all)
  if (all(df$n == 1)) {
    select(df, key, value=sum)
  } else {
    df
  }
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
    path(params$base_dir, "all-projects", file)
  }
  
  rel <- path_rel(url, params$base_dir)
  url <- str_c(params$base_url, "/", basename(params$base_dir), "/", rel)
  
  make_link(url, text)
}, USE.NAMES = FALSE)

make_link <- Vectorize(function(url, text) {
  glue('<a href="{url}">{text}</a>')
}, USE.NAMES = FALSE)

my_table <- function(...) {
  kable(...) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}

my_datatable <- function(df, page_size=20, ...) {
  if (nrow(df) < page_size) {
    datatable(df, options=list(paging=FALSE, searching=FALSE, info=FALSE), ...)
  } else {
    datatable(df, options=list(paging=TRUE, searching=TRUE, info=TRUE, pageLength=page_size), ...)
  }
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
        exit_code > 130 ~ "timed_out"
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
  df %>%
    rowwise() %>%
    do(guess_failure_cause(.)) %>%
    ungroup()
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

