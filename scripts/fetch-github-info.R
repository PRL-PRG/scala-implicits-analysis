#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(pbapply)
library(fs)
library(httr)

GH_LIMIT <- 5000

pboptions(type="txt")

get_gh_repo <- function(reponame, content_extraction_fun) {
  result <- tryCatch({
    url <- str_c("https://api.github.com/repos/", reponame, "?client_id=", client_id, "&client_secret=", client_secret)
    res <- GET(url)
    if (res$status_code == 200) {
      cnt <- content(res)
      mutate(content_extraction_fun(cnt), error=NA)
    } else {
      message("Error: ", res$status_code, " for ", reponame)
      tibble(error=str_c("Status ", res$status_code))
    }
  }, error=function(e) {
    tibble(error=str_c("Status ", e$message))
  })
  mutate(result, reponame=reponame)
}

call_github <- function(projects, fun) {
    result <- rep(NA, length(projects))

    while(any(is.na(result))) {
        batch <- which(is.na(result))
        if (length(batch) > GH_LIMIT) {
            batch <- batch[1:GH_LIMIT]
        }
        message("Done ", sum(!is.na(result)), "/", length(result))

        project_batch <- projects[batch]
        start <- Sys.time()
        result[batch] <- pblapply(project_batch, get_gh_repo, content_extraction_fun=fun)
        end <- Sys.time()

        if (any(is.na(result))) {
            duration <- as.numeric(end - start, unit="secs")
            pause <- 3600-duration
            message("Sleeping for ", pause)
            Sys.sleep(max(c(0, pause)))
        }
    }

    result
}

main <- function(projects_file, output_file) {
    finished <- if (file_exists(output_file)) {
        read_csv(
            output_file,
            col_types=cols(
                project_id = col_character(),
                name = col_character(),
                stars = col_double(),
                watchers = col_double(),
                created_at = col_datetime(format = ""),
                updated_at = col_datetime(format = ""),
                pushed_at = col_datetime(format = ""),
                fork = col_logical(),
                archived = col_logical(),
                error = col_logical()
            )
        ) %>% filter(is.na(error) | error == "Status 404")
    } else {
        tibble(
            project_id=character(0),
            name=character(0),
            stars=integer(0),
            watchers=integer(0),
            created_at=as.POSIXct(character(0)),
            updated_at=as.POSIXct(character(0)),
            pushed_at=as.POSIXct(character(0)),
            fork=logical(0),
            archived=logical(0),
            error=character(0)
        )
    }

    projects <- tibble(project_id=readLines(projects_file))
    projects <- mutate(projects, name=str_replace(project_id, "--", "/"))

    missing <- anti_join(projects, finished, by="project_id")
    if (nrow(missing) == 0) {
        message("All ", nrow(projects), "/", nrow(finished), " projects have been fetched")
        q(status=0)
    }

    message("Requested projects: ", nrow(projects), ", done: ", nrow(finished), " missing: ", nrow(missing))

    gh_list <- call_github(missing$name, function(content) {
        tibble(
            name=content$full_name,
            stars=content$stargazers_count,
            watchers=content$watchers,
            created_at=parse_datetime(content$created_at),
            updated_at=parse_datetime(content$updated_at),
            pushed_at=parse_datetime(content$pushed_at),
            fork=content$fork,
            archived=content$archived
        )
    })
    gh_df <- bind_rows(gh_list)
    result <- left_join(missing, gh_df, by="name") %>% select(-name)
    all <- bind_rows(finished, result)

    write_csv(all, output_file)
}

client_id <- Sys.getenv("GH_CLIENT_ID")
if (client_id == "") {
    stop("Missing GH_CLIENT_ID (GitHub OAuth Client ID)")
}

client_secret <- Sys.getenv("GH_CLIENT_SECRET")
if (client_secret == "") {
    stop("Missing GH_CLIENT_SECRET (GitHub OAuth Client Secret)")
}

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

main(args[1], args[2])
