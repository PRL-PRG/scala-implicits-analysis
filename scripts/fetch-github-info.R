#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))
library(pbapply)
library(fs)
library(httr)

GH_LIMIT <- 5000

get_gh_repo <- function(reponame, content_extraction_fun) {
  result <- tryCatch({
    url <- str_c("https://api.github.com/repos/", reponame, "?client_id=", client_id, "&client_secret=", client_secret)
    res <- GET(url)
    if (res$status_code == 200) {
      cnt <- content(res)
      mutate(content_extraction_fun(cnt), error=NA)
    } else {
      data_frame(error=str_c("Status ", res$status_code))
    }
  }, error=function(e) {
    data_frame(error=str_c("Status ", e$message))
  })
  message('Done: ', reponame)
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
        result[batch] <- lapply(project_batch, get_gh_repo, content_extraction_fun=fun)
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
    projects <- data_frame(project_id=readLines(projects_file))
    projects <- mutate(projects, reponame=str_replace(project_id, "--", "/"))

    gh_list <- call_github(projects$reponame, function(content) {
        data_frame(name=content$full_name, content$stargazers_count)
    })
    gh_df <- bind_rows(gh_list)
    result <- left_join(projects, gh_df, by="reponame") %>% select(-reponame)

    write_csv(result, output_file)
}

client_id <- Sys.getenv("GH_CLIENT_ID")
client_secret <- Sys.getenv("GH_CLIENT_SECRET")

stopifnot(nchar(client_id) > 0)
stopifnot(nchar(client_secret) > 0)

args <- commandArgs(trailingOnly=T)
stopifnot(length(args) == 2)

main(args[1], args[2])
