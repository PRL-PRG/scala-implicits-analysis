library(purrr)
library(tidyverse)
library(fs)
library(httr)

get_gh_stars <- function(reponame) {
  stars <- tryCatch({
    url <- str_c("https://api.github.com/repos/", reponame, "?client_id=", client_id, "&client_secret=", client_secret)
    res <- GET(url)
    if (res$status_code == 200) {
      cnt <- content(res)
      as.integer(cnt$stargazers_count)
    } else {
      -1L
    }
  }, error=function(e) {
    message("Unable to get stargazers for ", reponame)
    -1L
  })
  
  message(reponame, ": ", stars)
  stars
}

client_id <- read_lines("gh-client-id.txt")
client_secret <- read_lines("gh-client-secret.txt")
analysis_dir <- "/mnt/nvme1/scala/ghtorrent"

projects_scala <- read_csv(path(analysis_dir, "scala-projects.csv"))
projects_scala <- mutate(projects_scala, gh_project=str_replace(project, "--", "/"))
stars <- as.integer(rep(NA, nrow(projects_scala)))

while(any(is.na(stars))) {
  batch <- which(is.na(stars))
  if (length(batch) > 5000) {
    batch <- batch[1:5000]
  }
  repos <- projects_scala$gh_project[batch]
  
  message("Done ", sum(!is.na(stars)), "/", length(stars))
  start <- Sys.time()
  stars[batch] <- map_int(repos, ~get_gh_stars(.))
  end <- Sys.time()
  
  if (any(is.na(stars))) {
    duration <- as.numeric(end - start, unit="secs")
    pause <- 3600-duration
    message("Sleeping for ", pause)
    Sys.sleep(max(c(0, pause)))
  }
}

projects_scala_stars <- select(projects_scala, project_id) %>% mutate(stars=stars)

write_csv(projects_scala_stars, path(analysis_dir, "stars.csv"))
