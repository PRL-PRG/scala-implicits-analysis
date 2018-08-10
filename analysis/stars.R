library(pbapply)
library(tidyverse)
library(fs)
library(httr)

get_gh_stars <- function(reponame) {
  tryCatch({
    url <- str_c("https://api.github.com/repos/", reponame, "?client_id=", client_id, "&client_secret=", client_secret)
    as.integer(content(GET(url))$stargazers_count)
  }, error=function(e) {
    message("Unable to get stargazers for ", reponame)
    -1L
  })
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
  stars[batch] <- pbsapply(repos, get_gh_stars)
  end <- Sys.time()
  
  duration <- as.numeric(end - start, unit="secs")
  message("Sleeping for ", duration)
  Sys.sleep(max(c(0, 3600-duration)))
}

projects_scala_stars <- 
  left_join(
    select(projects_scala, project_id),
    select(watchers_count, project_id, stars=n),
    by="project_id"
  ) %>% 
  mutate(stars=coalesce(stars, 0L))

write_csv(projects_scala_stars, path(analysis_dir, "stars.csv"))
