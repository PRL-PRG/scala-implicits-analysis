library(tidyverse)
library(fs)
library(httr)
library(pbapply)

is_in_scaladex <- function(reponame) {
  url <- str_c("https://index.scala-lang.org/", reponame)
  tryCatch(GET(url, config=list(followlocation=1))$status_code, error=function(e)-1)
}

process <- function(gh) {
  res <- pblapply(gh, is_in_scaladex, cl=4)
  is_err <- map_lgl(res, ~is(., "try-error"))
  err <- res[is_err]
  data_frame(gh=gh[!is_err], status=sapply(res[!is_err], function(x) x$status_code))
}

gh <- read_csv("projects.csv", col_names=FALSE) %>% select(gh=X3) %>% mutate(gh=str_replace_all(gh, "\\\\_", "")) %>% .$gh
res <- pblapply(gh, is_in_scaladex, cl=1)

df <- data_frame(project_id=gh, scaladex=unlist(res))
write_csv(df, "scaladex.csv")
