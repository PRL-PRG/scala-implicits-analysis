library(fs)
library(httr)
library(tidyverse)
library(pbapply)

PAGES_TO_FETCH <- 70
SCALA_VERSIONS <- c("2.11", "2.12")

fetch_page <- function(page, version) {
  url <- str_glue("https://index.scala-lang.org/api/search?q=*&target=JVM&scalaVersion={version}&total=100&page={as.integer(page)}")
  GET(url)
}

pages <- pbapply(expand.grid(1:PAGES_TO_FETCH, SCALA_VERSIONS), 1, function(x) fetch_page(x[1], x[2]))
errors <- map_lgl(pages, ~ .$status_code != 200)
any(errors)

pages_json <- map(pages, ~content(.)) %>% unlist(recursive = F)

projects <- pages_json %>% map_dfr(~data_frame(project_id=str_c(.$organization, "--", .$repository))) %>% distinct()

write_lines(projects$project_id, "corpora/4-scaladex/projects.txt")
