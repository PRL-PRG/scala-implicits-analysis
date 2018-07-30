library(tidyverse)
library(fs)

ghtorrent_dir <- "/mnt/array/scala/ghtorrent"
analysis_dir <- "/mnt/nvme1/scala/ghtorrent"

projects_scala <- read_csv(path(analysis_dir, "scala-projects.csv"))

watchers <- read_csv(
  path(ghtorrent_dir, "watchers.csv"), 
  col_names=c("project_id", "user_id", "timestamp"), 
  col_types="ii-"
)
watchers_count <- count(watchers, project_id)
watchers <- NULL

projects_scala_stars <- 
  left_join(
    select(projects_scala, project_id),
    select(watchers_count, project_id, stars=n),
    by="project_id"
  ) %>% 
  mutate(stars=coalesce(stars, 0L))

write_csv(projects_scala_stars, path(analysis_dir, "stars.csv"))
