library(tidyverse)
library(fs)
library(data.table)

ghtorrent_dir <- "/mnt/array/scala/ghtorrent"
analysis_dir <- "/mnt/nvme1/scala/ghtorrent"

projects_scala <- read_csv(path(analysis_dir, "scala-projects.csv"))

process_one <- function(p) {
  commits <- 
    fread(
      p,
      header=FALSE,
      drop=2,
      showProgress=TRUE
    ) %>%
    as_data_frame() %>%
    rename(project_id=V1)
    
  commits_count <- count(commits, project_id)
  commits <- NULL
  
  left_join(
    select(projects_scala, project_id),
    select(commits_count, project_id, commits=n),
    by="project_id"
  ) %>% 
  filter(!is.na(commits))
}

files <- dir_ls(path(ghtorrent_dir, "projects-commits"))
dfs <- pblapply(files, process_one, cl=8)
df <- bind_rows(dfs)
commits <- group_by(df, project_id) %>% summarise(commits=sum(commits))

write_csv(commits, path(analysis_dir, "commits.csv"))
