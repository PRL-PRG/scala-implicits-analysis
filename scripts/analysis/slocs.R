library(tidyverse)
library(fs)

projects_dir <- "/mnt/nvme1/scala/ghtorrent/tmp"
analysis_dir <- "/mnt/nvme1/scala/ghtorrent"

projects_scala <- read_csv(path(analysis_dir, "scala-projects.csv"))

process_one <- function(project_id) {
  row <- NULL
  
  #print(project_id)
  sloc_file <- path(projects_dir, project_id, "_analysis_", "sloc.csv")
  if (file_exists(sloc_file)) {
    sloc <- read_csv(sloc_file, col_types="iciii")
    if (length(sloc) == 5) {
      row <- filter(sloc, language == "Scala") %>% mutate(project_id=project_id) %>% select(-language)
    }
  }

  if (is.null(row) || nrow(row) != 1) {
    row <- data_frame(files=0, blank=0, comment=0, code=0, project_id=project_id)
  } 
  
  row
}

dfs <- pblapply(projects_scala$project_id, process_one, cl=4)
df <- bind_rows(dfs)

projects_scala_slocs <- left_join(
  select(projects_scala, project_id),
  df,
  by="project_id"
)

write_csv(projects_scala_slocs, path(analysis_dir, "slocs.csv"))
