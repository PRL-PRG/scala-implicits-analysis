library(fs)
library(tidyverse)
library(pbapply)

# rename files and fix project_id

BASE_DIR <- "/mnt/array/scala-10.18/download"
projects <- dir_ls(BASE_DIR, recursive = FALSE, type = "directory")
analsis_path <- path(projects, "_analysis_")

files_to_migrate <- c("classpath.csv", "cleanpaths.csv", "source-directories.csv", "versions.csv")
new_names <- c("metadata-classpaths.csv", "metadata-cleanpaths.csv", "metadata-sourcepaths.csv", "metadata-versions.csv")
csv_col_types <- c("ccc", "ccc", "ccccciciii", "cccc")

migrate <- function(old, new, col_types) {
  odf <- read_csv(old, col_types=col_types)
  ndf <- mutate(odf, project_id=str_replace(project_id, "\\.git$", ""))
  write_csv(ndf, new)
  NULL
}

for (i in seq_along(files_to_migrate)) {
  old_files <- path(analsis_path, files_to_migrate[i])
  existing <- old_files[file_exists(old_files)]
  new_files <- path(dirname(existing), new_names[i])
  result <- pblapply(seq_along(existing), function(x) tryCatch(migrate(existing[x], new_files[x], csv_col_types[i]), error=function(e) e))
  if (all(sapply(is.null(result))) != TRUE) {
    stop("Failure for ", i)
  }
}

for (i in seq_along(files_to_migrate)) {
  df <- data_frame(
    old=path(analsis_path, files_to_migrate[i]),
    new=path(dirname(old), new_names[i]),
    old_existing=file_exists(old),
    new_existing=file_exists(new)
  )
  
  if (nrow(filter(df, old_existing != new_existing)) != 0) {
    stop("Different for ", i)
  }
  
  file_delete(filter(df, old_existing)$old)  
}

# metadata-status.csv
files <- path(analsis_path, "metadata-status.csv")
existing_files <- files[file_exists(files)]
result <- pblapply(existing_files, function(x) tryCatch(read_csv(x, col_types="iii", col_names=FALSE) %>% transmute(exit_code=X1, duration=X3) %>% write_csv(x), error=function(e) e))

# semanticdb-status.csv
files <- path(analsis_path, "semanticdb-status.csv")
existing_files <- files[file_exists(files)]
result <- pblapply(existing_files, function(x) tryCatch(read_csv(x, col_types="iii", col_names=FALSE) %>% transmute(exit_code=X1, duration=X3) %>% write_csv(x), error=function(e) e))
