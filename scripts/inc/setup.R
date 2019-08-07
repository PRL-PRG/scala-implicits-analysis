library(fs)
library(ggplot2)
library(ggthemes)
library(knitr)
library(pbapply)
library(stringr)

# set corpus_dir
corpus_dir <- params$corpus_dir
stopifnot(dir_exists(corpus_dir))
options(corpus_dir=corpus_dir)

source(file.path(params$lib_dir, "paths.R"))
source(file.path(params$lib_dir, "functions.R"))
source(file.path(params$lib_dir, "latextags.R"))

# create latex tags based on the report name
# the output will go the corpus_dir
report_name <- params$report_name
stopifnot(!is.null(report_name))
tags_path <- path(corpus_dir, str_c(report_name, ".tex"))
create_tags(tags_path, prefix="tg", default=TRUE)

message("Tags will go into: ", path_real(tags_path))

# let pbapply work in console
pboptions(type="txt")

# output code by default
knitr::opts_chunk$set(echo = TRUE)

# my favorite ggplot theme
theme_set(theme_minimal())

