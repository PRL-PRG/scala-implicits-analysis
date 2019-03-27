get_corpus_path <- function(p) {
  corpus_dir <- getOption("corpus_dir")
  if (is.null(corpus_dir)) {
    corpus_dir <- getwd()
  }
  file.path(corpus_dir, p)
}

CORPUS_STAGE1 <- get_corpus_path("corpus-stage1.csv")
CORPUS_STAGE3 <- get_corpus_path("corpus-stage3.csv")

REPO_METADATA <- get_corpus_path("repo-metadata.csv")
PROJECTS_GH_INFO_PINNED <- get_corpus_path("projects-github-info.csv.pinned")
PROJECTS_GH_INFO <- get_corpus_path("projects-github-info.csv")

ALL_PROJECTS_FILE <- get_corpus_path("all-projects.txt")
ALL_PROJECTS_FILE_ORIG <- get_corpus_path("all-projects.orig.txt")
SBT_PROJECTS_FILE <- get_corpus_path("sbt-projects.txt")
PROJECTS_FILE <- get_corpus_path("projects.txt")

ALL_PROJECTS_DIR <- get_corpus_path("all-projects")
PROJECTS_DIR <- get_corpus_path("projects")

DEJAVU_H2I <- get_corpus_path("dejavu-files-hash-h2i.csv")
DEJAVU_DUPLICATION <- get_corpus_path("dejavu-duplication.csv")

GITHUB_INFO <- get_corpus_path("projects-github-info.csv")
SCALADEX <- get_corpus_path("scaladex.txt")

IMPLICIT_DECLARATIONS <- "implicit-declarations.csv"
IMPLICIT_CALLSITES <- "implicit-callsites.csv"

# PER PROJECT
ANALYSIS_DIR <- "_analysis_"

COMPILE_STATUS <- get_corpus_path("compile-status.csv")
COMPILE_LOG <- get_corpus_path("compile.log")
GLOBAL_COMPILE_STATUS <- get_corpus_path("compile-status.csv")

METADATA_STATUS <- "metadata-status.csv"
METADATA_MODULES <- "metadata-modules.csv"
METADATA_SOURCEPATHS <- "metadata-sourcepaths.csv"
METADATA_DEPENDENCIES <- "metadata-dependencies.csv"
METADATA_LOG <- "metadata.log"
GLOBAL_METADATA_STATUS <- get_corpus_path(METADATA_STATUS)
GLOBAL_METADATA_MODULES <- get_corpus_path(METADATA_MODULES)
GLOBAL_METADATA_SOURCEPATHS <- get_corpus_path(METADATA_SOURCEPATHS)
GLOBAL_METADATA_DEPENDENCIES <- get_corpus_path(METADATA_DEPENDENCIES)

SEMANTICDB_STATUS <- "semanticdb-status.csv"
SEMANTICDB_STATS <- "semanticdb-stats.csv"
SEMANTICDB_LOG <- "semanticdb.log"
GLOBAL_SEMANTICDB_STATUS <- get_corpus_path(SEMANTICDB_STATUS)
GLOBAL_SEMANTICDB_STATS <- get_corpus_path(SEMANTICDB_STATS)

IMPLICITS_STATUS <- "implicits-status.csv"
IMPLICITS_STATS <- "implicits-stats.csv"
IMPLICITS_EXCEPTIONS <- "implicits-exceptions.csv"
IMPLICITS_LOG <- "implicits.log"
IMPLICITS <- "implicits.bin"
GLOBAL_IMPLICITS_STATUS <- get_corpus_path(IMPLICITS_STATUS)
GLOBAL_IMPLICITS_STATS <- get_corpus_path(IMPLICITS_STATS)
GLOBAL_IMPLICITS_EXCEPTIONS <- get_corpus_path(IMPLICITS_EXCEPTIONS)
GLOBAL_IMPLICITS <- get_corpus_path(IMPLICITS)

projects_file <- Vectorize(function(project, filename) {
  path(PROJECTS_DIR, project, ANALYSIS_DIR, filename)
}, USE.NAMES = FALSE, vectorize.args = "project")
