## About

This repository contains an analysis pipeline that
1. given a set of Scala projects, it builds a corpus of [SemanticDB](https://scalameta.org/docs/semanticdb/specification.html) databases including project metadata information extracted from GitHub and SBT, and
2. from the corpus, it creates [model](https://github.com/PRL-PRG/scala-implicits-analysis/blob/master/libs/model/src/main/protobuf/model.proto) of implicits declared and used in each project.

This has been used to analyze the use of Scala implicits in the wild.

## Requirements

- GNU make >= 4.2.1 (on OSX this means to install one from homebrew/macports/nix)
- GNU parallel >= 2018*
- GNU sed
- R >= 3.5
- R packages
  - tidyverse
  - fs
  - DT
  - lubridate
- cloc >= 1.8
- SBT

## Structure

- `libs` - extractor tools
- `libs/metadata` - cases classes and constants defining project metadata
- `libs/model` - definition of the extracted protobuf [model](https://github.com/PRL-PRG/scala-implicits-analysis/blob/master/libs/model/src/main/protobuf/model.proto)
- `libs/tools` - implicit extractor
- `sbt-plugins` - metadata and semanticdb plugin
- `scripts` - various scripts used during a corpus building
- `scripts/analysis` - scripts for the corpus analysis
- `scripts/tools` - [Dejavu](https://github.com/PRL-PRG/dejavu-artifact) updated to process Scala

- `Makefile` - building this project
- `Makefile.project` - building a project in corpus
- `Makefile.corpus` - orchestrating corpus build
- `Makevars` - variables
- `Makevars-$USER@$HOST` - local variables override
- `docker-compose-$USER@$HOST.yml` - a docker setup that starts Rstudio and Jupyer with all data mounted

## Build

```
make
```

## Corpus

### Creating a corpus

1. In some directory, create a text file `all-projects.txt` which has one `<github-organization-name>--<github-repository-name>` per line.
2. Symlink there `Makefile.corpus` as `Makefile`
3. (optional) create `all-projects-patch.csv` which contains `project_id,patch` where patch is a command that shall be run in the repository right after it is downloaded from github.
4. Set `GH_CLIENT_ID` and `GH_CLIENT_SECRET` environment variables.
5. Run `make report`

### Generated files

- `compile-status.csv` - result of compilation of projects
- `dejavu-files-hash-h2i.csv` - a hash of the Scala indexed files
- `implicits.bin` - a protobuf stream of
  [`Project`](https://github.com/PRL-PRG/scala-implicits-analysis/blob/master/libs/model/src/main/protobuf/model.proto)s,
  one per successfully extracted project
- `implicits-exceptions.csv` - implicit extraction exceptions
- `implicits-stats.csv` - implicit extraction stats like number of implicit declarations or number of  implicit callsites
- `implicits-status.csv` - result of implicit extraction
- `metadata-status.csv`  - result of metadata extraction
- `projects-github-info.csv` - full project names and number of stars fetched from github
- `projects.txt` - final list of projects on which the extraction will be attempted
- `repo-metadata.csv` - repository metadata
- `report.html` - a status report
- `repo-sloc.csv` - SLOC of each project
- `sbt-projects.txt` - list of projects using SBT >= 0.13
- `semanticdb-stats.csv` - number of semanticdb files, symbols, occurences and synthetics
- `semanticdb-status.csv` - result of semanticdb compilation
- `so-projects.txt` - list of unique projects

### Caching

It is a good idea to setup a cache for dependencies. One way is:

1. spawn a docker container with arfifactory-oss

```sh
docker pull docker.bintray.io/jfrog/artifactory-oss
docker volume create --name scala-implicits-artifactory-cache
docker run --name scala-implicits-artifactory -d -v scala-implicits-artifactory-cache:/var/opt/jfrog/artifactory -p 8081:8081 docker.bintray.io/jfrog/artifactory-oss:latest
```

2. log into http://localhost:8081 and create a new virtual repository called `local-sbt-cache`

3. put the following into `~/.sbt/repositories`:

```
[repositories]
local
my-ivy-proxy-releases: http://localhost:8081/artifactory/local-sbt-cache/, [organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]
my-maven-proxy-releases: http://localhost:8081/artifactory/local-sbt-cache/
```

