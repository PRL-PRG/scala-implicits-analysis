## Requirements

- GNU make >= 4.2.1 (on OSX this means to install one from homebrew/macports/nix)
- GNU parallel >= 2018*
- R >= 3.5
- R packages
  - tidyverse
  - fs
  - DT
- cloc >= 1.8


From some reason I had to install xml2 package manually (it just would not pick the `PKG_CONFIG_DIR` from homebrew):

```sh
$ Rscript -e 'install.packages("xml2")'
...
* installing *source* package ‘xml2’ ...
** package ‘xml2’ successfully unpacked and MD5 sums checked
Found pkg-config cflags and libs!
Using PKG_CFLAGS=-I/usr/include/libxml2
Using PKG_LIBS=-L/usr/lib -lxml2 -lz -lpthread -licucore -lm
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libxml-2.0 was not found. Try installing:
 * deb: libxml2-dev (Debian, Ubuntu, etc)
 * rpm: libxml2-devel (Fedora, CentOS, RHEL)
 * csw: libxml2_dev (Solaris)
If libxml-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libxml-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘xml2’
* removing ‘/usr/local/lib/R/3.5/site-library/xml2’

The downloaded source packages are in
	‘/private/var/folders/wg/z8mlj6wn0y5_y5lvh7rl03zc0000gn/T/RtmphvM8l8/downloaded_packages’
Warning message:
In install.packages("xml2") :
  installation of package ‘xml2’ had non-zero exit status

$ cd /private/var/folders/wg/z8mlj6wn0y5_y5lvh7rl03zc0000gn/T/RtmphvM8l8/downloaded_packages
$ tar xfzv xml2_1.2.0.tar.gz
$ cd xml2
$ R CMD INSTALL --configure-vars='INCLUDE_DIR=/usr/local/Cellar/libxml2/2.9.7/include/libxml2 LIB_DIR=/usr/local/Cellar/libxml2/2.9.7/lib' .
```

## Get projects

First things is to populate the `RUN_DIR` which is by default `projects`. There
are two ways to do that. Either download projects from github or from existing
ghtorrent snapshot.

### Download projects from github

The `download-projects` task in the `Makefile` will download all projects found
the `PROJECTS_FILE` file. This is a text file which serves as a driver to most
of the task defined in the `Makefile`. It contains a list of projects separated
by new lines. A project name consists of github username and repository
separated by `--`: `<github-username>--<repository-name>` (_e.g._
`ensime--ensime-server`).

```sh
make download-projects PROJECT_FILE=projects-top24.txt
```

### Dejavu


```sh
make link-ghtorrent-projects SCALA_PROJECT_CSV=scala-projects-top24.csv
```

```sh
make projects-all.txt SCALA_PROJECT_CSV=scala-projects-top24.csv
```

### Generate semanticdb

```sh
make semanticdb PROJECTS_FILE=projects-all.txt
```

### Generating reports

```sh
make report-latest
```

### Regenerating semanticdb

```sh
make clean-semanticdb semanticdb PROJECTS_FILE=projects-all.txt
```
