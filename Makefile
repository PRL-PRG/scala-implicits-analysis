include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/Makevars

GHTORRENT_DIR := /mnt/nvme1/scala/ghtorrent
SCRIPTS_DIR := scripts
RUN_DIR := projects
LOG_DIR_BASE := logs
LOG_DIR := $(LOG_DIR_BASE)/$(shell echo $$(($$(ls -1 $(LOG_DIR_BASE) | grep '[0-9][0-9]*' | wc -l) + 1)))
TIMESTAMP := $(shell LC_LOCALE=C date)
LOG_DIR_LATEST := $(LOG_DIR_BASE)/latest

SCALA_PROJECT_CSV := scala-projects.csv
PROJECTS_ALL_FILE := projects-all.txt
PROJECTS_FILE := projects.txt
JOBS_FILE := jobsfile.txt

base_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# parallel exit code is based on how many jobs has failed there will for sure be
# some so we just say keep going this will run make on each package with tasks
# given in as parameters
define parallel =
@echo $(N_JOBS) > jobsfile.txt
-parallel \
  --jobs $(JOBS_FILE) \
  -a "$(PROJECTS_FILE)" \
  --files \
  --bar \
  --tagstring "$@ - {}:" \
  --result "$(RUN_DIR)/{1}/$(ANALYSIS_DIR)/{2}" \
  --joblog "$(LOG_DIR)/parallel.log" \
  --timeout $(TIMEOUT) \
  --shuf \
  make $(MFLAGS) -C "$(RUN_DIR)/{1}" -f $(base_dir)/Makefile.project "{2}" \
  :::
endef

bootstrap := $(LOG_DIR) $(RUN_DIR)

.PHONY: all metadata semanticdb clean distclean sbt-plugins

all: $(LOG_DIR)
	$(parallel) semanticdb metadata-raw

$(LOG_DIR):
	@echo "LOG_DIR: $(LOG_DIR)"
	@mkdir -p $(LOG_DIR)
	echo "$(TIMESTAMP)" > $(LOG_DIR)/timestamp
    -@rm $(LOG_DIR_LATEST)
	ln -s $(notdir $(LOG_DIR)) $(LOG_DIR_LATEST)

$(PROJECTS_ALL_FILE): $(SCALA_PROJECT_CSV)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "{project}")' > $(PROJECTS_ALL_FILE)

download-projects: $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	cat projects-all.txt | sed 's|\(.*\)--\(.*\)|\1,\2|g' | \
        parallel -C, --bar -j2 git clone "https://github.com/{1}/{2}" projects/"{1}--{2}"

# this one has a hard coded requirement for the projects file which comes from clean-corpus.Rmd
link-ghtorrent-projects: $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "$(GHTORRENT_DIR)/tmp/{project_id},$(RUN_DIR)/{project}")' | \
		parallel -C, --bar -j2 ln -sf "{1}" "{2}"

log-result: $(LOG_DIR_LATEST)
	$(SCRIPTS_DIR)/projects-by-status.R $(LOG_DIR_LATEST)/parallel.log
	cp $(SCRIPTS_DIR)/parallel-task-result.Rmd $(LOG_DIR_LATEST)
	Rscript -e 'rmarkdown::render("$(LOG_DIR_LATEST)/parallel-task-result.Rmd")'
	@rm $(LOG_DIR_LATEST)/parallel-task-result.Rmd

clean-semanticdb:
	parallel --bar -j2 -a $(PROJECTS_FILE) rm -f $(RUN_DIR)/"{1}"/$(SEMANTICDB)

metadata: $(bootstrap)
	$(parallel) $@
compile: $(bootstrap)
	$(parallel) $@
semanticdb: $(bootstrap)
	$(parallel) $@
clean: $(bootstrap)
	$(parallel) $@
classclean: $(bootstrap)
	$(parallel) $@
gitclean: $(bootstrap)
	$(parallel) $@
sbtclean: $(bootstrap)
	$(parallel) $@
distclean: $(bootstrap)
	$(parallel) $@

sbt-plugins: $(IVY_DIR)
	-rm ~/.sbt/0.13/plugins/scala-corpus.sbt
	-rm ~/.sbt/1.0/plugins/scala-corpus.sbt
	cd sbt-plugins && sbt -batch -ivy ../$(IVY_DIR) "^ publishLocal"
	-mkdir -p ~/.sbt/0.13/plugins
	-mkdir -p ~/.sbt/1.0/plugins
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' > ~/.sbt/0.13/plugins/scala-corpus.sbt
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' > ~/.sbt/1.0/plugins/scala-corpus.sbt
