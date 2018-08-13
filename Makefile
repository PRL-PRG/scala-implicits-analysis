include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/Makevars

TIMEOUT := 30m

GHTORRENT_DIR := /mnt/nvme1/scala/ghtorrent
RUN_DIR := projects
LOG_DIR_BASE := logs
LOG_DIR := $(LOG_DIR_BASE)/$(shell date +'%Y%m%d-%H%M%S')
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
-parallel \
  --jobs $(JOBS_FILE) \
  -a "$(PROJECTS_FILE)" \
  --files \
  --bar \
  --tagstring "$@ - {}:" \
  --result "$(RUN_DIR)/{1}/$(ANALYSIS_DIR)/{2}" \
  --joblog "$(LOG_DIR)/$@.log" \
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
	ln -sf $(notdir $(LOG_DIR)) $(LOG_DIR_LATEST)

$(PROJECTS_ALL_FILE): $(SCALA_PROJECT_CSV)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "{project}")' > $(PROJECTS_ALL_FILE)

# this one has a hard coded requirement for the projects file which comes from clean-corpus.Rmd
$(RUN_DIR): $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "$(GHTORRENT_DIR)/tmp/{project_id},$(RUN_DIR)/{project}")' | \
		parallel -C, --bar -j2 ln -sf "{1}" "{2}"

metadata: $(bootstrap)
	@echo 20 > jobsfile.txt
	$(parallel) $@
compile: $(bootstrap)
	@echo 20 > jobsfile.txt
	$(parallel) $@
semanticdb: $(bootstrap)
	@echo 20 > jobsfile.txt
	$(parallel) $@
clean: $(bootstrap)
	@echo 64 > jobsfile.txt
	$(parallel) $@
classclean: $(bootstrap)
	@echo 64 > jobsfile.txt
	$(parallel) $@
gitclean: $(bootstrap)
	@echo 64 > jobsfile.txt
	$(parallel) $@
sbtclean: $(bootstrap)
	@echo 24 > jobsfile.txt
	$(parallel) $@
distclean: $(bootstrap)
	@echo 64 > jobsfile.txt
	$(parallel) $@

sbt-plugins: $(IVY_DIR)
	rm ~/.sbt/0.13/plugins/scala-corpus.sbt
	rm ~/.sbt/1.0/plugins/scala-corpus.sbt
	cd sbt-plugins && sbt -batch -ivy ../$(IVY_DIR) "^ publishLocal"
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' > ~/.sbt/0.13/plugins/scala-corpus.sbt
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' > ~/.sbt/1.0/plugins/scala-corpus.sbt
