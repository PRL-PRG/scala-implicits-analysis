include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/Makevars

GHTORRENT_DIR := /mnt/nvme1/scala/ghtorrent
SCRIPTS_DIR := scripts
RUN_DIR := projects
LOG_DIR_BASE := logs
TIMESTAMP := $(shell LC_LOCALE=C date)
LOG_DIR_LATEST := $(LOG_DIR_BASE)/latest

# it cannot be simply be ?= because that one is not immediate, but deferred so
# it will be evaluated multiple times
ifeq ($(origin LOG_DIR), undefined)
	LOG_DIR := $(LOG_DIR_BASE)/$(shell echo $$(($$(ls -1 $(LOG_DIR_BASE) | grep '[0-9][0-9]*' | wc -l) + 1)))
endif

# a file describing the corpus generated from dejavu by
# scripts/analysis/corpus.Rmd
SCALA_PROJECT_CSV := scala-projects.csv
# a file with project names (one per line) that should be run
PROJECTS_FILE := projects.txt
# a file controlling the number of parallel tasks it can be updated while the
# task is run, but gets reset before a task is run
JOBS_FILE := jobsfile.txt

# --------------------------------------------------------------------------------

# the location of this makefile
base_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# parallel exit code is based on how many jobs has failed there will for sure be
# some so we just say keep going this will run make on each package with tasks
# given in as parameters
define parallel =
@echo $(N_JOBS) > jobsfile.txt
@echo ">> $(color_green)Running parallel with logs in $(LOG_DIR)$(color_off)..."
-parallel \
  --jobs $(JOBS_FILE) \
  -a "$(PROJECTS_FILE)" \
  --files \
  --bar \
  --tagstring "$@ - {}:" \
  --result "$(RUN_DIR)/{1}/$(ANALYSIS_DIR)/parallel/{2}" \
  --joblog "$(LOG_DIR)/parallel.log" \
  --timeout $(TIMEOUT) \
  --shuf \
  make $(MFLAGS) -C "$(RUN_DIR)/{1}" -f $(base_dir)/Makefile.project "{2}" \
  :::
endef

# there is no direct rule how to create RUN_DIR this has to be done manually
# using either download-projects or link-ghtorrent-projects tasks
bootstrap := $(LOG_DIR) $(RUN_DIR)

.PHONY: metadata semanticdb clean distclean build-sbt-plugins

$(LOG_DIR):
	@mkdir -p $(LOG_DIR)
	@echo "$(TIMESTAMP)" > $(LOG_DIR)/timestamp
	@rm -f $(LOG_DIR_LATEST)
	@ln -s $(notdir $(LOG_DIR)) $(LOG_DIR_LATEST)


download-projects: $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	cat projects-all.txt | sed 's|\(.*\)--\(.*\)|\1,\2|g' | \
        parallel -C, --bar -j4 git clone "https://github.com/{1}/{2}" projects/"{1}--{2}"

# this one has a hard coded requirement for the projects file which comes from clean-corpus.Rmd
link-ghtorrent-projects: $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "$(GHTORRENT_DIR)/tmp/{project_id},$(RUN_DIR)/{project}")' | \
		parallel -C, --bar -j1 ln -sf "{1}" "{2}"
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "{project}")' > $(PROJECT_FILE)

tail-log: $(LOG_DIR_LATEST)
	tail -f $(LOG_DIR_LATEST)/parallel.log

generate-task-report:
	[ -d $(LOG_DIR) ] || exit 1
	$(SCRIPTS_DIR)/projects-by-status.R $(LOG_DIR)/parallel.log
	cp $(SCRIPTS_DIR)/parallel-task-result.Rmd $(LOG_DIR)
	Rscript -e 'rmarkdown::render("$(LOG_DIR)/parallel-task-result.Rmd")'
	@rm $(LOG_DIR)/parallel-task-result.Rmd
	@echo "REPORT is in $(LOG_DIR)/parallel-task-result.html"
	@wc -l $(LOG_DIR)/*.txt

metadata: $(bootstrap)
	$(parallel) $@


clean-metadata:
	$(parallel) $@
	$(generate-task-report)

semanticdb: $(bootstrap)
	$(parallel) $@
	cp $(SCRIPTS_DIR)/semanticdb-task-result.Rmd $(LOG_DIR)

	env PROJECTS_FILE=$(PROJECTS_FILE) \
      Rscript -e 'rmarkdown::render("$(SCRIPTS_DIR)/semanticdb-task-result.Rmd", output_file="$(LOG_DIR)/result.html", knit_root_dir="$(LOG_DIR)")'
	@echo ">> $(color_green)Report is in $(LOG_DIR)/result.html$(color_off)"

clean-semanticdb:
	$(parallel) $@
	$(generate-task-report)

clean: $(bootstrap)
	$(parallel) $@
	$(generate-task-report)

clean-classes: $(bootstrap)
	$(parallel) $@
	$(generate-task-report)

clean-sbt: $(bootstrap)
	$(parallel) $@
	$(generate-task-report)

reset: $(bootstrap)
	$(parallel) $@
	$(generate-task-report)

build-sbt-plugins: $(IVY_DIR)
	-rm ~/.sbt/0.13/plugins/scala-corpus.sbt
	-rm ~/.sbt/1.0/plugins/scala-corpus.sbt
	cd sbt-plugins && sbt -batch -ivy ../$(IVY_DIR) "^ publishLocal"
	-mkdir -p ~/.sbt/0.13/plugins
	-mkdir -p ~/.sbt/1.0/plugins
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' >\
        ~/.sbt/0.13/plugins/scala-corpus.sbt
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' >\
        ~/.sbt/1.0/plugins/scala-corpus.sbt
