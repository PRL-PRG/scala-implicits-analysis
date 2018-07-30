TIMEOUT := 30m

GHTORRENT_DIR := /mnt/nvme1/scala/ghtorrent
ANALYSIS_DIR := _analysis_
RUN_DIR := projects
LOG_DIR := logs

SCALA_PROJECT_CSV := $(GHTORRENT_DIR)/scala-projects.csv
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

.PHONY: all metadata semanticdb clean distclean

all: $(LOG_DIR)
	$(parallel) semanticdb metadata-raw

$(LOG_DIR):
	-mkdir -p $(LOG_DIR)

$(PROJECTS_ALL_FILE): $(SCALA_PROJECT_CSV)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "{project}")' > $(PROJECTS_ALL_FILE)

# this one has a hard coded requirement for the projects file which comes from clean-corpus.Rmd
$(RUN_DIR): $(SCALA_PROJECT_CSV)
	-mkdir -p $(RUN_DIR)
	Rscript -e 'glue::glue_data(readr::read_csv("$(SCALA_PROJECT_CSV)"), "$(GHTORRENT_DIR)/tmp/{project_id},$(RUN_DIR)/{project}")' | \
		parallel -C, --bar -j2 ln -sf "{1}" "{2}"

metadata: $(bootstrap)
	$(parallel) $@
compile: $(bootstrap)
	$(parallel) $@
semanticdb: $(bootstrap)
	$(parallel) $@
clean: $(bootstrap)
	$(parallel) $@
sbtclean: $(bootstrap)
	$(parallel) $@
distclean: $(bootstrap)
	$(parallel) $@
