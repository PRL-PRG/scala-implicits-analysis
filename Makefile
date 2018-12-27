include Makevars

.PHONY: all libs sbt-plugins sync-prl4

all: libs sbt-plugins

libs:
	$(MAKE) -C libs

sbt-plugins:
	$(MAKE) -C sbt-plugins

sync-prl4:
	rsync \
      -avh \
      --exclude "jobsfile.txt" \
      --exclude "logs" \
      --exclude ".ivy" \
      --exclude "corpora" \
      --exclude "target" \
      . prl4:Research/Projects/scala-corpus/

