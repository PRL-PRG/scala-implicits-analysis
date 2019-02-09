include Makevars

.PHONY: all libs sbt-plugins tools

all: libs sbt-plugins tools

libs:
	$(MAKE) -C libs

sbt-plugins:
	$(MAKE) -C sbt-plugins uninstall all

tools:
	$(MAKE) -C scripts/tools


