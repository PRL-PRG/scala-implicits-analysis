include Makevars

.PHONY: all libs sbt-plugins

all: libs sbt-plugins

libs:
	$(MAKE) -C libs

sbt-plugins:
	$(MAKE) -C sbt-plugins
