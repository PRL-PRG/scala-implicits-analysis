include ../Makevars

GROUP_ID := cz.cvut.fit.prl.scala.implicits
ARTIFACT_ID := sbt-plugins
VERSION := 1.0-SNAPSHOT

FILENAME := scala-implicits.sbt
PLUGIN_013 := ~/.sbt/0.13/plugins/$(FILENAME)
PLUGIN_10 := ~/.sbt/1.0/plugins/$(FILENAME)
JAR := $(GLOBAL_IVY_DIR)/local/$(GROUP_ID)/$(ARTIFACT_ID)/scala_2.12/sbt_1.0/$(VERSION)/jars/$(ARTIFACT_ID).jar

.PHONY: all install uninstall

all: install

$(JAR):
	sbt -batch -ivy $(GLOBAL_IVY_DIR) "^ publishLocal"

$(PLUGIN_013): $(JAR)
	-mkdir -p ~/.sbt/0.13/plugins
	echo 'addSbtPlugin("$(GROUP_ID)" % "$(ARTIFACT_ID)" % "$(VERSION)")' >\
        $(PLUGIN_013)

$(PLUGIN_10): $(JAR)
	echo 'addSbtPlugin("$(GROUP_ID)" % "$(ARTIFACT_ID)" % "$(VERSION)")' >\
        $(PLUGIN_10)
	-mkdir -p ~/.sbt/1.0/plugins

install: $(PLUGIN_013) $(PLUGIN_10)

uninstall:
	-rm ~/.sbt/0.13/plugins/$(FILENAME)
	-rm ~/.sbt/1.0/plugins/$(FILENAME)
	-rm $(JAR)
