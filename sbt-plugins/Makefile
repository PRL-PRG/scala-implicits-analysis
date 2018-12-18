include ../Makevars

FILENAME := scala-implicits.sbt

all: $(IVY_DIR)
	-rm ~/.sbt/0.13/plugins/$(FILENAME)
	-rm ~/.sbt/1.0/plugins/$(FILENAME)
	sbt -batch -ivy ../$(IVY_DIR) "^ publishLocal"
	-mkdir -p ~/.sbt/0.13/plugins
	-mkdir -p ~/.sbt/1.0/plugins
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala.implicits" % "plugins" % "1.0-SNAPSHOT")' >\
        ~/.sbt/0.13/plugins/$(FILENAME)
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala.implicits" % "plugins" % "1.0-SNAPSHOT")' >\
        ~/.sbt/1.0/plugins/$(FILENAME)
