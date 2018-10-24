include ../Makevars

all: $(IVY_DIR)
	-rm ~/.sbt/0.13/plugins/scala-corpus.sbt
	-rm ~/.sbt/1.0/plugins/scala-corpus.sbt
	sbt -batch -ivy ../$(IVY_DIR) "^ publishLocal"
	-mkdir -p ~/.sbt/0.13/plugins
	-mkdir -p ~/.sbt/1.0/plugins
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' >\
        ~/.sbt/0.13/plugins/scala-corpus.sbt
	echo 'addSbtPlugin("cz.cvut.fit.prl.scala-corpus" % "sbt-plugins" % "0.1-SNAPSHOT")' >\
        ~/.sbt/1.0/plugins/scala-corpus.sbt
