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

	rsync \
      -avh \
      --delete \
      ~/.ivy2/local/cz.cvut.fit.prl.scala.implicits/ \
      prl4:.ivy2/local/cz.cvut.fit.prl.scala.implicits

	scp ~/.sbt/1.0/global.sbt prl4:.sbt/1.0/global.sbt
	scp ~/.sbt/0.13/global.sbt prl4:.sbt/0.13/global.sbt
