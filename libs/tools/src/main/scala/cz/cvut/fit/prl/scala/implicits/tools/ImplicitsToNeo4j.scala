package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.tools.neo4j.Converter
import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}
import org.neo4j.graphdb.{GraphDatabaseService, Transaction}



object ImplicitsToNeo4j extends App {

  // Registers a shutdown hook for the Neo4j instance so that it
  // shuts down nicely when the VM exits (even if you "Ctrl-C" the
  // running application).
  private def registerShutdownHook(managementService: DatabaseManagementService): Unit = {

    sys.addShutdownHook(new Thread() {
      override def run(): Unit = {
        managementService.shutdown()
      }
    })
  }

  def sayHello():String = {
    "Hello"
  }

  def cleanUpDatabase(implicit graphDb: GraphDatabaseService): Unit = {
    val transaction = graphDb.beginTx()

    try {
      transaction.execute("MATCH (n) DETACH DELETE n")
      transaction.commit()
    } catch {
      case e: Throwable =>
        e.printStackTrace()}
    finally {
      transaction.close()
    }
  }

  def run(projectDir: File, implicitsFile: File): Unit = {
    val DEFAULT_DB_NAME = "neo4j"

    val dbDirectoryRelPath = "/neo4jDB"
    val dbDirectoryPath = File(projectDir + dbDirectoryRelPath).toJava

    // opening/creating new graph db
    val managementService = new DatabaseManagementServiceBuilder(dbDirectoryPath).build()
    implicit val graphDb: GraphDatabaseService = managementService.database(DEFAULT_DB_NAME)
    registerShutdownHook(managementService)

    cleanUpDatabase

    val transaction: Transaction = graphDb.beginTx()
    val converter = Converter(transaction)
    try {

      implicitsFile.inputStream.apply(
        input => Project.streamFrom(input).foreach(
          project => {
            print(s"Converting ${project.projectId}:")
            val time = System.currentTimeMillis()
            converter.createProject(project)
            println(
              s" Converted in ${System.currentTimeMillis() - time}ms")
          }
        )
      )

      transaction.commit()
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    } finally {
      transaction.close()
      managementService.shutdown()
    }
  }

  val corporaDir = "/home/panpuncocha/skola/bt/OOPSLA19-artifact/corpora/"

  val projectDir = File(corporaDir + "2-single")
  val implicitsBinRelPath = "/implicits2.bin"
//  val projectDir = File(corporaDir + "1-example")
//  val implicitsBinRelPath = "/_analysis_/implicits.bin"

  val implicitsFile = File(projectDir + implicitsBinRelPath)

  run(projectDir, implicitsFile)
}
