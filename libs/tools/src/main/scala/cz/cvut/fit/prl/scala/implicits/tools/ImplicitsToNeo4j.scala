package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.tools.neo4j.Converter
import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}
import org.neo4j.graphdb.{GraphDatabaseService, Transaction}
import org.neo4j.configuration.GraphDatabaseSettings


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
    val managementService = new DatabaseManagementServiceBuilder(dbDirectoryPath)
      .setConfig[java.lang.String](GraphDatabaseSettings.keep_logical_logs, "1 files")
      .setConfig[java.lang.Long](GraphDatabaseSettings.logical_log_rotation_threshold, (1024*128).toLong)
      .setConfig[java.lang.String](GraphDatabaseSettings.keep_logical_logs, "keep_none")
      .loadPropertiesFromFile(getClass.getResource("/neo4j.conf").getFile)
      .build()
    implicit val graphDb: GraphDatabaseService = managementService.database(DEFAULT_DB_NAME)
    registerShutdownHook(managementService)

    cleanUpDatabase

    val beforeTransactionTime = System.currentTimeMillis()
    val transaction: Transaction = graphDb.beginTx()
    val converter = Converter(transaction)
    val totalProjectExportTime = System.currentTimeMillis()
    try {
      converter.initiateDatabase()
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
      println(s" Projects export in ${System.currentTimeMillis() - totalProjectExportTime}ms")

      val transactionCommitTime = System.currentTimeMillis()
      transaction.commit()
      println(s" Transaction commit in ${System.currentTimeMillis() - transactionCommitTime}ms")
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    } finally {
      transaction.close()
      println(s" Transaction time: ${System.currentTimeMillis() - beforeTransactionTime}ms")
      managementService.shutdown()
    }
  }

  val corporaDir = "/home/panpuncocha/skola/bt/OOPSLA19-artifact/corpora/"
//
  val projectDir = File(corporaDir + "2-single")
//  val projectDir = File(corporaDir + "5-extracted")

  val implicitsBinRelPath = "/implicits2.bin"
//  val projectDir = File(corporaDir + "1-example")
//  val implicitsBinRelPath = "/_analysis_/implicits.bin"

  val implicitsFile = File(projectDir + implicitsBinRelPath)

  run(projectDir, implicitsFile)
}
