package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, ClassSignature, ClasspathEntry, Declaration, Language, Location, MethodSignature, Module, Parameter, ParameterList, PathEntry, Project, SourcepathEntry, TypeRef, ValueRef, ValueSignature}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.Labels
import org.neo4j.dbms.api.DatabaseManagementServiceBuilder
import org.neo4j.graphdb.GraphDatabaseService
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.reflect.io.Directory


class ConverterITSuite extends FunSuite with Matchers {
  test("convertProject") {
    val tmpDirPath = sys.props("java.io.tmpdir")
    val dbDirectory = File(tmpDirPath + "/ImplicitsToNeo4jITtests/convertProjectTest")
    if (dbDirectory.exists)
      new Directory(dbDirectory.toJava).deleteRecursively()
    dbDirectory.createDirectories()

    // opening/creating new graph db
    val managementService = new DatabaseManagementServiceBuilder(dbDirectory.toJava)
      .build()


    val DEFAULT_DB_NAME = "neo4j"
    val graphDb: GraphDatabaseService = managementService.database(DEFAULT_DB_NAME)


    val beforeTransactionTime = System.currentTimeMillis()
    val converter = Converter()
    var transaction = graphDb.beginTx()
    var failed = false

    try {
      converter.createProject(createProject(), transaction)
      transaction.commit()

      transaction = graphDb.beginTx()
      transaction.getAllLabels.asScala.size shouldBe 16
      transaction.findNodes(Labels.Declaration).asScala.size shouldBe 3
      transaction.findNodes(Labels.CallSite).asScala.size shouldBe 1
      transaction.findNodes(Labels.Project).asScala.size shouldBe 1
      transaction.findNodes(Labels.Module).asScala.size shouldBe 1
      transaction.findNodes(Labels.TypeReference).asScala.size shouldBe 1
      // todo some test

    } catch {
      case e: Throwable =>
        e.printStackTrace()
        failed = true
    } finally {
      transaction.close()
      println(s"Total time: ${System.currentTimeMillis() - beforeTransactionTime}ms")
      managementService.shutdown()
      new Directory(dbDirectory.toJava).deleteRecursively()
    }
    failed shouldBe false
  }

  def createProject(): Project = {
    Project("projectId", "sbtVersion", createModules())
  }

  // creating declarations: Int, implicitVal: Int, sum(a: Int)(implicit b: Int)
  // implicit callsite sum()
  def createModules(): Map[String, Module] = {
    val paths = Map("path1" -> SourcepathEntry("path1", "", true), "scala" -> ClasspathEntry("scala", "scala", "scala.values", "", "", true, true, true))
    val moduleId = "moduleId"
    val declaration1 = Declaration("implValue", moduleId, Declaration.Kind.VAL, 32, "implValue", Declaration.Access.PUBLIC,
      Location("path1",""), Language.SCALA, signature = ValueSignature(TypeRef("Int")))
    val declaration2 = Declaration("Int", moduleId, Declaration.Kind.CLASS, 0, "Int", Declaration.Access.PUBLIC,
      Location("scala",""), Language.SCALA, signature = ClassSignature())

    val sumParameterLists = Seq(ParameterList(Seq(Parameter("a", TypeRef("Int"), false))), ParameterList(Seq(Parameter("b", TypeRef("Int"), true))))
    val declaration3 = Declaration("sum", moduleId, Declaration.Kind.DEF, 0, "sum", Declaration.Access.PUBLIC,
      Location("path1",""), Language.SCALA, signature = MethodSignature(Seq(), sumParameterLists, TypeRef("Int")))

    val declarations = Map("implValue" -> declaration1, "Int" -> declaration2, "sum" -> declaration3)


    val implicitCallSites = Seq(CallSite(1, None, moduleId, "sum", "sum()", null, implicitArgumentTypes = Seq(ValueRef("implValue"))))

    val module = Module(moduleId, "projectId", "groupId", "artifactId1", "version", "commit", "scalaVersion",
      paths, declarations, implicitCallSites, implicitCallSites.size, 0)

    Map(moduleId -> module)
  }
}
