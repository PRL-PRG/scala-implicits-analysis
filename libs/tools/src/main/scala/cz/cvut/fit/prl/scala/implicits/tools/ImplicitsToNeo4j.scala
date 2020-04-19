package cz.cvut.fit.prl.scala.implicits.tools

import java.util.stream.StreamSupport

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, CallSiteRef, ClassSignature, ClasspathEntry, Declaration, MethodSignature, Module, ParameterList, Project, Signature, SourcepathEntry, TypeRef, TypeSignature, ValueRef, ValueSignature}
import cz.cvut.fit.prl.scala.implicits.tools.neo4j.ImplicitType
import cz.cvut.fit.prl.scala.implicits.utils._
import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}
import org.neo4j.graphdb.{Direction, GraphDatabaseService, Node, Transaction}

import scala.collection.JavaConverters._
import scala.collection.mutable

// TODO - it might be possible to store the references to nodes in memory... -
//  should be much faster and this might even help with making the import multithreaded
class Converter(transaction: Transaction) {

  // Avoids passing module node and module entity through every function
  // TODO pass it as arguments - using implicits could make this cleaner
  var currentModuleNode: Option[Node] = None
  var moduleContext: Option[Module] = None
  val UNIT_DECLARATIONID = "scala/Unit#"
  val FUNCTION1_DECLARATIONID = "scala/Function1#"

  private def createSignatureNode(signature: Signature): Node = {
    val signatureNode = createNode(Labels.Signature)

    signature match {
      case MethodSignature(typeParameters, parameterLists, returnType) =>
        addSignatureType(signatureNode, "method")

        typeParameters.foreach(param => {
          val paramNode = mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        parameterLists.foreach(parameterList => {
          val parameterListNode = createNode(Labels.ParameterList)
          parameterList.parameters.foreach(parameter => {
            val parameterNode = createNode(Labels.Parameter, Map("name" -> parameter.name))
            if (parameter.isImplicit) {
              parameterNode.addLabel(Labels.ImplicitParameter)
            }

            val parameterTypeNode = mergeTypeReferenceNode(parameter.tpe)
            parameterNode.createRelationshipTo(parameterTypeNode, Relationships.TYPE)

            parameterListNode.createRelationshipTo(parameterNode, Relationships.HAS_PARAMETER)
          })
          signatureNode.createRelationshipTo(parameterListNode, Relationships.HAS_PARAMETERLIST)
        })
        val returnTypeNode = mergeTypeReferenceNode(returnType)
        signatureNode.createRelationshipTo(returnTypeNode, Relationships.RETURN_TYPE)

      case ClassSignature(typeParameters, parents) =>
        addSignatureType(signatureNode, "class")

        typeParameters.foreach(param => {
          val paramNode = mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        parents.foreach(parent => {
          val parentNode = mergeTypeReferenceNode(parent)
          signatureNode.createRelationshipTo(parentNode, Relationships.PARENT)
        })
      case TypeSignature(typeParameters, upperBound, lowerBound) =>
        addSignatureType(signatureNode, "type")

        typeParameters.foreach(param => {
          val paramNode = mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        if (upperBound.nonEmpty) {
          val upperBoundNode = mergeTypeReferenceNode(upperBound.get)
          signatureNode.createRelationshipTo(upperBoundNode, Relationships.UPPER_BOUND)
        }

        if (lowerBound.nonEmpty) {
          val upperBoundNode = mergeTypeReferenceNode(lowerBound.get)
          signatureNode.createRelationshipTo(upperBoundNode, Relationships.LOWER_BOUND)
        }
      case ValueSignature(tpe) =>
        addSignatureType(signatureNode, "value")

        val valueTypeNode = mergeTypeReferenceNode(tpe)

        signatureNode.createRelationshipTo(valueTypeNode, Relationships.TYPE)
      case _ => throw new IllegalArgumentException("Unexpected signature type")
    }

    signatureNode
  }

  private def mergeImplicitTypeNode(implicitType: ImplicitType.Value): Node =
    mergeNode(Labels.ImplicitType, Map("type" -> implicitType.toString))

  private def addSignatureType(signatureNode: Node, signatureType: String): Unit = {
    val signatureTypeNode = mergeNode(Labels.SignatureType, Map("name" -> signatureType))
    signatureNode.createRelationshipTo(signatureTypeNode, Relationships.SIGNATURE_TYPE)
  }

  private def mergeTypeReferenceNode(tpe: TypeRef): Node = {
    val declarationNode = mergeDeclarationNodeWrapper(moduleContext.get.declarations(tpe.declarationId))

    val typeExpression = getTypeExpression(tpe)

    val typeRefNodeOpt = declarationNode.getRelationships(Direction.INCOMING, Relationships.TYPEREF_DECLARATION)
      .iterator
      .asScala
      .find(_.getStartNode.getProperty("typeExpression") == typeExpression)
      .map(_.getStartNode)

    if (typeRefNodeOpt.nonEmpty) typeRefNodeOpt.get
    else {
      val typeRefNode = createNode(Labels.TypeReference, Map("typeExpression" -> typeExpression))
      typeRefNode.createRelationshipTo(declarationNode, Relationships.TYPEREF_DECLARATION)
      typeRefNode
    }
  }

  // Declaration is not unique in the whole scope - groupId and artifactId could be added to ensure the uniqueness
  // but it is good enough for most purposes and is more performing
  private def getTypeExpression(typeArgument: TypeRef): String = {
    val declarationId = typeArgument.declarationId
    if (typeArgument.typeArguments.isEmpty) declarationId
    else {
      val typeArguments = typeArgument.typeArguments.map(getTypeExpression).mkString("[",",","]")
      declarationId + typeArguments
    }
  }

  private def createDeclarationNode(declaration: Declaration): Node = {
    val properties = Map("declarationId" -> declaration.declarationId,
      "name" -> declaration.name,
      "properties" -> declaration.properties)
    val declarationNode = createNode(Labels.Declaration, properties)


    val accessNode = mergeNode(Labels.Access, Map("name" -> declaration.access.toString()))
    declarationNode.createRelationshipTo(accessNode, Relationships.HAS_ACCESS)

    val languageNode = mergeNode(Labels.Language, Map("name" -> declaration.language.toString()))
    declarationNode.createRelationshipTo(languageNode, Relationships.IN_LANGUAGE)

    val declarationTypeNode = mergeNode(Labels.DeclarationType, Map("name" -> declaration.kind.toString()))
    declarationNode.createRelationshipTo(declarationTypeNode, Relationships.DECLARATION_TYPE)

    declarationNode
  }

  def isImplicitConvFunctionType(returnType: TypeRef): Option[(Node, Node)] = {
    // is function A => B, where A,B is non-unit type
    if (returnType.declarationId != FUNCTION1_DECLARATIONID ||
      returnType.typeArguments.size != 2) {
      None
    }
    else {
      val fromTypeArg = returnType.typeArguments.head
      val toTypeArg = returnType.typeArguments.tail.head

      if (fromTypeArg.declarationId == UNIT_DECLARATIONID || toTypeArg.declarationId == UNIT_DECLARATIONID)
        None
      else
        Some(mergeTypeReferenceNode(fromTypeArg),mergeTypeReferenceNode(toTypeArg))
    }
  }

  def isMethodImplicitConv(parameterLists: Seq[ParameterList], returnType: TypeRef): Option[(Node, Node)] = {
    // is function A=>B with exactly one non-implicit parameter in the first parameter list
    // and zero or more implicit parameters in second parameter list
    if (returnType.declarationId == UNIT_DECLARATIONID)
      return None
    val toTypeRefNode = mergeTypeReferenceNode(returnType)

    if (parameterLists.isEmpty && parameterLists.head.parameters.size != 1)
      return None

    val firstListParameter = parameterLists.head.parameters.head

    if (firstListParameter.isImplicit || firstListParameter.tpe.declarationId == UNIT_DECLARATIONID)
      return None

    val fromTypeRefNode = mergeTypeReferenceNode(firstListParameter.tpe)

    if (parameterLists.size > 2)
      return None

    if (parameterLists.size == 2 && !parameterLists(1).parameters.forall(param => param.isImplicit))
      None
    Some(fromTypeRefNode, toTypeRefNode)
  }

  // returns from/to typereferences of implicit conversion
  private def getImplicitConversion(implicitDeclaration: Declaration): Option[(Node,Node)] = {
    assert(isImplicit(implicitDeclaration))
    implicitDeclaration.signature match {
      case MethodSignature(_, parameterLists, returnType) =>
        val isAnonymous = parameterLists.isEmpty
        if (isAnonymous)
          isImplicitConvFunctionType(returnType)
        else
          isMethodImplicitConv(parameterLists, returnType)
      case ClassSignature(_, parents) =>
        // TODO is this really correct?
        if (parents.size != 1)
          None
        else
          isImplicitConvFunctionType(parents.head)
      case ValueSignature(_) => Option.empty
      case _ => Option.empty
    }
  }

  private def connectDeclaration(declaration: Declaration, declarationNode: Node): Unit = {
    // check whether the signature is not already connected - declaration was connected, when processing different module
    if (declarationNode.hasRelationship(Direction.OUTGOING, Relationships.DECLARATION_SIGNATURE))
      return

    if (isImplicit(declaration)) {
      declarationNode.addLabel(Labels.ImplicitDeclaration)
      getImplicitConversion(declaration).map {
        case (fromNode, toNode) =>
          val implicitTypeNode = mergeImplicitTypeNode(ImplicitType.Conversion)
          declarationNode.createRelationshipTo(implicitTypeNode, Relationships.IMPLICIT_TYPE)
          declarationNode.createRelationshipTo(fromNode, Relationships.CONVERSION_FROM)
          declarationNode.createRelationshipTo(toNode, Relationships.CONVERSION_TO)
        }
    }

    val signatureNode = createSignatureNode(declaration.signature)
    declarationNode.createRelationshipTo(signatureNode, Relationships.DECLARATION_SIGNATURE)

    declaration.annotations.foreach(annotation => {
      val annotationNode = mergeTypeReferenceNode(annotation)
      declarationNode.createRelationshipTo(annotationNode, Relationships.ANNOTATION)
    })
  }

  def isImplicit(declaration: Declaration): Boolean = (declaration.properties & 0x20) != 0

  // Gets declaration node, if it is available or creates new one with the connection to the artifactId and groupId
  // Could be simplified by creating all groupIds and artifacts beforehand
  // TODO Edit - at least create sub-functions
  // TODO refactor java stream to scala iterator - conversion cost?
  // TODO Module declares Declaration relationShip wont be ever created in case module does not declare this declaration
  //  - Is this relation necessary? This relation-ship is already present in form of artifact relation
  private def mergeDeclarationNode(declaration: Declaration, artifactId: String, groupId: String): Node = {
    val module = moduleContext.get
    val groupNodeOpt = transaction.findNodes(Labels.Group, "groupId", groupId).stream()
        .findFirst()
    // group, artifact, declaration needs to be created
    if (groupNodeOpt.isEmpty) {
      val groupNode = createNode(Labels.Group, Map("groupId" -> groupId))
      val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
      val declarationNode = createDeclarationNode(declaration)

      groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
      artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
      if (module.artifactId == artifactId && module.groupId == groupId)
        currentModuleNode.get.createRelationshipTo(declarationNode, Relationships.DECLARES)
      declarationNode
    }
    else {
      val groupNode = groupNodeOpt.get()
      val artifactRelationOpt = StreamSupport.stream(
        groupNode.getRelationships(Direction.OUTGOING, Relationships.GROUP_ARTIFACT).spliterator(), false)
        .filter(relationShip => relationShip.getEndNode.getProperty("artifactId").equals(artifactId))
        .findFirst()


      // artifact, declaration needs to be created
      if (artifactRelationOpt.isEmpty) {
        //
        val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
        val declarationNode = createDeclarationNode(declaration)

        groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
        artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
        if (module.artifactId == artifactId && module.groupId == groupId)
          currentModuleNode.get.createRelationshipTo(declarationNode, Relationships.DECLARES)
        declarationNode
      }
      else {
        val artifactNode = artifactRelationOpt.get.getEndNode

        val declarationRelationOpt = StreamSupport.stream(
          artifactNode.getRelationships(Direction.OUTGOING, Relationships.ARTIFACT_DECLARATION).spliterator(), false)
          .filter(relationShip =>
            relationShip.getEndNode.getProperty("declarationId").equals(declaration.declarationId)
          )
          .findFirst()


        if (declarationRelationOpt.isPresent) {
          declarationRelationOpt.get().getEndNode
        }
        // declaration needs to be created
        else {
          val declarationNode = createDeclarationNode(declaration)

          artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
          if (module.artifactId == artifactId && module.groupId == groupId)
            currentModuleNode.get.createRelationshipTo(declarationNode, Relationships.DECLARES)
          declarationNode
        }
      }
    }
  }

  private def createCallSiteNode(callSite: CallSite): Node = {
    // TODO remove callSiteId?
    val properties = Map(("callSiteId", callSite.callSiteId), ("code", callSite.code))
    val callSiteNode = createNode(Labels.CallSite, properties)

    callSite.typeArguments.foreach(typeArgument => {
      val typeArgumentNode = mergeTypeReferenceNode(typeArgument)
      callSiteNode.createRelationshipTo(typeArgumentNode, Relationships.TYPE_ARGUMENT)
    })


    val declaration = moduleContext.get.declarations(callSite.declarationId)
    val declarationNode = mergeDeclarationNodeWrapper(declaration)
    callSiteNode.createRelationshipTo(declarationNode, Relationships.DECLARED_BY)

    currentModuleNode.get.createRelationshipTo(callSiteNode, Relationships.HAS_CALLSITE)

    callSiteNode
  }

  private def connectCallSite(callSite: CallSite, callSiteNode: Node, callSiteTuples: mutable.Map[Int, (CallSite, Node)]): Unit = {
    callSite.implicitArgumentTypes.foreach {
      case ValueRef(declarationId) =>
        val declarationNode = mergeDeclarationNodeWrapper(moduleContext.get.declarations(declarationId))
        callSiteNode.createRelationshipTo(declarationNode, Relationships.IMPLICIT_VALUEREF)
      case CallSiteRef(callsiteId) =>
        callSiteNode.createRelationshipTo(callSiteTuples(callsiteId)._2, Relationships.IMPLICIT_CALLSITEREF)
      case _ => throw new IllegalArgumentException("Unknown implicit argument found")
    }

    callSite.parentId.fold{}{parentId =>
      // Some with parent Ids do not link to any callsiteId exist!
      callSiteTuples.get(parentId).fold{}{
        case (_, parentNode) => callSiteNode.createRelationshipTo(parentNode, Relationships.PARENT)
      }

    }
  }

  private def mergeDeclarationNodeWrapper(declaration: Declaration): Node = {
    // TODO Does the path needs to be somehow adjusted?
    val path = fromRelativePath(declaration.location.path)
    val module = moduleContext.get
    val entryPath = module.paths(path)

    // TODO - create some path nodes? Or should they be ignored?
    entryPath match {
      case ClasspathEntry(_, groupId, artifactId, _, _, _, _, _) =>
        mergeDeclarationNode(declaration, artifactId, groupId)
      case SourcepathEntry(_, _, _) =>
        mergeDeclarationNode(declaration, module.artifactId, module.groupId)
      case _ => throw new IllegalArgumentException("No path \"" + path + "\" found! ")
    }
  }

  // relative path needs to be adjusted to match paths in module
  private def fromRelativePath(relativePath: String): String = {
    relativePath.lastIndexOf("../") match {
      case -1 => relativePath
      case index => relativePath.substring(index + 2)
    }
  }

  def createModuleNode(module: Module): Node = {
    val moduleProperties = Map(("moduleId", module.moduleId), ("groupId", module.groupId),
      ("scalaVersion", module.scalaVersion), ("artifactId", module.artifactId),("version", module.version),
      ("commit", module.commit))

    val moduleNode = createNode(Labels.Module, moduleProperties)
    currentModuleNode = Option(moduleNode)

    // 1. create declarations
    // 2. create declaration signatures and annotations
    module.declarations.values
      .map(declaration => (declaration, mergeDeclarationNodeWrapper(declaration)))
      .foreach(declarationTuple => (connectDeclaration _).tupled(declarationTuple))


    // callsites ids are unique per module
    // 3. create callsites and bounds to its references
    val callSiteTuples = module.implicitCallSites
      .foldLeft(mutable.Map[Int, (CallSite, Node)]())(
        (map, callSite) => map += callSite.callSiteId -> (callSite, createCallSiteNode(callSite)))

    callSiteTuples.values
      .foreach{
        case (callSite, callSiteNode) => connectCallSite(callSite, callSiteNode, callSiteTuples)
      }

    currentModuleNode = None
    moduleNode
  }

  def createProject(project: Project): Node = {
    val projectProperties = Map(("projectId", project.projectId),("sbtVersion", project.sbtVersion))
    val projectNode = mergeNode(Labels.Project, projectProperties)

    project.modules.foreach {
      case (_, module) =>
        moduleContext = Option(module)
        val moduleNode = createModuleNode(module)
        projectNode.createRelationshipTo(moduleNode, Relationships.HAS_MODULE)
    }

    projectNode
  }

  private def mergeNode(label: Labels, properties: Map[String, Object]): Node =
    transaction.findNodes(label, properties.asJava).stream()
      .findFirst()
      .orElseGet(() => createNode(label, properties))

  private def mergeNode(label:Labels): Node =
    transaction.findNodes(label).stream()
      .findFirst()
      .orElseGet(() => createNode(label))

  private def createNode(label: Labels, properties: Map[String, Any]): Node =
    setNodeProperties(transaction.createNode(label), properties)

  private def createNode(label: Labels): Node =
    transaction.createNode(label)

  private def setNodeProperties(node: Node, properties: Map[String, Any]): Node = {
    properties.foreach(property => node.setProperty(property._1, property._2))
    node
  }
}

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

  def run(): Unit = {
    val DEFAULT_DB_NAME = "neo4j"
    val corporaDir = "/home/panpuncocha/skola/bt/OOPSLA19-artifact/corpora/"
//    val projectDir = corporaDir + "2-single"
//    val implicitsBinRelPath = "/implicits.bin"
    val projectDir = corporaDir + "1-example"
    val implicitsBinRelPath = "/_analysis_/implicits.bin"

    val implicitsPath = projectDir + implicitsBinRelPath

    val dbDirectoryRelPath = "/neo4jDB"
    val dbDirectoryPath = File(projectDir + dbDirectoryRelPath).toJava

    // opening/creating new graph db
    val managementService = new DatabaseManagementServiceBuilder(dbDirectoryPath).build()
    implicit val graphDb: GraphDatabaseService = managementService.database(DEFAULT_DB_NAME)
    registerShutdownHook(managementService)

    cleanUpDatabase

    val transaction: Transaction = graphDb.beginTx()
    val converter = new Converter(transaction)
    try {

      File(implicitsPath).inputStream.apply(
        input => Project.streamFrom(input).foreach(
          project => {
            val time = System.currentTimeMillis()
            converter.createProject(project)
            println(
              s"Converted ${project.projectId} in ${System.currentTimeMillis() - time}ms")
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

  run()
}
