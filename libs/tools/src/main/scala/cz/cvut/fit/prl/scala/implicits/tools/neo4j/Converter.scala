package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{CallSite, CallSiteRef, ClassSignature, ClasspathEntry, Declaration, MethodSignature, Module, ParameterList, Project, Signature, SourcepathEntry, TypeRef, TypeSignature, ValueRef, ValueSignature}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.{Labels, Relationships}
import org.neo4j.graphdb.{Direction, Node, Transaction}

import scala.collection.mutable
import scala.collection.JavaConverters._


// TODO - it might be possible to store the references to nodes in memory... -
//  should be much faster and this might even help with making the import multithreaded
class Converter(transaction: Transaction, proxy: Proxy) {

  // Avoids passing module node and module entity through every function
  // TODO pass it as arguments - using implicits could make this cleaner
  implicit var currentModuleNode: Node = _
  implicit var moduleContext: Module = _
  val UNIT_DECLARATIONID = "scala/Unit#"
  val FUNCTION1_DECLARATIONID = "scala/Function1#"

  private def createSignatureNode(signature: Signature): Node = {
    val signatureNode = createNode(Labels.Signature)

    signature match {
      case MethodSignature(typeParameters, parameterLists, returnType) =>
        addSignatureType(signatureNode, "method")

        typeParameters.foreach(param => {
          val paramNode = proxy.mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        parameterLists.foreach(parameterList => {
          val parameterListNode = createNode(Labels.ParameterList)
          parameterList.parameters.foreach(parameter => {
            val parameterNode = createNode(Labels.Parameter, Map("name" -> parameter.name))
            if (parameter.isImplicit) {
              parameterNode.addLabel(Labels.ImplicitParameter)
            }

            val parameterTypeNode = proxy.mergeTypeReferenceNode(parameter.tpe)
            parameterNode.createRelationshipTo(parameterTypeNode, Relationships.TYPE)

            parameterListNode.createRelationshipTo(parameterNode, Relationships.HAS_PARAMETER)
          })
          signatureNode.createRelationshipTo(parameterListNode, Relationships.HAS_PARAMETERLIST)
        })
        val returnTypeNode = proxy.mergeTypeReferenceNode(returnType)
        signatureNode.createRelationshipTo(returnTypeNode, Relationships.RETURN_TYPE)

      case ClassSignature(typeParameters, parents) =>
        addSignatureType(signatureNode, "class")

        typeParameters.foreach(param => {
          val paramNode = proxy.mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        parents.foreach(parent => {
          val parentNode = proxy.mergeTypeReferenceNode(parent)
          signatureNode.createRelationshipTo(parentNode, Relationships.PARENT)
        })
      case TypeSignature(typeParameters, upperBound, lowerBound) =>
        addSignatureType(signatureNode, "type")

        typeParameters.foreach(param => {
          val paramNode = proxy.mergeTypeReferenceNode(param)
          signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
        })

        if (upperBound.nonEmpty) {
          val upperBoundNode = proxy.mergeTypeReferenceNode(upperBound.get)
          signatureNode.createRelationshipTo(upperBoundNode, Relationships.UPPER_BOUND)
        }

        if (lowerBound.nonEmpty) {
          val upperBoundNode = proxy.mergeTypeReferenceNode(lowerBound.get)
          signatureNode.createRelationshipTo(upperBoundNode, Relationships.LOWER_BOUND)
        }
      case ValueSignature(tpe) =>
        addSignatureType(signatureNode, "value")

        val valueTypeNode = proxy.mergeTypeReferenceNode(tpe)

        signatureNode.createRelationshipTo(valueTypeNode, Relationships.TYPE)
      case _ => throw new IllegalArgumentException("Unexpected signature type")
    }

    signatureNode
  }

  private def mergeImplicitTypeNode(implicitType: ImplicitType.Value): Node =
    mergeNode(Labels.ImplicitType, Map("name" -> implicitType.toString))

  private def addSignatureType(signatureNode: Node, signatureType: String): Unit = {
    val signatureTypeNode = mergeNode(Labels.SignatureType, Map("name" -> signatureType))
    signatureNode.createRelationshipTo(signatureTypeNode, Relationships.SIGNATURE_TYPE)
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
      else {
        val fromNode = proxy.mergeTypeReferenceNode(fromTypeArg)(moduleContext, currentModuleNode)
        val toNode = proxy.mergeTypeReferenceNode(toTypeArg)
        Some(fromNode, toNode)
      }
    }
  }

  def isMethodImplicitConv(parameterLists: Seq[ParameterList], returnType: TypeRef): Option[(Node, Node)] = {
    // is function A=>B with exactly one non-implicit parameter in the first parameter list
    // and zero or more implicit parameters in second parameter list
    if (returnType.declarationId == UNIT_DECLARATIONID)
      return None
    val toTypeRefNode = proxy.mergeTypeReferenceNode(returnType)

    if (parameterLists.isEmpty && parameterLists.head.parameters.size != 1)
      return None

    val firstListParameter = parameterLists.head.parameters.head

    if (firstListParameter.isImplicit || firstListParameter.tpe.declarationId == UNIT_DECLARATIONID)
      return None

    val fromTypeRefNode = proxy.mergeTypeReferenceNode(firstListParameter.tpe)

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
      val annotationNode = proxy.mergeTypeReferenceNode(annotation)
      declarationNode.createRelationshipTo(annotationNode, Relationships.ANNOTATION)
    })
  }

  def isImplicit(declaration: Declaration): Boolean = (declaration.properties & 0x20) != 0


  private def createCallSiteNode(callSite: CallSite): Node = {
    // TODO remove callSiteId?
    val properties = Map(("callSiteId", callSite.callSiteId), ("code", callSite.code))
    val callSiteNode = createNode(Labels.CallSite, properties)

    callSite.typeArguments.foreach(typeArgument => {
      val typeArgumentNode = proxy.mergeTypeReferenceNode(typeArgument)
      callSiteNode.createRelationshipTo(typeArgumentNode, Relationships.TYPE_ARGUMENT)
    })

    val declaration = moduleContext.declarations(callSite.declarationId)
    val declarationNode = mergeDeclarationNodeWrapper(declaration)
    callSiteNode.createRelationshipTo(declarationNode, Relationships.DECLARED_BY)

    currentModuleNode.createRelationshipTo(callSiteNode, Relationships.HAS_CALLSITE)

    callSiteNode
  }

  private def connectCallSite(callSite: CallSite, callSiteNode: Node, callSiteTuples: mutable.Map[Int, (CallSite, Node)]): Unit = {
    callSite.implicitArgumentTypes.foreach {
      case ValueRef(declarationId) =>
        val declaration = moduleContext.declarations(declarationId)
        val declarationNode = mergeDeclarationNodeWrapper(declaration)

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
    val (groupId, artifactId) = Utils.getGroupArtifact(declaration)(moduleContext)
    proxy.mergeDeclarationNode(declaration, artifactId, groupId)
  }


  def createModuleNode(module: Module): Node = {
    val moduleProperties = Map(("moduleId", module.moduleId), ("groupId", module.groupId),
      ("scalaVersion", module.scalaVersion), ("artifactId", module.artifactId),("version", module.version),
      ("commit", module.commit))

    val moduleNode: Node = createNode(Labels.Module, moduleProperties)
    currentModuleNode = moduleNode
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
    currentModuleNode = null
    moduleNode
  }

  def createProject(project: Project): Node = {
    val projectProperties = Map(("projectId", project.projectId),("sbtVersion", project.sbtVersion))
    val projectNode = mergeNode(Labels.Project, projectProperties)

    project.modules.foreach {
      case (_, module) =>
        moduleContext = module
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

object Converter {
  def apply(transaction: Transaction, cache: NodesCache): Converter = new Converter(transaction, new Proxy(transaction,cache))
  def apply(transaction: Transaction): Converter = new Converter(transaction, new Proxy(transaction, NodesCache()))
}
