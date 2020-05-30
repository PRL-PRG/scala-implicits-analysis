package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{CallSite, CallSiteRef, ClassSignature, Declaration, MethodSignature, Module, ParameterList, Project, Signature, TypeRef, TypeSignature, ValueRef, ValueSignature}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.{Labels, Relationships}
import org.neo4j.graphdb.{Direction, Node, Transaction}

import scala.collection.mutable


class Converter(proxy: Proxy) {

  // Avoids passing module node and module entity through every function
  // TODO pass it as arguments - using implicits could make this cleaner
  implicit var currentModuleNode: Node = _
  implicit var moduleContext: Module = _
  implicit var transaction: Transaction = _

  def createProject(project: Project, transaction: Transaction): Node = {
    this.transaction = transaction
    val projectProperties = Map(("projectId", project.projectId),("sbtVersion", project.sbtVersion))
    val projectNode = proxy.createNode(Labels.Project, projectProperties)(transaction)

    project.modules.foreach {
      case (_, module) =>
        moduleContext = module
        val moduleNode = createModuleNode(module)
        projectNode.createRelationshipTo(moduleNode, Relationships.HAS_MODULE)
    }

    projectNode
  }

  def initiateDatabase(transaction: Transaction): Unit = {
    proxy.createUnknownDeclarationNode(transaction)
  }

  private def createModuleNode(module: Module): Node = {
    val moduleProperties = Map(("moduleId", module.moduleId), ("groupId", module.groupId),
      ("scalaVersion", module.scalaVersion), ("artifactId", module.artifactId),("version", module.version),
      ("commit", module.commit))

    val moduleNode: Node = proxy.createNode(Labels.Module, moduleProperties)
    currentModuleNode = moduleNode
    // 1. create declarations
    // 2. create declaration signatures and annotations
    module.declarations.values
      .map(declaration => (declaration, mergeDeclarationNodeWrapper(declaration)))
      .foreach(declarationTuple => (connectDeclaration _).tupled(declarationTuple))


    // 3. create callsites
    // 4. bind callsites to its references
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

  private def createSignatureNode(signature: Signature): Node = {
    val signatureNode = proxy.createNode(Labels.Signature)

    signature match {
      case MethodSignature(typeParameters, parameterLists, returnType) =>
        createMethodSignature(signatureNode, typeParameters, parameterLists, returnType)
      case ClassSignature(typeParameters, parents) =>
        createClassSignature(signatureNode, typeParameters, parents)
      case TypeSignature(typeParameters, upperBound, lowerBound) =>
        createTypeSignature(signatureNode, typeParameters, upperBound, lowerBound)
      case ValueSignature(tpe) =>
        createValueSignature(signatureNode, tpe)
      case _ => throw new IllegalArgumentException("Unexpected signature type")
    }

    signatureNode
  }

  private def createMethodSignature(signatureNode: Node, typeParameters: Seq[TypeRef],
                                    parameterLists: Seq[ParameterList],
                                    returnType: TypeRef): Unit = {
    addSignatureType(signatureNode, "method")

    typeParameters.foreach(param => {
      val paramNode = proxy.mergeTypeReferenceNode(param)
      signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
    })

    parameterLists.foreach(parameterList => {
      val parameterListNode = proxy.createNode(Labels.ParameterList)
      parameterList.parameters.foreach(parameter => {
        val parameterNode = proxy.createNode(Labels.Parameter, Map("name" -> parameter.name))
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
  }

  private def createClassSignature(signatureNode: Node,
                                  typeParameters: Seq[TypeRef],
                                  parents: Seq[TypeRef]): Unit = {
    addSignatureType(signatureNode, "class")

    typeParameters.foreach(param => {
      val paramNode = proxy.mergeTypeReferenceNode(param)
      signatureNode.createRelationshipTo(paramNode, Relationships.TYPE_PARAMETER)
    })

    parents.foreach(parent => {
      val parentNode = proxy.mergeTypeReferenceNode(parent)
      signatureNode.createRelationshipTo(parentNode, Relationships.PARENT)
    })
  }

  private def createTypeSignature(signatureNode: Node, typeParameters: Seq[TypeRef], upperBound: Option[TypeRef],
                                  lowerBound: Option[TypeRef]): Unit = {
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
  }

  private def createValueSignature(signatureNode: Node, tpe: TypeRef): Unit = {
    addSignatureType(signatureNode, "value")

    val valueTypeNode = proxy.mergeTypeReferenceNode(tpe)

    signatureNode.createRelationshipTo(valueTypeNode, Relationships.TYPE)
  }

  private def addSignatureType(signatureNode: Node, signatureType: String): Unit = {
    val signatureTypeNode = proxy.mergeNode(Labels.SignatureType, Map("name" -> signatureType))
    signatureNode.createRelationshipTo(signatureTypeNode, Relationships.SIGNATURE_TYPE)
  }


  private def getAnonymousConversion(returnType: TypeRef): Option[(Node,Node)] = {
    if (ModelLogic.isImplicitConvFunction(returnType)) {
      val (fromType, toType) = ModelLogic.getConversionTypes(returnType)

      val fromNode = proxy.mergeTypeReferenceNode(fromType)
      val toNode = proxy.mergeTypeReferenceNode(toType)
      Some(fromNode, toNode)
    }
    else
      None
  }

  private def getMethodConversion(parameterLists: Seq[ParameterList], returnType: TypeRef): Option[(Node,Node)] = {
    if (ModelLogic.isImplicitConvMethod(parameterLists, returnType)) {
      val (fromType, toType) = ModelLogic.getConversionTypes(parameterLists, returnType)

      val fromTypeRefNode = proxy.mergeTypeReferenceNode(fromType)
      val toTypeRefNode = proxy.mergeTypeReferenceNode(toType)
      Some(fromTypeRefNode, toTypeRefNode)
    }
    else
      None
  }


  // returns from/to typereferences of implicit conversion
  private def getImplicitConversion(implicitDeclaration: Declaration): Option[(Node,Node)] = {
    assert(ModelLogic.isImplicit(implicitDeclaration))
    implicitDeclaration.signature match {
      case MethodSignature(_, parameterLists, returnType) =>
        val isAnonymous = parameterLists.isEmpty
        if (isAnonymous)
          getAnonymousConversion(returnType)
        else
          getMethodConversion(parameterLists, returnType)
      case ClassSignature(_, parents) =>
        // TODO is this really correct?
        if (parents.size != 1)
          None
        else
          getAnonymousConversion(parents.head)
      case ValueSignature(tpe) => getAnonymousConversion(tpe)
      case _ => None
    }
  }

  private def connectDeclaration(declaration: Declaration, declarationNode: Node): Unit = {
    // check whether the signature is not already connected -
    // declaration might have been connected, when processing different module was processed
    if (declarationNode.hasRelationship(Direction.OUTGOING, Relationships.DECLARATION_SIGNATURE))
      return

    if (ModelLogic.isImplicit(declaration)) {
      declarationNode.addLabel(Labels.ImplicitDeclaration)
      getImplicitConversion(declaration).map {
        case (fromNode, toNode) =>
          declarationNode.addLabel(Labels.ImplicitConversion)
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

  private def createCallSiteNode(callSite: CallSite): Node = {
    val properties = Map(("code", callSite.code))
    val callSiteNode = proxy.createNode(Labels.CallSite, properties)

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

        callSiteNode.createRelationshipTo(declarationNode, Relationships.HAS_IMPLICIT_ARGUMENT_VALUEREF)
      case CallSiteRef(callsiteId) =>
        callSiteNode.createRelationshipTo(callSiteTuples(callsiteId)._2, Relationships.HAS_IMPLICIT_ARGUMENT_CALLSITEREF)
      case _ => throw new IllegalArgumentException("Unknown implicit argument found")
    }

    callSite.parentId.fold{}{parentId =>
      // Some callsite parent Ids do not link to any callsiteId exist!
      callSiteTuples.get(parentId).fold{}{
        case (_, parentNode) => callSiteNode.createRelationshipTo(parentNode, Relationships.PARENT)
      }
    }
  }

  private def mergeDeclarationNodeWrapper(declaration: Declaration): Node = {
    val (groupId, artifactId) = ModelLogic.getGroupArtifact(declaration)(moduleContext)
    proxy.mergeDeclarationNode(declaration, artifactId, groupId)
  }
}

object Converter {
  def apply(cache: NodesCache): Converter = new Converter(new Proxy(cache))
  def apply(): Converter = new Converter(new Proxy(NodesCache()))
}
