package cz.cvut.fit.prl.scala.implicits

import scala.language.implicitConversions
import scala.meta.internal.semanticdb.Scala._

package object model {

  trait ModuleOps {
    protected def moduleId: String

    def module(implicit idx: Index): Module = idx.modules(moduleId)

    def project(implicit idx: Index): Project = module.project
  }

  trait LocationOps {
    protected def module(implicit idx: Index): Module

    protected def location: Location

    def isModuleLocal(implicit idx: Index): Boolean = isLocal(module.paths)

    def isProjectLocal(implicit idx: Index): Boolean = isLocal(module.project.paths)

    private def isLocal(paths: Map[String, PathEntry]): Boolean = {
      paths
        .get(location.path)
        .collectFirst {
          case _: SourcepathEntry => true
          case x: ClasspathEntry => x.internal
        }
        .getOrElse(false)
    }

    def library(implicit idx: Index): Library = module.paths(location.path) match {
      case _: SourcepathEntry => module.library
      case x: ClasspathEntry => x.library
      case PathEntry.Empty => throw new Exception(s"Calling Empty.library")
    }

    def githubURL(implicit idx: Index): Option[String] = {
      if (isProjectLocal) {
        Some(module.githubURL + "/" + location.relativeUri)
      } else {
        None
      }
    }
  }

  implicit class IdentityHashCode(x: AnyRef) {
    def identityHashCode: Int = System.identityHashCode(x)
  }

  implicit class XtensionProject(that: Project) {
    def declarations: Iterable[Declaration] = that.modules.flatMap(_.declarations.values)
    def implicitCallSites: Iterable[CallSite] = that.modules.flatMap(_.implicitCallSites)
    def githubUserName: String = that.projectId.split("--").apply(0)
    def githubRepoName: String = that.projectId.split("--").apply(1)
    def githubURL: String = s"https://github.com/${that.githubUserName}/${that.githubRepoName}"
    def paths: Map[String, PathEntry] =
      that.modules.foldLeft(Map[String, PathEntry]())(_ ++ _.paths)
  }

  implicit class XtensionClasspathEntry(that: ClasspathEntry) {
    def library: Library = Library(that.groupId, that.artifactId, that.version)
  }

  implicit class XtensionModule(that: Module) {
    def project(implicit idx: Index): Project = idx.projects(that.moduleId)
    def library: Library = Library(that.groupId, that.artifactId, that.version)
    def githubURL(implicit idx: Index): String = project.githubURL
  }

  implicit class XtensionParameterList(that: ParameterList) {
    def isImplicit: Boolean = that.parameters.exists(_.isImplicit)
  }

  implicit class XtensionPathEntry(that: PathEntry) {
    def path: String = that match {
      case x: SourcepathEntry => x.path
      case x: ClasspathEntry => x.path
      case _ => throw new Exception(s"Trying to get path on $that")
    }
  }

  implicit class XtensionType(that: Type) {
    def asCode: String = that match {
      case x: TypeRef => x.asCode
      case x: TypeParameterRef => x.asCode
      case Type.Empty => ""
    }

    def declarationId: String = that match {
      case TypeRef(r, _) => r
      case TypeParameterRef(r, _, _) => r
      case _ => throw new Exception(s"Trying to get declarationId on $that")
    }
  }

  implicit class XtensionTypeRef(that: TypeRef) {
    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.declarationId + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionTypeParameterRef(that: TypeParameterRef) {
    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.name + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionCallSite(that: CallSite) extends ModuleOps with LocationOps {
    def isImplicit(implicit resolver: TypeResolver): Boolean = {
      val declaration = that.declaration
      declaration.signature.isMethod && (
        declaration.isImplicit || declaration.hasImplicitParameters
      )
    }

    def declarationRef: DeclarationRef = DeclarationRef(that.moduleId, that.declarationId)

    def declaration(implicit resolver: TypeResolver): Declaration =
      that.declarationRef.declaration

    override protected def moduleId: String = that.moduleId

    override protected def location: Location = that.location
  }

  implicit class XtensionDeclaration(that: Declaration) extends ModuleOps with LocationOps {
    import Declaration.Kind._

    def isMethod: Boolean = that.kind == DEF
    def isVal: Boolean = that.kind == VAL
    def isVar: Boolean = that.kind == VAR
    def isType: Boolean = that.kind == TYPE
    def isClass: Boolean = that.kind == CLASS
    def isTrait: Boolean = that.kind == TRAIT
    def isObject: Boolean = that.kind == OBJECT
    def isInterface: Boolean = that.kind == INTERFACE
    def isEnum: Boolean = that.kind == ENUM
    def isMacro: Boolean = that.kind == MACRO
    def isMethodDeclaration: Boolean = that.signature.isMethod
    def isTypeDeclaration: Boolean = that.signature.isType

    /**
      * The following code will create two implicit definitions
      * {{{
      *   implicit class A(x: Int) { ... }
      * }}}
  *
  * 1. an implicit class `A#`
  * 2. an implicit def `A().`
  *
  * This method identifies such declarations for the def part.
  *
  * @param resolver to be used to resolve declarations
  * @return
  */
    def isImplicitClassCompanionDef(implicit resolver: TypeResolver): Boolean =
      that.implicitClassCompanion.isDefined

    def implicitClassCompanion(implicit resolver: TypeResolver): Option[Declaration] =
      if (that.isMethod && that.parameterLists.headOption.exists(_.parameters.size == 1)) {
        val rt = that.returnType
        if (rt.isImplicit && rt.isClass) {
          Some(rt)
        } else {
          None
        }
      } else {
        None
      }

    // TODO: also any MethodSignature that extends FunctionX
    def isFunctionLike: Boolean = that.isMethod

    def typeParameters: Seq[TypeParameter] = that.signature.value match {
      case x: MethodSignature => x.typeParameters
      case x: TypeSignature => x.typeParameters
    }

    def parameterLists: Seq[ParameterList] =
      that.signature.value match {
        case x: MethodSignature => x.parameterLists
        case _ =>
          throw new Exception(
            s"Trying to get parameterList on ${that.signature}"
          )
      }

    def hasImplicitParameters: Boolean =
      that.parameterLists.exists(_.isImplicit)

    def returnTypeRef: DeclarationRef =
      that.signature.value match {
        case x: MethodSignature =>
          DeclarationRef(that.moduleId, x.returnType.declarationId)
        case _ =>
          throw new Exception(s"Trying to get returnType on ${that.signature}")
      }

    def returnType(implicit resolver: TypeResolver): Declaration =
      returnTypeRef.declaration

    def declaringTypeRef: DeclarationRef =
      that.signature.value match {
        case _: MethodSignature =>
          DeclarationRef(that.moduleId, that.declarationId.owner)
        case _: ValueSignature =>
          DeclarationRef(that.moduleId, that.declarationId.owner)
        case _ =>
          throw new Exception(s"Trying to get declaringType on ${that.signature}")
      }

    def declaringType(implicit ctx: TypeResolver): Declaration =
      declaringTypeRef.declaration

    def parents: Seq[Type] =
      that.signature.value match {
        case x: TypeSignature => x.parents
        case _ =>
          throw new Exception(s"Trying to get parents on ${that.signature}")
      }

    def ref: DeclarationRef = DeclarationRef(that.moduleId, that.declarationId)

    override protected def moduleId: String = that.moduleId

    override protected def location: Location = that.location
  }

  implicit def typeSignature2type(x: TypeSignature): Declaration.Signature.Type =
    Declaration.Signature.Type(x)

  implicit def methodSignature2method(x: MethodSignature): Declaration.Signature.Method =
    Declaration.Signature.Method(x)
}
