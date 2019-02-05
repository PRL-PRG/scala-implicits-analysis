package cz.cvut.fit.prl.scala.implicits

import scala.language.implicitConversions
import scala.meta.internal.semanticdb.Scala._

package object model {

  case class DeclarationRef(moduleId: String, declarationFqn: String) {
    def declaration(implicit resolver: TypeResolver): Declaration =
      resolver.resolveType(this)
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
    def paths: Map[String, PathEntry] = that.modules.foldLeft(Map[String, PathEntry]())(_ ++ _.paths)
  }

  implicit class XtensionLocation(that: Location) {
    def project(implicit idx: Index): Project = idx.project(that)
    def module(implicit idx: Index): Module = idx.module(that)
    def githubURL(implicit idx: Index): String = project.githubURL + "/" + that.relativeUri
  }

  implicit class XtensionModule(that: Module) {
    def project(implicit idx: Index): Project = idx.project(that)
    def library: Library = Library(that.groupId, that.artifactId, that.version)
    def githubURL(implicit idx: Index): String = project.githubURL
  }

  implicit class XtensionParameterList(that: ParameterList) {
    def isImplicit: Boolean = that.parameters.exists(_.isImplicit)
  }

  implicit class XtenstionPathEntry(that: PathEntry) {
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

    def declarationFqn: String = that match {
      case TypeRef(r, _) => r
      case TypeParameterRef(r, _, _) => r
      case _ => throw new Exception(s"Trying to get declarationRef on $that")
    }
  }

  implicit class XtensionTypeRef(that: TypeRef) {

    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.declarationFqn + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionTypeParameterRef(that: TypeParameterRef) {

    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.name + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionCallSite(that: CallSite) {

    def isImplicit(implicit resolver: TypeResolver): Boolean = {
      val declaration = that.declaration
      declaration.signature.isMethod && (
        declaration.isImplicit || declaration.hasImplicitParameters
      )
    }

    def declarationRef: DeclarationRef = DeclarationRef(that.moduleId, that.declarationFqn)

    def declaration(implicit resolver: TypeResolver): Declaration =
      that.declarationRef.declaration
  }

  implicit class XtensionDeclaration(that: Declaration) {
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
    * @param resolver
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

    def returnType(implicit resolver: TypeResolver): Declaration =
      that.signature.value match {
        case x: MethodSignature =>
      resolver.resolveType(DeclarationRef(that.moduleId, x.returnType.declarationFqn))
        case _ =>
          throw new Exception(s"Trying to get returnType on ${that.signature}")
      }

    def declaringType(implicit ctx: TypeResolver): Declaration =
      that.signature.value match {
        case _: MethodSignature => ctx.resolveType(DeclarationRef(that.moduleId, that.fqn.owner))
        case _: ValueSignature => ctx.resolveType(DeclarationRef(that.moduleId, that.fqn.owner))
        case _ =>
          throw new Exception(s"Trying to get declaringType on ${that.signature}")
      }

    def parents: Seq[Type] =
      that.signature.value match {
        case x: TypeSignature => x.parents
        case _ =>
          throw new Exception(s"Trying to get parents on ${that.signature}")
      }

    def module(implicit idx: Index): Module = idx.modules(that.moduleId)

    def project(implicit idx: Index): Project = that.module.project

    def ref: DeclarationRef = DeclarationRef(that.moduleId, that.fqn)

    def isModuleLocal(implicit idx: Index): Boolean = isLocal(that.module.paths)

    def isProjectLocal(implicit idx: Index): Boolean = isLocal(that.project.paths)

    private def isLocal(paths: Map[String, PathEntry]): Boolean = {
    paths
      .get(that.location.path)
      .collectFirst {
        case _: SourcepathEntry => true
        case x: ClasspathEntry => x.internal
      }
      .getOrElse(false)
  }
  }

  implicit def typeSignature2type(x: TypeSignature): Declaration.Signature.Type =
    Declaration.Signature.Type(x)

  implicit def methodSignature2method(x: MethodSignature): Declaration.Signature.Method =
    Declaration.Signature.Method(x)
}
