package cz.cvut.fit.prl.scala.implicits

import scala.language.implicitConversions
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

package object model {

  trait ModuleOps {
    protected def moduleId: String

    def module(implicit resolver: ModuleResolver): Module = resolver.module(moduleId)

    def project(implicit resolver: ModuleResolver): Project = module.project

    def projectId(implicit resolver: ModuleResolver): String = project.projectId
  }

  trait LocationOps {
    protected def module(implicit resolver: ModuleResolver): Module

    protected def location: Location

    def isModuleLocal(implicit resolver: ModuleResolver): Boolean = isLocal(module.paths)

    def isProjectLocal(implicit resolver: ModuleResolver): Boolean =
      module.project.modules.values.exists(x => isLocal(x.paths))

    private def isLocal(paths: Map[String, PathEntry]): Boolean = {
      paths
        .get(location.path)
        .collectFirst {
          case _: SourcepathEntry => true
          case x: ClasspathEntry => x.internal
        }
        .getOrElse(false)
    }

    def library(implicit resolver: ModuleResolver): Library = module.paths(location.path) match {
      case _: SourcepathEntry => module.library
      case x: ClasspathEntry => x.library
      case PathEntry.Empty => throw new Exception(s"Calling Empty.library")
    }

    def githubURL(implicit resolver: ModuleResolver): Option[String] = {
      if (isProjectLocal) {
        Some(module.githubURL + "/" + location.relativeUri)
      } else {
        None
      }
    }

    def locationScope(implicit resolver: ModuleResolver): Option[String] =
      module.paths.get(location.path) match {
        case Some(SourcepathEntry(_, scope, _)) => Some(scope)
        case Some(ClasspathEntry(_, _, _, _, scope, true, _, _)) => Some(scope)
        case Some(ClasspathEntry(_, _, _, _, scope, false, _, _)) => Some(scope + "_dependency")
        case _ => None
      }
  }

  implicit class IdentityHashCode(x: AnyRef) {
    def identityHashCode: Int = System.identityHashCode(x)
  }

  implicit class XtensionProject(that: Project) {
    def declarations: Iterable[Declaration] =
      that.modules.values.flatMap(_.declarations.values)
    def implicitDeclarations: Iterable[Declaration] =
      that.modules.values.flatMap(_.implicitDeclarations)
    def implicitCallSites: Iterable[CallSite] =
      that.modules.values.flatMap(_.implicitCallSites)
    def githubUserName: String =
      that.projectId.split("--").apply(0)
    def githubRepoName: String =
      that.projectId.split("--").apply(1)
    def githubURL: String =
      s"https://github.com/${that.githubUserName}/${that.githubRepoName}"
  }

  implicit class XtensionClasspathEntry(that: ClasspathEntry) {
    def library: Library = Library(that.groupId, that.artifactId, that.version)
  }

  implicit class XtensionModule(that: Module) {
    def project(implicit resolver: ModuleResolver): Project =
      resolver.project(that.projectId)
    def library: Library = Library(that.groupId,
      that.artifactId, that.version)
    def githubURL(implicit resolver: ModuleResolver): String =
      project.githubURL
    def compilePaths: Map[String, PathEntry] =
      that.paths.filter(_._2.scope == "compile")
    def testPaths: Map[String, PathEntry] =
      that.paths.filter(_._2.scope == "test")
    def implicitDeclarations: Iterable[Declaration] =
      that.declarations.values.filter(x => x.isImplicit || x.hasImplicitParameters)
  }

  implicit class XtensionParameterList(that: ParameterList) {
    def isImplicit: Boolean = that.parameters.forall(_.isImplicit)
  }

  implicit class XtensionPathEntry(that: PathEntry) {
    def scope: String = that match {
      case SourcepathEntry(_, scope, _) => scope
      case ClasspathEntry(_, _, _, _, scope, _, _, _) => scope
      case PathEntry.Empty => throw new IllegalStateException("Calling PathEntry.Empty.scope")
    }

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
    def isImplicit(implicit resolver: DeclarationResolver): Boolean = {
      val declaration = that.declaration
      declaration.signature.isMethod && (
        declaration.isImplicit || declaration.hasImplicitParameters
      )
    }

    def declarationRef: DeclarationRef = DeclarationRef(that.moduleId, that.declarationId)

    def declaration(implicit resolver: DeclarationResolver): Declaration =
      that.declarationRef.declaration

    override protected def moduleId: String = that.moduleId

    override protected def location: Location = that.location
  }

  implicit class XtensionDeclaration(that: Declaration) extends ModuleOps with LocationOps {
    import Declaration.Kind._

    object declaration {
      def unapply(tpe: Type)(implicit resolver: DeclarationResolver): Option[Declaration] =
        tpe match {
          case TypeRef(declarationId, _) =>
            Some(DeclarationRef(that.moduleId, declarationId).declaration)
          case _ => None
        }
    }

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
    def isImplicitClassCompanionDef(implicit resolver: DeclarationResolver): Boolean =
      that.implicitClassCompanion.isDefined

    def implicitClassCompanion(implicit resolver: DeclarationResolver): Option[Declaration] =
      if (that.isMethod) {
        that.signature.value match {
          // a method that has one parameter and returns an implicit class
          case MethodSignature(_, Seq(_), declaration(ret)) if ret.isImplicit && ret.isClass =>
            Some(ret)
          case _ => None
        }
      } else {
        None
      }

    // TODO: also any MethodSignature that extends FunctionX
    def isFunctionLike: Boolean = that.isMethod

    // cf. https://www.scala-lang.org/files/archive/spec/2.12/04-basic-declarations-and-definitions.html#default-arguments
    // cf. parameters in https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.md
    def isDefaultArgument: Boolean =
      that.isMethod && that.name.matches(".*\\$default\\$\\d+$")

    def typeParameters: Seq[TypeParameter] = that.signature.value match {
      case x: MethodSignature => x.typeParameters
      case x: TypeSignature => x.typeParameters
      case _: ValueSignature => Seq.empty
    }

    def parameterLists: Seq[ParameterList] =
      that.signature.value match {
        case x: MethodSignature => x.parameterLists
        case _ => Seq.empty
      }

    def implicitParameterList: Option[ParameterList] =
      parameterLists.lastOption.filter(_.isImplicit)

    def hasImplicitParameters: Boolean =
      that.implicitParameterList.isDefined

    def returnType(implicit resolver: DeclarationResolver): Option[Declaration] =
      that.signature.value match {
        case MethodSignature(_, _, declaration(rt)) => Some(rt)
        case _ => None
      }

    def declaringTypeRef: DeclarationRef =
      that.signature.value match {
        case _: MethodSignature =>
          DeclarationRef(that.moduleId, that.declarationId.owner)
        case _: ValueSignature =>
          DeclarationRef(that.moduleId, that.declarationId.owner)
        case _ =>
          throw new Exception(s"Trying to get declaringType on ${that.signature}")
      }

    def declaringType(implicit ctx: DeclarationResolver): Declaration =
      declaringTypeRef.declaration

    def parents: Seq[Type] =
      that.signature.value match {
        case x: TypeSignature => x.parents
        case _ =>
          throw new Exception(s"Trying to get parents on ${that.signature}")
      }

    def allParameters: Iterable[Parameter] = parameterLists.flatMap(_.parameters)

    def parameterDeclaration(name: String)(implicit ctx: DeclarationResolver): Declaration = {
      allParameters.find(_.name == name) match {
        case Some(param) => parameterDeclaration(param)
        case _ => throw new IllegalArgumentException(s"Parameter $name does not exist in $this")
      }
    }

    def parameterDeclaration(param: Parameter)(implicit ctx: DeclarationResolver): Declaration = {
      val ref = DeclarationRef(moduleId, param.tpe.declarationId)
      ref.declaration
    }

    def ref: DeclarationRef = DeclarationRef(that.moduleId, that.declarationId)

    override protected def moduleId: String = that.moduleId

    override protected def location: Location = that.location

    def isScalaUnit: Boolean = that.declarationId == "scala/Unit#"

    def isScalaFunction1: Boolean = that.declarationId == "scala/Function1#"

    def isProjectLocal: Boolean = that.location.relativeUri.endsWith(".scala")
  }

  implicit def typeSignature2type(x: TypeSignature): Declaration.Signature.Type =
    Declaration.Signature.Type(x)

  implicit def methodSignature2method(x: MethodSignature): Declaration.Signature.Method =
    Declaration.Signature.Method(x)

  implicit def scalaMetaLanguage2Language(x: s.Language): Language = x match {
    case s.Language.JAVA => Language.JAVA
    case s.Language.SCALA => Language.SCALA
    case _ => Language.UNKNOWN_LANGUAGE
  }
}
