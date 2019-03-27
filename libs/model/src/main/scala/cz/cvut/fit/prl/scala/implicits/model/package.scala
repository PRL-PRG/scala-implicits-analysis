package cz.cvut.fit.prl.scala.implicits

import scala.language.implicitConversions
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb => s}
import scala.util.matching.Regex

package object model {

  val BlockLocalIdSep: String = "::"
  val BlockLocalIdPattern: Regex = s"^local\\d+$BlockLocalIdSep\\d+$$".r

  object declarationIds {
    val Unit = "scala/Unit#"
    val Function1 = "scala/Function1#"
  }

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
    def library: Library =
      Library(that.groupId, that.artifactId, that.version)
    def githubURL(implicit resolver: ModuleResolver): String =
      project.githubURL
    def compilePaths: Map[String, PathEntry] =
      that.paths.filter(_._2.scope == "compile")
    def testPaths: Map[String, PathEntry] =
      that.paths.filter(_._2.scope == "test")
    def implicitDeclarations: Iterable[Declaration] =
      that.declarations.values.filter(x => x.isImplicit || x.hasImplicitParameters)
    def resolve(declarationId: String)(implicit idx: Index): Declaration =
      idx.resolveDeclaration(that.moduleId, declarationId)
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
      val d = that.declaration
      d.hasMethodSignature && (d.isImplicit || d.hasImplicitParameters)
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
    def hasMethodSignature: Boolean = that.signature match {
      case _: MethodSignature => true
      case _ => false
    }
    def hasTypeSignature: Boolean = that.signature match {
      case _: TypeSignature => true
      case _ => false
    }
    def hasClassSignature: Boolean = that.signature match {
      case _: ClassSignature => true
      case _ => false
    }

    def methodSignature: MethodSignature = that.signature match {
      case x: MethodSignature => x
      case _ => throw new IllegalStateException(s"Calling `methodSignature` on $that")
    }

    def classSignature: ClassSignature = that.signature match {
      case x: ClassSignature => x
      case _ => throw new IllegalStateException(s"Calling `classSignature` on $that")
    }

    def typeSignature: TypeSignature = that.signature match {
      case x: TypeSignature => x
      case _ => throw new IllegalStateException(s"Calling `typeSignature` on $that")
    }

    def valueSignature: ValueSignature = that.signature match {
      case x: ValueSignature => x
      case _ => throw new IllegalStateException(s"Calling `valueSignature` on $that")
    }

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
        that.signature match {
          case MethodSignature(_, ImplicitConversionParameters(_, _), declaration(ret))
              if ret.isImplicit && ret.isClass =>
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

    def typeParameters: Seq[TypeParameter] = that.signature match {
      case x: MethodSignature => x.typeParameters
      case x: ClassSignature => x.typeParameters
      case x: TypeSignature => x.typeParameters
      case _ => Seq.empty
    }

    def parameterLists: Seq[ParameterList] =
      that.signature match {
        case x: MethodSignature => x.parameterLists
        case _ => Seq.empty
      }

    def implicitParameterList: Option[ParameterList] =
      parameterLists.lastOption.filter(_.isImplicit)

    def hasImplicitParameters: Boolean =
      that.implicitParameterList.isDefined

    def returnDeclaration(implicit resolver: DeclarationResolver): Option[Declaration] =
      that.signature match {
        case MethodSignature(_, _, declaration(rt)) => Some(rt)
        case _ => None
      }

    def returnType: Option[Type] =
      that.signature match {
        case MethodSignature(_, _, rt) => Some(rt)
        case _ => None
      }

    def declaringTypeRef: DeclarationRef =
      that.signature match {
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
      that.signature match {
        case x: ClassSignature => x.parents
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

    def isProjectLocal: Boolean = that.location.relativeUri.endsWith(".scala")

    def isImplicit: Boolean = (that.properties & p.IMPLICIT.value) != 0

    def isLazy: Boolean = (that.properties & p.LAZY.value) != 0

    def isTypeOf(declarationId: String)(implicit resolver: DeclarationResolver): Boolean = {
      if (that.declarationId == declarationId) {
        true
      } else if (that.isType) {
        that.resolveTypeDeclaration.exists(_.isTypeOf(declarationId))
      } else {
        false
      }
    }

    def isKindOf(declarationId: String)(implicit resolver: DeclarationResolver): Boolean = {
      if (that.declarationId == declarationId) {
        true
      } else if (that.hasClassSignature) {
        that.parentsDeclarations.exists(_.isKindOf(declarationId))
      } else if (that.hasTypeSignature) {
        that.resolveTypeDeclaration.exists(_.isKindOf(declarationId))
      } else {
        false
      }
    }

    def isScalaUnit(implicit resolver: DeclarationResolver): Boolean = isTypeOf(declarationIds.Unit)

    def isKindOfScalaFunction1(implicit resolver: DeclarationResolver): Boolean =
      isKindOf(declarationIds.Function1)

    def isBlockLocal: Boolean = BlockLocalIdPattern.findFirstMatchIn(that.declarationId).isDefined

    def compilationUnit: Option[String] = {
      import s.Scala.ScalaSymbolOps

      def isCompilationUnit(s: String) =
        s.isType || (s.isTerm && !(s.endsWith(").") || s.endsWith("].")))

      def find(d: String, last: Option[String]): Option[String] = {
        if (d.isNone) {
          last
        } else {
          val next = if (isCompilationUnit(d)) Some(d) else last
          find(d.owner, next)
        }
      }

      find(that.declarationId, None)
    }

    def resolveTypeDeclaration(implicit resolver: DeclarationResolver): Option[Declaration] = {
      def resolveToClass(d: Declaration): Option[Declaration] = d.signature match {
        case _: ClassSignature =>
          Some(d)
        case tpe: TypeSignature =>
          val nextId = (tpe.lowerBound, tpe.upperBound) match {
            case (Type.Empty, Type.Empty) => None
            case (Type.Empty, x) => Some(x.declarationId)
            case (x, Type.Empty) => Some(x.declarationId)
            case (x, y) if x == y => Some(x.declarationId)
          }

          nextId match {
            case Some(v) => resolveToClass(resolver.resolveDeclaration(that.moduleId, v))
            case None => None
          }
        case _ =>
          None
      }

      resolveToClass(that)
    }

    def parentsDeclarations(implicit resolver: DeclarationResolver): Iterable[Declaration] =
      that.signature match {
        case clazz: ClassSignature =>
          clazz.parents
            .map(x => DeclarationRef(that.moduleId, x.declarationId))
            .map(resolver.resolveDeclaration)
        case _ =>
          Iterable.empty
      }

    def allParents(implicit resolver: DeclarationResolver): Stream[Declaration] =
      that.signature match {
        case _: ClassSignature =>
          val xs = that.parentsDeclarations.toStream
          (xs #::: xs.flatMap(_.parentsDeclarations)).distinct
        case _ =>
          Stream.empty
      }
  }

  implicit def scalaMetaLanguage2Language(x: s.Language): Language = x match {
    case s.Language.JAVA => Language.JAVA
    case s.Language.SCALA => Language.SCALA
    case _ => Language.UNKNOWN_LANGUAGE
  }

  object ImplicitConversionParameters {
    def unapply(arg: Seq[ParameterList]): Option[(Parameter, Seq[Parameter])] = arg match {
      case Seq(ParameterList(Seq(p @ Parameter(_, _, false)))) =>
        // one parameter
        Some((p, Seq.empty))
      case Seq(ParameterList(Seq(p @ Parameter(_, _, false))), secondList)
          if secondList.isImplicit =>
        // or one parameter and any number of implicit parameters
        Some((p, secondList.parameters))
      case _ =>
        None
    }
  }
}
