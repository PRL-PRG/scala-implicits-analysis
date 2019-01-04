package cz.cvut.fit.prl.scala.implicits

import scala.meta.internal.semanticdb.Scala._

package object model {

  implicit class XtensionProject(that: Project) {
    def declarations = that.modules.flatMap(_.declarations)
    def implicitCallSites = that.modules.flatMap(_.implicitCallSites)
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

    def declarationRef: String = that match {
      case TypeRef(r, _) => r
      case TypeParameterRef(r, _, _) => r
      case _ => throw new Exception(s"Trying to get declarationRef on $that")
    }

    def declaration(implicit resolver: TypeResolver): Declaration =
      resolver.resolveType(that)
  }

  implicit class XtensionTypeRef(that: TypeRef) {

    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.declarationRef + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionTypeParameterRef(that: TypeParameterRef) {

    def asCode: String = {
      val args = that.typeArguments.map(_.asCode)
      that.name + (if (args.nonEmpty) args.mkString("[", ",", "]") else "")
    }
  }

  implicit class XtensionCallSite(that: CallSite) {

    def isImplicit(implicit resolver: DeclarationResolver): Boolean = {
      val declaration = that.declaration
      declaration.signature.isMethod && (
        declaration.isImplicit || declaration.hasImplicitParameters
      )
    }

    def declaration(implicit resolver: DeclarationResolver): Declaration =
      that.declarationRef.declaration
  }

  implicit class DeclarationRefOPs(that: String) {
    def declaration(implicit resolver: DeclarationResolver): Declaration =
      resolver.resolveDeclaration(that)
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
        case x: MethodSignature => x.returnType.declaration
        case _ =>
          throw new Exception(s"Trying to get returnType on ${that.signature}")
      }

    def declaringType(implicit ctx: DeclarationResolver): Declaration =
      that.signature.value match {
        case x: MethodSignature => ctx.resolveDeclaration(that.fqn.owner)
        case _ =>
          throw new Exception(s"Trying to get declaringType on ${that.signature}")
      }

    def parents: Seq[Type] =
      that.signature.value match {
        case x: TypeSignature => x.parents
        case _ =>
          throw new Exception(s"Trying to get parents on ${that.signature}")
      }
  }
}
