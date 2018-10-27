package cz.cvut.fit.prl.scala.implicits

import scala.meta.internal.semanticdb.Scala._

package object model {

  implicit class XtensionParameterList(that: ParameterList) {
    def isImplicit: Boolean = that.parameters.exists(_.isImplicit)
  }

  implicit class XtensionLocation(that: Location) {

    def isLocal: Boolean = that match {
      case _: Local => true
      case _        => false
    }
  }

  implicit class XtensionType(that: Type) {

    def ref: DeclarationRef = that match {
      case TypeRef(r, _)             => r
      case TypeParameterRef(r, _, _) => r
    }

    def declaration(implicit resolver: TypeResolver): Declaration = resolver.resolveType(that)
  }

  implicit class XtensionCallSite(that: CallSite) {

    def isImplicit(implicit resolver: DeclarationResolver): Boolean = {
      val declaration = that.ref.declaration
      declaration.signature.isMethod && (
        declaration.isImplicit || declaration.hasImplicitParameters
      )
    }
  }

  implicit class XtensionDeclarationRef(that: DeclarationRef) {
    def declaration(implicit resolver: DeclarationResolver): Declaration = resolver.resolveDeclaration(that.fqn)
  }

  implicit class XtensionDeclaration(that: Declaration) {
    import Declaration.Kind._

    def ref: DeclarationRef = DeclarationRef(that.location, that.fqn)

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

    def typeParameters: Seq[TypeParameter] = that.signature.value match {
      case x: MethodSignature => x.typeParameters
      case x: TypeSignature   => x.typeParameters
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
