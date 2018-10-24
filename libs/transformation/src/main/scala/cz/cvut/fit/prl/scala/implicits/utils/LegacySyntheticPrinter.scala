package cz.cvut.fit.prl.scala.implicits.utils

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

class LegacySyntheticPrinter {
  case class PositionedSymbol(symbol: String, start: Int, end: Int)

  private val buf = List.newBuilder[PositionedSymbol]
  private val text = new StringBuilder

  private def emit(symbol: String): Unit = {
    emitCode(symbol.desc.value, symbol)
  }

  private def emitCode(code: String, sym: String): Unit = {
    val start = text.length
    text.append(code)
    val end = text.length
    buf += PositionedSymbol(sym, start, end)
  }
  private def mkString[T](start: String, trees: Seq[T], end: String)(
    fn: T => Unit): Unit = {
    if (trees.isEmpty) ()
    else {
      text.append(start)
      var first = true
      trees.foreach { tree =>
        fn(tree)
        if (first) {
          first = false
        } else {
          text.append(", ")
        }
      }
      text.append(end)
    }
  }

  private def pprint(sig: s.Signature): Unit = sig match {
    case s.ValueSignature(tpe) =>
      pprint(tpe)
    case _ =>
  }
  private def pprint(tpe: s.Type): Unit = tpe match {
    case s.TypeRef(prefix, symbol, typeArguments) =>
      prefix match {
        case s.NoType =>
        case _ =>
          pprint(prefix)
          text.append(".")
      }
      emit(symbol)
      mkString("[", typeArguments, "]")(pprint)
    // TODO(olafur): Print out more advanced types https://github.com/scalacenter/scalafix/issues/785
    case s.SingleType(prefix, symbol) =>
      pprint(prefix)
      emit(symbol)
    case s.ThisType(symbol) =>
      emit(symbol)
    case s.SuperType(prefix, symbol) =>
      pprint(prefix)
      emit(symbol)
    case s.ConstantType(constant) =>
      pprint(constant)
    case s.IntersectionType(types) =>
      types.foreach(pprint)
    case s.UnionType(types) =>
      types.foreach(pprint)
    case s.WithType(types) =>
      types.foreach(pprint)
    case s.StructuralType(tpe, declarations) =>
      pprint(tpe)
      declarations.foreach { s =>
        s.symbols.foreach(emit)
        s.hardlinks.foreach(info => pprint(info.signature))
      }
    case s.AnnotatedType(_, tpe) =>
      pprint(tpe)
    case s.ExistentialType(tpe, declarations) =>
      pprint(tpe)
      declarations.foreach { s =>
        s.symbols.foreach(emit)
        s.hardlinks.foreach(info => pprint(info.signature))
      }
    case s.UniversalType(typeParameters, tpe) =>
      typeParameters.foreach { s =>
        s.symbols.foreach(emit)
        s.hardlinks.foreach(info => pprint(info.signature))
      }
      pprint(tpe)
    case s.ByNameType(tpe) =>
      pprint(tpe)
    case s.RepeatedType(tpe) =>
      pprint(tpe)
    case s.NoType =>
  }
  private def pprint(const: s.Constant): Unit = {
    const match {
      case s.NoConstant =>
        text.append("<?>")
      case s.UnitConstant() =>
        text.append("()")
      case s.BooleanConstant(true) =>
        text.append(true)
      case s.BooleanConstant(false) =>
        text.append(false)
      case s.ByteConstant(value) =>
        text.append(value.toByte)
      case s.ShortConstant(value) =>
        text.append(value.toShort)
      case s.CharConstant(value) =>
        text.append("'" + value.toChar + "'")
      case s.IntConstant(value) =>
        text.append(value)
      case s.LongConstant(value) =>
        text.append(value + "L")
      case s.FloatConstant(value) =>
        text.append(value + "f")
      case s.DoubleConstant(value) =>
        text.append(value)
      case s.StringConstant(value) =>
        // TODO: Escape
        text.append("\"" + value + "\"")
      case s.NullConstant() =>
        text.append("null")
    }
  }
  private def loop(tree: s.Tree): Unit = tree match {
    case s.NoTree =>
    case s.ApplyTree(fn, args) =>
      loop(fn)
      mkString("(", args, ")")(loop)
    case s.FunctionTree(params, term) =>
      text.append("{")
      mkString("(", params, ") => ")(loop)
      loop(term)
      text.append("}")
    case s.IdTree(sym) =>
      emit(sym)
    case s.LiteralTree(const) =>
      pprint(const)
    case s.MacroExpansionTree(_, tpe) =>
      text.append("(`macro-expandee` : ")
      pprint(tpe)
      text.append(")")
    case s.OriginalTree(_) =>
      emitCode("*", "_star_.")
    case s.SelectTree(qual, id) =>
      loop(qual)
      text.append(".")
      id.foreach(loop) // id should be no_box
    case s.TypeApplyTree(fn, targs) =>
      loop(fn)
      mkString("[", targs, "]")(pprint)
  }

  def toLegacy(synthetic: s.Synthetic): String = {
    loop(synthetic.tree)
    text.toString()
  }
}