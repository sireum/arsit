// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir
import org.sireum.lang.ast.Typed
import org.sireum.lang.{ast => AST}

object GumboXGenUtil {

  def getSlangType(typ: Typed, aadlTypes: AadlTypes): String = {
    @strictpure def toAadl(ids: ISZ[String]): String =
      st"${((if (ops.ISZOps(ids).last == "Type") ops.ISZOps(ids).dropRight(1) else ids), "::")}".render

    typ match {
      case i: AST.Typed.Name =>
        if (i.ids == AST.Typed.optionName) {
          val typeKey = toAadl(i.args(0).asInstanceOf[AST.Typed.Name].ids)
          return s"Option[${aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName}]"
        } else {
          val typeKey = toAadl(i.ids)
          return aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName
        }
      case _ => halt(s"Unexpected ${typ}")
    }
  }

  @strictpure def sortParam(params: ISZ[GGParam]): ISZ[GGParam] =
    (for (partition <-
            (for (kind <- SymbolKind.elements) yield ops.ISZOps(params).filter(p => p.kind == kind))) yield
      ops.ISZOps(partition).sortWith((a, b) => a.name <= b.name)).flatMap(a => a)


  @enum object SymbolKind {
    "StateVarPre"
    "StateVar"
    "ApiVar"
    "Parameter"
  }

  @datatype class GGParam(val name: String,
                          val typ: AST.Typed,
                          val kind: SymbolKind.Type,
                          val origin: AST.Exp)

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: AST.Exp)

  @record class EE() extends ir.MTransformer {
    var params: Set[GGParam] = Set.empty

    override def pre_langastExpSelect(o: AST.Exp.Select): ir.MTransformer.PreResult[AST.Exp] = {
      o match {
        case AST.Exp.Select(Some(AST.Exp.Ident(AST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[AST.Typed.Name]
          val paramName = s"api_${id.value}"
          params = params + GGParam(paramName, typed, SymbolKind.ApiVar, o)
          return ir.MTransformer.PreResult(
            F,
            MSome(AST.Exp.Ident(id = AST.Id(value = paramName, attr = AST.Attr(None())), attr = o.attr)))
        case _ =>
          return ir.MTransformer.PreResult(T, MNone[AST.Exp]())
      }
    }

    override def pre_langastExpInput(o: AST.Exp.Input): ir.MTransformer.PreResult[AST.Exp] = {
      val ret: AST.Exp.Ident = o.exp match {
        case i: AST.Exp.Ident =>
          val name = s"In_${i.id.value}"

          val kind: SymbolKind.Type = i.attr.resOpt match {
            case Some(riv: AST.ResolvedInfo.Var) => SymbolKind.StateVarPre
            case x => halt(s"Infeasible ${x}")
          }

          val typed: AST.Typed = i.attr.typedOpt match {
            case Some(atn: AST.Typed.Name) => atn
            case x => halt(s"Infeasible ${x}")
          }

          params = params + GGParam(name, typed, kind, o)
          AST.Exp.Ident(id = AST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.typedOpt.get match {
        case typed: AST.Typed.Name =>
          params = params + GGParam(o.id.value, typed, SymbolKind.StateVar, o)
        case _ =>
      }
      return ir.MTransformer.PreResult(F, MNone[AST.Exp]())
    }
  }

  def rewriteToExpX(exp: AST.Exp): GGExpParamHolder = {
    val e = EE()
    val ret: GGExpParamHolder = e.transform_langastExp(exp) match {
      case MSome(x) => GGExpParamHolder(e.params, x)
      case _ => GGExpParamHolder(e.params, exp)
    }
    return ret
  }
}