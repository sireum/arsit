// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{GclAnnexInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.RecordType
import org.sireum.hamr.ir.{GclAccessExp, GclBinaryExp, GclBinaryOp, GclExp, GclInvariant, GclLiteralExp, GclLiteralType, GclNameExp, GclSubclause, GclUnaryExp, GclUnaryOp}

object GumboGen {
  def convertBinaryOp(op: GclBinaryOp.Type): String = {
    val ret: String = op match {
      case GclBinaryOp.Gte => ">="
      case GclBinaryOp.Gt => ">"
      case GclBinaryOp.Lte => "<="
      case GclBinaryOp.Lt => "<"
      case GclBinaryOp.Eq => "=="

      case GclBinaryOp.Plus => "+"
      case GclBinaryOp.Minus => "-"
      case GclBinaryOp.Div => "/"
      case GclBinaryOp.Mult => "*"

      case GclBinaryOp.And => "&&"
      case GclBinaryOp.AndThen => "&"
      case GclBinaryOp.Or => "||"
      case GclBinaryOp.OrElse => "|"

      case x => halt(s"Not yet $x")
    }
    return ret
  }

  @strictpure def getGclAnnexInfos(componentPath: String, symbolTable: SymbolTable): ISZ[GclAnnexInfo] = {
    val aadlComponent = symbolTable.componentMap.get(componentPath).get
    val annexInfos: ISZ[GclAnnexInfo] = symbolTable.annexInfos.get(aadlComponent) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexInfo]).map(m => m.asInstanceOf[GclAnnexInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processInvariants(e: RecordType, symbolTable: SymbolTable): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()

    val ais = getGclAnnexInfos(e.name, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for(ai <- ais) {
      val sc = ai.annex.asInstanceOf[GclSubclause]
      val gclSymTable = ai.gclSymbolTable

      ret = ret ++ GumboGen(sc, gclSymTable, symbolTable).processInvariants(sc.invariants)

    }

    return ret
  }

  def convertToMethodName(s: String): String = {
    return ops.StringOps(s).replaceAllLiterally(" ", "_")
  }
}

@record class GumboGen(subClause: GclSubclause,
                       gclSymbolTable: GclSymbolTable,
                       symbolTable: SymbolTable) {

  def processBinaryExp(be: GclBinaryExp): ST = {
    val lhs = processExp(be.lhs)
    val rhs = processExp(be.rhs)
    val op = GumboGen.convertBinaryOp(be.op)

    return st"($lhs $op $rhs)"
  }

  def processUnaryExp(ue: GclUnaryExp): ST = {
    val e = processExp(ue.exp)

    val ret : ST = ue.op match {
      case GclUnaryOp.Neg => st"-$e"
      case GclUnaryOp.Not => st"!($e)"
      case x => halt(s"not yet $x")
    }

    return ret
  }

  def processLiteralExp(lit: GclLiteralExp): ST = {
    val ret: ST = lit.typ match {
      case GclLiteralType.Real => st"${lit.exp}f" // F32 for now
      case GclLiteralType.Integer => st"${lit.exp}" // Z
      case GclLiteralType.String => st""""${lit.exp}""""
      case GclLiteralType.Boolean =>
        val e = ops.StringOps(lit.exp).toLower
        e match {
          case "true" => st"T"
          case "false" => st"F"
          case "t" => st"T"
          case "f" => st"F"
          case x => halt(s"$x")
        }
    }
    return ret
  }

  def processAccessExp(value: GclAccessExp): ST = {
    val e = processExp(value.exp)
    return st"${e}.${value.attributeName}"
  }

  def processNameExp(value: GclNameExp): ST = {
    val n = CommonUtil.getName(value.name)
    return st"${n}"
  }

  def processExp(e: GclExp): ST = {
    val ret: ST = e match {
      case be: GclBinaryExp => processBinaryExp(be)
      case ue: GclUnaryExp => processUnaryExp(ue)
      case lit: GclLiteralExp => processLiteralExp(lit)
      case ne: GclNameExp => processNameExp(ne)
      case ae: GclAccessExp => processAccessExp(ae)
    }
    return ret
  }

  def processInvariants(invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    for(i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.name)

      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${processExp(i.exp)}
            |)"""
    }
    return ret
  }
}