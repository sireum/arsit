// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.Util
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlPort, AadlThreadOrDevice, GclAnnexInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, RecordType}
import org.sireum.hamr.ir.{Direction, GclAccessExp, GclBinaryExp, GclBinaryOp, GclExp, GclIntegration, GclInvariant, GclLiteralExp, GclLiteralType, GclNameExp, GclSubclause, GclUnaryExp, GclUnaryOp}

object GumboGen {

  def convertBinaryOp(op: GclBinaryOp.Type): String = {
    val ret: String = op match {
      case GclBinaryOp.Gte => ">="
      case GclBinaryOp.Gt => ">"
      case GclBinaryOp.Lte => "<="
      case GclBinaryOp.Lt => "<"
      case GclBinaryOp.Eq => "=="
      case GclBinaryOp.Neq => "!="

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

  def processInvariants(e: RecordType, symbolTable: SymbolTable, basePackageName: String): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()

    val ais = getGclAnnexInfos(e.name, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for(ai <- ais) {
      val sc = ai.annex.asInstanceOf[GclSubclause]
      val gclSymTable = ai.gclSymbolTable

      ret = ret ++ GumboGen(sc, gclSymTable, symbolTable, basePackageName).processInvariants(sc.invariants)

    }

    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice, symbolTable: SymbolTable, basePackageName: String): Map[AadlPort, (ST, ST)] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if(ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      val ret: Map[AadlPort, (ST, ST)] = sc.integration match {
        case Some(gclIntegration) => GumboGen(sc, gclSymbolTable, symbolTable, basePackageName).processIntegrationContract(gclIntegration)
        case _ => Map.empty
      }
      return ret
    } else {
      return Map.empty
    }
  }

  def convertToMethodName(s: String): String = {
    import org.sireum.C._

    def isInt(c: C): B = { return c >= c"1" && c <= c"9" }
    def isChar(c: C) : B = { return (c >= c"A" && c <= c"Z") || (c >= c"a" && c <= c"z") }

    var cis = ops.ISZOps(conversions.String.toCis(s)).map((c: C) => if(isInt(c) || isChar(c)) c else c"_")
    if(isInt(cis(0))) {
      cis = c"_" +: cis
    }
    return conversions.String.fromCis(cis)
  }
}

@record class GumboGen(subClause: GclSubclause,
                       gclSymbolTable: GclSymbolTable,
                       symbolTable: SymbolTable,
                       basePackageName: String) {

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

  def processIntegrationContract(gclIntegration: GclIntegration): Map[AadlPort, (ST, ST)] = {
    var ret: Map[AadlPort, (ST, ST)] = Map.empty
    for(spec <- gclIntegration.specs) {
      val port: AadlPort = gclSymbolTable.integrationPort.get(spec).get

      val methodName = GumboGen.convertToMethodName(spec.name)

      val aadlType: AadlType = port match {
        case i: AadlDataPort => i.aadlType
        case i: AadlEventDataPort => i.aadlType
        case _ => halt("Symbol resolver should have prevented this")
      }

      val dataTypeNames = Util.getDataTypeNames(aadlType, basePackageName)

      val function: ST =
        st"""@strictpure def $methodName(${port.identifier}: ${dataTypeNames.qualifiedReferencedTypeName}): B =
            |  ${processExp(spec.exp)}
            |
            |@spec var ${port.identifier}: ${dataTypeNames.qualifiedReferencedTypeName} = $$ // Logika spec var representing port state
            |@spec def ${port.identifier}_Inv = Invariant(
            |  ${methodName}(${port.identifier})
            |)
            |
            |"""

      val contract: ST = {
        if(port.direction == Direction.In) {
          st"""Contract(
              |  Ensures(${methodName}(${port.identifier}),
              |    Res == Some(${port.identifier})
              |  )
              |)"""
        } else {
          st"""Contract(
              |  Requires(${methodName}(value)),
              |  Modifies(${port.identifier}),
              |  Ensures(${port.identifier} == value)
              |)
              |Spec {
              |  ${port.identifier} = value
              |}
              |"""
        }
      }

      ret = ret + (port ~> ((function, contract)))
    }

    return ret
  }
}