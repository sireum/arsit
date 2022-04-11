// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.Util
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlPort, AadlThreadOrDevice, GclAnnexInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, RecordType}
import org.sireum.hamr.ir.{Direction, GclGuarantee, GclIntegration, GclInvariant, GclStateVar, GclSubclause}
import org.sireum.lang.ast.Exp

object GumboGen {

  val StateVarMarker: Marker = Marker("// BEGIN STATE VARS", "// END STATE VARS")
  val InitializesModifiesMarker: Marker = Marker("// BEGIN INITIALIZES MODIFIES", "// END INITIALIZES MODIFIES")
  val InitializesEnsuresMarker: Marker = Marker("// BEGIN INITIALIZES ENSURES", "// END INITIALIZES ENSURES")

  var imports: ISZ[ST] = ISZ()
  def resetImports(): Unit = { imports = ISZ() }
  def addImports(gen: GumboGen): Unit = { imports = imports ++ gen.imports }

  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexInfo] = {
    val aadlComponent = symbolTable.componentMap.get(componentPath).get
    val annexInfos: ISZ[GclAnnexInfo] = symbolTable.annexInfos.get(aadlComponent) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexInfo]).map(m => m.asInstanceOf[GclAnnexInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processInitializes(m: AadlThreadOrDevice, symbolTable: SymbolTable, types: AadlTypes, basePackage: String): Option[(ST, ISZ[Marker])] = {
    resetImports()

    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if(ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      if(sc.initializes.nonEmpty) {
        val modifies: ISZ[ST] = sc.initializes.get.modifies.map((m: Exp) => st"${m}")

        val inits: ISZ[ST] = sc.initializes.get.guarantees.map((m: GclGuarantee) => {
          imports = imports ++ GumboUtil.resolveLitInterpolateImports(m.exp)
          st"""// guarantee "${m.name}"
              |${m.exp}"""
        })

        val ret: ST =
          st"""Contract(
              |  Modifies(
              |    ${InitializesModifiesMarker.beginMarker}
              |    ${(modifies, ",\n")}
              |    ${InitializesModifiesMarker.endMarker}
              |  ),
              |  Ensures(
              |    ${InitializesEnsuresMarker.beginMarker}
              |    ${(inits, ",\n")}
              |    ${InitializesEnsuresMarker.endMarker}
              |  )
              |)"""

        return Some((ret, ISZ(InitializesEnsuresMarker, InitializesModifiesMarker)))
      } else {
        return None()
      }
    } else {
      return None()
    }
  }

  def processInvariants(e: RecordType, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): ISZ[ST] = {
    resetImports()
    var ret: ISZ[ST] = ISZ()

    val FIXME = ISZ(e.name)
    val ais = getGclAnnexInfos(FIXME, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for(ai <- ais) {
      val sc = ai.annex.asInstanceOf[GclSubclause]
      val gclSymTable = ai.gclSymbolTable
      val gg = GumboGen(sc, gclSymTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(sc.invariants)
      addImports(gg)
    }

    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): Map[AadlPort, (ST, ST)] = {
    resetImports()
    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if(ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      val ret: Map[AadlPort, (ST, ST)] = sc.integration match {
        case Some(gclIntegration) =>
          val gg = GumboGen(sc, gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          val _contracts = gg.processIntegrationContract(gclIntegration)
          addImports(gg)
          _contracts
        case _ => Map.empty
      }
      return ret
    } else {
      return Map.empty
    }
  }

  def processStateVars(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): Option[(ST, Marker)] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)

    if(ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      if(sc.state.nonEmpty) {
        return Some(GumboGen(sc, gclSymbolTable, symbolTable, aadlTypes, basePackageName).processStateVars(sc.state))
      } else {
        return None()
      }
    } else {
      return None()
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
                       aadlTypes: AadlTypes,
                       basePackageName: String) {

  var imports: ISZ[ST] = ISZ()

  def processStateVars(stateVars: ISZ[GclStateVar]): (ST, Marker) = {
    val svs: ISZ[ST] = stateVars.map((sv: GclStateVar) => {
      val typ = aadlTypes.typeMap.get(sv.classifier).get
      val typeNames = Util.getDataTypeNames(typ, basePackageName)
      st"var ${sv.name}: ${typeNames.qualifiedReferencedTypeName} = ${typeNames.example()}"
    })

    return (st"""${GumboGen.StateVarMarker.beginMarker}
                |${(svs, "\n\n")}
                |${GumboGen.StateVarMarker.endMarker}""", GumboGen.StateVarMarker)
  }

  def processInvariants(invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    for(i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.name)

      imports = imports ++ GumboUtil.resolveLitInterpolateImports(i.exp)

      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${i.exp}
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

      imports = imports ++ GumboUtil.resolveLitInterpolateImports(spec.exp)

      val function: ST =
        st"""@strictpure def $methodName(${port.identifier}: ${dataTypeNames.qualifiedReferencedTypeName}): B =
            |  ${spec.exp}
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