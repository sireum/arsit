// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.Util
import org.sireum.hamr.arsit.gcl.GumboGen.{GclComputeEventHolder, GclEntryPointSporadicCompute}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, RecordType, TypeUtil}
import org.sireum.hamr.ir._
import org.sireum.lang.ast.Exp

object GumboGen {

  @sig trait GclEntryPointContainer

  @datatype class GclEntryPointInitialize(val markers: ISZ[Marker],
                                          val contract: ST) extends GclEntryPointContainer

  @sig trait GclHolder

  @datatype class GclBareHolder(val requires: ISZ[ST],
                                val ensures: ISZ[ST]) extends GclHolder

  @datatype class GclComputeEventHolder(val modifies: Option[ST],
                                        val requires: Option[ST],
                                        val ensures: Option[ST]) extends GclHolder

  @datatype class GclCaseHolder(val caseId: String,
                                val requires: ISZ[ST],
                                val ensures: ISZ[ST]) extends GclHolder

  @datatype class GclEntryPointPeriodicCompute(val markers: ISZ[Marker],
                                               val modifies: Option[ST],
                                               val cases: ISZ[GclHolder]) extends GclEntryPointContainer

  @datatype class GclEntryPointSporadicCompute(val markers: ISZ[Marker],
                                               val handlers: HashSMap[AadlPort, GclComputeEventHolder]) extends GclEntryPointContainer


  val StateVarMarker: Marker = Marker("// BEGIN STATE VARS", "// END STATE VARS")
  val InitializesModifiesMarker: Marker = Marker("// BEGIN INITIALIZES MODIFIES", "// END INITIALIZES MODIFIES")
  val InitializesEnsuresMarker: Marker = Marker("// BEGIN INITIALIZES ENSURES", "// END INITIALIZES ENSURES")

  var imports: ISZ[ST] = ISZ()

  def resetImports(): Unit = {
    imports = ISZ()
  }

  def addImports(gen: GumboGen): Unit = {
    imports = imports ++ gen.imports
  }

  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexInfo] = {
    val aadlComponent = symbolTable.componentMap.get(componentPath).get
    val annexInfos: ISZ[GclAnnexInfo] = symbolTable.annexInfos.get(aadlComponent) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexInfo]).map(m => m.asInstanceOf[GclAnnexInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processInitializes(m: AadlThreadOrDevice, symbolTable: SymbolTable, types: AadlTypes, basePackage: String): Option[GclEntryPointInitialize] = {
    resetImports()

    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if (ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.initializes.nonEmpty) {
        val modifies: ISZ[ST] = sc.initializes.get.modifies.map((m: Exp) => st"${m}")

        val inits: ISZ[ST] = sc.initializes.get.guarantees.map((m: GclGuarantee) => {
          imports = imports ++ GumboUtil.resolveLitInterpolateImports(m.exp)
          st"""// guarantee ${m.id}
              |${processDescriptor(m.descriptor, "//   ")}
              |${gclSymbolTable.rexprs.get(m.exp).get}"""
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

        return Some(GclEntryPointInitialize(ISZ(InitializesEnsuresMarker, InitializesModifiesMarker), ret))
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

    for (ai <- ais) {
      val sc = ai.annex.asInstanceOf[GclSubclause]
      val gclSymTable = ai.gclSymbolTable
      val gg = GumboGen(sc, gclSymTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(sc.invariants)
      addImports(gg)
    }

    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 symbolTable: SymbolTable,
                                 aadlTypes: AadlTypes,
                                 basePackageName: String): Map[AadlPort, (Option[ST], ST, ST)] = {
    resetImports()
    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if (ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      val ret: Map[AadlPort, (Option[ST], ST, ST)] = {
        if (gclSymbolTable.apiReferences.nonEmpty || gclSymbolTable.integrationMap.nonEmpty) {
          val gg = GumboGen(sc, gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          val _contracts = gg.processIntegrationContract(m, gclSymbolTable)
          addImports(gg)
          _contracts
        } else {
          Map.empty
        }
      }
      return ret
    } else {
      return Map.empty
    }
  }

  def processStateVars(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): Option[(ST, Marker)] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)

    if (ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.state.nonEmpty) {
        return Some(GumboGen(sc, gclSymbolTable, symbolTable, aadlTypes, basePackageName).processStateVars(sc.state))
      } else {
        return None()
      }
    } else {
      return None()
    }
  }

  def processCompute(m: AadlThreadOrDevice,
                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     basePackageName: String): Option[GclEntryPointContainer] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)

    if (ais.nonEmpty) {
      val sc = ais(0).annex.asInstanceOf[GclSubclause]
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.compute.nonEmpty) {
        return Some(GumboGen(sc, gclSymbolTable, symbolTable, aadlTypes, basePackageName).processCompute(sc.compute.get, m))
      } else {
        return None()
      }
    } else {
      return None()
    }

  }

  def convertToMethodName(s: String): String = {

    def isInt(c: C): B = {
      return c >= c"1" && c <= c"9"
    }

    def isChar(c: C): B = {
      return (c >= c"A" && c <= c"Z") || (c >= c"a" && c <= c"z")
    }

    var cis = ops.ISZOps(conversions.String.toCis(s)).map((c: C) => if (isInt(c) || isChar(c)) c else c"_")
    if (isInt(cis(0))) {
      cis = c"_" +: cis
    }
    return conversions.String.fromCis(cis)
  }


  def processDescriptor(descriptor: Option[String], pad: String): Option[ST] = {
    def getPipeLoc(cis: ISZ[C]): Z = {
      var firstNonSpace: Z = 0
      while (cis(firstNonSpace) == ' ') {
        firstNonSpace = firstNonSpace + 1
      }
      if (firstNonSpace < cis.size - 1 && cis(firstNonSpace) == '|') {
        return firstNonSpace + 1
      } else {
        return 0
      }
    }

    val ret: Option[ST] = descriptor match {
      case Some(s) =>
        val lines = StringUtil.split_PreserveEmptySegments(s, (c: C) => c == '\n')
        var mLines: ISZ[ST] = ISZ(st"${pad}${lines(0)}")
        if (lines.size > 1) {
          for (i <- 1 until lines.size) {
            val lop = conversions.String.toCis(lines(i))
            val pipeLoc = getPipeLoc(lop)
            val x = conversions.String.fromCis(ops.ISZOps(lop).slice(pipeLoc, lop.size))
            mLines = mLines :+ st"${pad}${x}"
          }
        }
        Some(st"${(mLines, "\n")}")
      case _ => None()
    }
    return ret
  }
}

@record class GumboGen(subClause: GclSubclause,
                       gclSymbolTable: GclSymbolTable,
                       symbolTable: SymbolTable,
                       aadlTypes: AadlTypes,
                       basePackageName: String) {

  def fetchHandler(port: AadlPort, handlers: ISZ[GclHandle]): Option[GclHandle] = {
    var ret: Option[GclHandle] = None()
    for (h <- handlers if ret.isEmpty) {
      gclSymbolTable.computeHandlerPortMap.get(h.port) match {
        case Some(p) if p == port => ret = Some(h)
        case _ =>
      }
    }
    return ret
  }

  def processCompute(compute: GclCompute, context: AadlThreadOrDevice): GumboGen.GclEntryPointContainer = {
    var markers: Set[Marker] = Set.empty

    def genComputeModifiesMarker(eventPort: AadlPort, typ: String): Marker = {
      val m = Marker(
        s"// BEGIN_COMPUTE_${typ}_${eventPort.identifier}",
        s"// END_COMPUTE ${typ}_${eventPort.identifier}")

      markers = markers + m
      return m
    }

    if (context.isPeriodic() && compute.handlers.nonEmpty) {
      halt(s"${context.identifier} is periodic but has handlers -- resolver phase should reject this")
    }


    val generalModifies: ISZ[ST] = compute.modifies.map((e: Exp) => st"${e}")

    //var generalRequires: ISZ[ST] = ISZ()
    var generalEnsures: ISZ[ST] = ISZ()

    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {
        val rassume = gclSymbolTable.rexprs.get(generalCase.assumes).get
        imports = imports ++ GumboUtil.resolveLitInterpolateImports(rassume)

        val rguarantee = gclSymbolTable.rexprs.get(generalCase.guarantees).get
        imports = imports ++ GumboUtil.resolveLitInterpolateImports(rguarantee)

        val desc: Option[ST] = if (generalCase.descriptor.nonEmpty) Some(st"${generalCase.descriptor.get}") else None()
        generalEnsures = generalEnsures :+
          st"""// guarantee ${generalCase.id}
              |${GumboGen.processDescriptor(generalCase.descriptor, "//   ")}
              |(${rassume}) ->: (${rguarantee})"""
      }
    }

    if (context.isSporadic()) {
      var handlerMap: HashSMap[AadlPort, GclComputeEventHolder] = HashSMap.empty
      val inEventPorts = context.getPorts().filter((p: AadlPort) => p.direction == Direction.In && p.isInstanceOf[AadlFeatureEvent])
      for (eventPort <- inEventPorts) {

        fetchHandler(eventPort, compute.handlers) match {
          case Some(handler) => {
            val modifies: Option[ST] = if (generalModifies.nonEmpty || handler.modifies.nonEmpty) {
              val modMarker = genComputeModifiesMarker(eventPort, "MODIFIES")
              var handlerModifies = generalModifies ++ handler.modifies.map((m: Exp) => st"${m}")
              Some(
                st"""Modifies(
                    |  ${modMarker.beginMarker}
                    |  ${(handlerModifies, ",\n")}
                    |  ${modMarker.endMarker}
                    |)""")
            } else {
              None()
            }

            /*
            val requires: Option[ST] = if(generalRequires.nonEmpty) {
              val marker = genComputeModifiesMarker(eventPort, "REQUIRES")
              Some(
                st"""Requires(
                    |  ${marker.beginMarker}
                    |  ${(generalRequires, ",\n")}
                    |  ${marker.endMarker}
                    |)""")
            } else {
              None()
            }
             */

            val ensures: Option[ST] = if (generalEnsures.nonEmpty || handler.guarantees.nonEmpty) {
              val handlerEnsures = generalEnsures ++
                handler.guarantees.map((g: GclGuarantee) => {
                  imports = imports ++ GumboUtil.resolveLitInterpolateImports(g.exp)
                  st"""// guarantees ${g.id}
                      |${GumboGen.processDescriptor(g.descriptor, "//   ")}
                      |${gclSymbolTable.rexprs.get(g.exp).get}"""
                })
              val marker = genComputeModifiesMarker(eventPort, "ENSURES")
              Some(
                st"""Ensures(
                    |  ${marker.beginMarker}
                    |  ${(handlerEnsures, ",\n")}
                    |  ${marker.endMarker}
                    |)""")
            } else {
              None()
            }

            handlerMap = handlerMap + eventPort ~> GclComputeEventHolder(modifies, None(), ensures)
          }
          case _ => {
            // use the general ones
            val modifies: Option[ST] = if (generalModifies.nonEmpty) {
              val modMarker = genComputeModifiesMarker(eventPort, "MODIFIES")
              Some(
                st"""Modifies(
                    |  ${modMarker.beginMarker}
                    |  ${(generalModifies, ",\n")}
                    |  ${modMarker.endMarker}
                    |)""")
            } else {
              None()
            }

            /*
            val requires: Option[ST] = if(generalRequires.nonEmpty) {
              val marker = genComputeModifiesMarker(eventPort, "REQUIRES")
              Some(
                st"""Requires(
                    |  ${marker.beginMarker}
                    |  ${(generalRequires, ",\n")}
                    |  ${marker.endMarker}
                    |)""")
            } else {
              None()
            }
              */

            val ensures: Option[ST] = if (generalEnsures.nonEmpty) {
              val marker = genComputeModifiesMarker(eventPort, "ENSURES")
              Some(
                st"""Ensures(
                    |  ${marker.beginMarker}
                    |  ${(generalEnsures, ",\n")}
                    |  ${marker.endMarker}
                    |)""")
            } else {
              None()
            }

            handlerMap = handlerMap + eventPort ~>
              GclComputeEventHolder(modifies, None(), ensures)
          }
        }
      }

      return GclEntryPointSporadicCompute(markers.elements, handlerMap)
    } else {
      halt("handle periodic case")
    }
  }


  var imports: ISZ[ST] = ISZ()

  def getRExp(e: Exp): Exp = {
    return gclSymbolTable.rexprs.get(e).get
  }

  def processStateVars(stateVars: ISZ[GclStateVar]): (ST, Marker) = {
    val svs: ISZ[ST] = stateVars.map((sv: GclStateVar) => {
      val typ = aadlTypes.typeMap.get(sv.classifier).get
      val typeNames = Util.getDataTypeNames(typ, basePackageName)
      st"var ${sv.name}: ${typeNames.qualifiedReferencedTypeName} = ${typeNames.example()}"
    })

    return (
      st"""${GumboGen.StateVarMarker.beginMarker}
          |${(svs, "\n\n")}
          |${GumboGen.StateVarMarker.endMarker}""", GumboGen.StateVarMarker)
  }

  def processInvariants(invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    for (i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.id)

      imports = imports ++ GumboUtil.resolveLitInterpolateImports(i.exp)

      // will be placed in data type def so use resolved exp
      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${getRExp(i.exp)}
            |)"""
    }
    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 gclSymTable: GclSymbolTable): Map[AadlPort, (Option[ST], ST, ST)] = {
    var ret: Map[AadlPort, (Option[ST], ST, ST)] = Map.empty

    for (port <- m.getPorts()) {
      val integration: Option[GclSpec] = gclSymTable.integrationMap.get(port)

      val (aadlType, isEvent, isData): (AadlType, B, B) = port match {
        case i: AadlEventDataPort => (i.aadlType, T, T)
        case i: AadlDataPort => (i.aadlType, F, T)
        case i: AadlEventPort => (TypeUtil.EmptyType, T, F)
        case x => halt("Unexpected port type: $x")
      }

      val isIncoming = port.direction == Direction.In

      val dataTypeNames = Util.getDataTypeNames(aadlType, basePackageName)

      val (specFunction, specEnsures, specRequires, specInvariant): (Option[ST], Option[ST], Option[ST], Option[ST]) =
        integration match {
          case Some(spec) =>
            val portInvariantMethodName = GumboGen.convertToMethodName(spec.id)

            imports = imports ++ GumboUtil.resolveLitInterpolateImports(spec.exp)

            // will be placed in api so don't use resolved expr
            val pureFunc: ST =
              st"""@strictpure def $portInvariantMethodName(${port.identifier}: ${dataTypeNames.qualifiedReferencedTypeName}): B =
                  |  ${spec.exp}"""

            val ensures = st"${portInvariantMethodName}(${port.identifier}),"

            val requires = st"Requires(${portInvariantMethodName}(value)),"

            val body: ST =
              if(!isIncoming && isEvent) st"${port.identifier} == None[${dataTypeNames.qualifiedReferencedTypeName}]() || ${portInvariantMethodName}(${port.identifier}.get) // NOTE: isEmpty symbol undefined in CVC4"
              else st"${portInvariantMethodName}(${port.identifier})"

            val inv =
              st"""@spec def ${port.identifier}_Inv = Invariant(
                  |  ${body}
                  |)"""

            (Some(pureFunc), Some(ensures), Some(requires), Some(inv))
          case _ => (None(), None(), None(), None())
        }

      val portDir: String = if (isIncoming) "incoming" else "outgoing"
      val portType = st"${if (isEvent) "event " else ""}${if (isData) "data " else ""}"

      val specType: String =
        if (!isIncoming && !port.isInstanceOf[AadlDataPort]) s"Option[${dataTypeNames.qualifiedReferencedTypeName}]"
        else dataTypeNames.qualifiedReferencedTypeName
      val specVar: ST =
        st"""// Logika spec var representing port state for ${portDir} ${portType}port
            |@spec var ${port.identifier}: ${specType} = $$
            |${specInvariant}
            |"""

      val contract: ST = {
        val inValue: String =
          if (port.isInstanceOf[AadlEventPort]) s"Some(Empty())"
          else s"Some(${port.identifier})"
        if (isIncoming) {
          st"""Contract(
              |  Ensures(
              |    ${specEnsures}
              |    Res == $inValue
              |  )
              |)"""
        } else {
          val outValue: String =
            if (isEvent) s"Some(${if (isData) "value" else "Empty()"})"
            else "value"
          st"""Contract(
              |  ${specRequires}
              |  Modifies(${port.identifier}),
              |  Ensures(${port.identifier} == $outValue)
              |)
              |Spec {
              |  ${port.identifier} = $outValue
              |}
              |"""
        }
      }

      ret = ret + (port ~> ((specFunction, specVar, contract)))
    }

    return ret
  }
}