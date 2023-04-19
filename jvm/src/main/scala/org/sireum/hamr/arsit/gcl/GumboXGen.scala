// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil.{GGParam, paramsToComment}
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.lang.ast.Exp
import org.sireum.lang.{ast => AST}

object GumboXGen {

  @record class InvariantRewriter() extends org.sireum.hamr.ir.MTransformer {
    override def pre_langastExpIdent(o: AST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(v: AST.ResolvedInfo.Var) =>
          val value = Exp.Ident(id = AST.Id(value = "value", attr = AST.Attr(posOpt = o.posOpt)), attr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None()))
          val select = Exp.Select(receiverOpt = Some(value), id = o.id, targs = ISZ(), attr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None()))
          return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(select))
        case _ =>
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
      }
    }
  }

  def rewriteInvariant(exp: Exp): Exp = {
    InvariantRewriter().transform_langastExp(exp) match {
      case MSome(e) => return e
      case _ => return exp
    }
  }

  @pure def createInvariantObjectName(aadlType: AadlType): String = {
    return s"${aadlType.nameProvider.typeName}_GumboX"
  }

  @pure def createInvariantMethodName(aadlType: AadlType): String = {
    return s"${aadlType.nameProvider.typeName}_Invariant"
  }

  @pure def convertInvariantToMethodName(id: String, aadlType: AadlType): String = {
    return s"${GumboGen.convertToMethodName(id)}_Invariant"
  }

  var imports: ISZ[String] = ISZ()

  def resetImports(): Unit = {
    imports = ISZ()
  }

  def addImports(gen: GumboXGen): Unit = {
    imports = imports ++ gen.imports
  }

  def getDatatypesWithInvariants(aadlTypes: AadlTypes, symbolTable: SymbolTable): ISZ[AadlType] = {
    @pure def hasInvariant(aadlType: AadlType): B = {
      return getGclAnnexInfos(ISZ(aadlType.name), symbolTable).nonEmpty
    }

    var datatypesWithInvariant: ISZ[AadlType] = ISZ()
    for (aadlType <- aadlTypes.typeMap.values if hasInvariant(aadlType)) {
      datatypesWithInvariant = datatypesWithInvariant :+ aadlType
    }
    return datatypesWithInvariant
  }


  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexClauseInfo] = {
    val aadlComponent = symbolTable.componentMap.get(componentPath).get
    val annexInfos: ISZ[GclAnnexClauseInfo] = symbolTable.annexClauseInfos.get(aadlComponent) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexClauseInfo]).map(m => m.asInstanceOf[GclAnnexClauseInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processCompute2(m: AadlThreadOrDevice,
                      annex: GclSubclause,
                      gclSymbolTable: GclSymbolTable,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      basePackageName: String): BehaviorEntryPointProviderPlugin.PartialMethodContributions = {

    val gg = GumboXGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName)
    val ret = gg.processCompute2(annex.compute.get, m, getDatatypesWithInvariants(aadlTypes, symbolTable))
    addImports(gg)
    return ret
  }

  def processDescriptor(descriptor: Option[String], pad: String): Option[ST] = {
    def getPipeLoc(cis: ISZ[C]): Z = {
      var firstNonSpace: Z = 0
      while (cis(firstNonSpace) == ' ' || cis(firstNonSpace) == '\t') {
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


  def processInvariant(aadlType: AadlType,
                       symbolTable: SymbolTable,
                       aadlTypes: AadlTypes,
                       basePackageName: String): ISZ[ST] = {

    resetImports()

    val ais = getGclAnnexInfos(ISZ(aadlType.name), symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    var ret: ISZ[ST] = ISZ()
    for (ai <- ais) {
      val sc = ai.annex
      val gclSymbolTable = ai.gclSymbolTable
      val gg = GumboXGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(aadlType, sc.invariants)
      addImports(gg)
    }

    return ret
  }
}

@record class GumboXGen(gclSymbolTable: GclSymbolTable,
                        symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        basePackageName: String) {
  var imports: ISZ[String] = ISZ()

  def getRExp(e: AST.Exp): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(e).get)
  }

  def getR2Exp(e: AST.Exp.Ref): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(e.asExp).get)
  }

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

  def processInvariants(aadlType: AadlType, invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()

    var methodNames: ISZ[String] = ISZ()
    for (i <- invariants) {
      val methodName = GumboXGen.convertInvariantToMethodName(i.id, aadlType)

      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(i.exp)

      val descriptor = GumboXGen.processDescriptor(i.descriptor, "*   ")
      methodNames = methodNames :+ methodName
      ret = ret :+
        st"""/** invariant ${i.id}
            |  ${descriptor}
            |  */
            |@strictpure def ${methodName}(value: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
            |  ${GumboXGen.rewriteInvariant(getRExp(i.exp))}"""
    }
    val oracleName = GumboXGen.createInvariantMethodName(aadlType)
    ret = ret :+
      st"""@strictpure def ${oracleName}(value: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
          |  ${(methodNames, "(value) &\n")}(value)"""

    return ret
  }

  def processCompute(compute: GclCompute, context: AadlThreadOrDevice, hasInvariant: ISZ[AadlType]): Option[ST] = {

    var computeSpecRequires: ISZ[ST] = ISZ()
    var oracleComputeSpecCalls: ISZ[ST] = ISZ()

    var requireMethods: ISZ[ST] = ISZ()

    var oracleParams: Set[GGParam] = Set.empty
    var oracleMethods: ISZ[ST] = ISZ()

    for (spec <- compute.specs) {
      val rspec = gclSymbolTable.rexprs.get(spec.exp).get
      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec)

      val descriptor = GumboXGen.processDescriptor(spec.descriptor, "*   ")

      spec match {
        case g: GclAssume =>
        // TODO:

        case g: GclGuarantee =>
          val gg = GumboXGenUtil.rewriteToExpX(rspec, aadlTypes)
          val methodName = st"compute_spec_${g.id}_guarantee"

          oracleParams = oracleParams ++ gg.params.elements

          val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

          oracleComputeSpecCalls = oracleComputeSpecCalls :+
            st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          val method =
            st"""/** guarantees ${g.id}
                |  ${descriptor}
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def $methodName(
                |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                |  ${gg.exp}"""

          computeSpecRequires = computeSpecRequires :+ method
      }
    }


    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {

        val rguarantee = gclSymbolTable.rexprs.get(generalCase.guarantees).get
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee)

        val gg = GumboXGenUtil.rewriteToExpX(rguarantee, aadlTypes)
        val methodName = st"compute_case_${generalCase.id}_guarantee"

        oracleParams = oracleParams ++ gg.params.elements

        val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

        oracleComputeSpecCalls = oracleComputeSpecCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

        val descriptor = GumboXGen.processDescriptor(generalCase.descriptor, "*   ")
        val method =
          st"""/** guarantees ${generalCase.id}
              |  ${descriptor}
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def $methodName(
              |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
              |  ${gg.exp}"""

        computeSpecRequires = computeSpecRequires :+ method
      }
    }

    if (context.isSporadic()) {
      val inEventPorts = context.getPorts().filter((p: AadlPort) => p.direction == Direction.In && p.isInstanceOf[AadlFeatureEvent])

      for (eventPort <- inEventPorts) {

        var allHandlerParams: Set[GGParam] = Set.empty
        var oracleCalls: ISZ[ST] = ISZ()

        fetchHandler(eventPort, compute.handlers) match {
          case Some(handler) => {

            if (handler.guarantees.nonEmpty) {

              val handlerEnsures: ISZ[ST] = //eneralElems ++ _cases ++
                handler.guarantees.map((g: GclGuarantee) => {
                  val rexp = gclSymbolTable.rexprs.get(g.exp).get
                  imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp)

                  val gg = GumboXGenUtil.rewriteToExpX(rexp, aadlTypes)
                  val methodName = st"compute_${handler.port.prettyST}_${g.id}_guarantee"

                  allHandlerParams = allHandlerParams ++ gg.params.elements

                  val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

                  oracleCalls = oracleCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

                  val descriptor = GumboXGen.processDescriptor(g.descriptor, "*   ")
                  st"""/** guarantees ${g.id}
                      |  ${descriptor}
                      |  ${(paramsToComment(sortedParams), "\n")}
                      |  */
                      |@strictpure def $methodName(
                      |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                      |  ${gg.exp}"""
                })

              requireMethods = requireMethods ++ handlerEnsures
            }
          }
          case _ =>
        }

        val sortedParams = GumboXGenUtil.sortParam((oracleParams ++ allHandlerParams.elements).elements)

        var invariantBlocks: ISZ[ST] = ISZ()
        for (sortedParam <- sortedParams if ops.ISZOps(hasInvariant).contains(sortedParam.aadlType)) {
          val qualifiedName = s"${sortedParam.aadlType.nameProvider.qualifiedPackageName}.${GumboXGen.createInvariantObjectName(sortedParam.aadlType)}.${GumboXGen.createInvariantMethodName(sortedParam.aadlType)}"
          invariantBlocks = invariantBlocks :+ st"$qualifiedName(${sortedParam.name})"
        }

        val blocks = invariantBlocks ++ oracleComputeSpecCalls ++ oracleCalls
        if (blocks.nonEmpty) {
          oracleMethods = oracleMethods :+
            st"""/**
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def ${eventPort.identifier}_oracle(
                |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                |  ${(blocks, " &\n")}"""
        }
      } // end for loop
    }
    else {
      // periodic component
      val sortedParams = GumboXGenUtil.sortParam(oracleParams.elements)

      var invariantBlocks: ISZ[ST] = ISZ()
      for (sortedParam <- sortedParams if ops.ISZOps(hasInvariant).contains(sortedParam.aadlType)) {
        val qualifiedName = s"${sortedParam.aadlType.nameProvider.qualifiedPackageName}.${GumboXGen.createInvariantObjectName(sortedParam.aadlType)}.${GumboXGen.createInvariantMethodName(sortedParam.aadlType)}"
        invariantBlocks = invariantBlocks :+ st"$qualifiedName(${sortedParam.name})"
      }

      val blocks = invariantBlocks ++ oracleComputeSpecCalls
      if (blocks.nonEmpty) {
        oracleMethods = oracleMethods :+
          st"""/**
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def time_triggered_oracle(
              |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
              |  ${(blocks, " &\n")}"""
      }
    }

    return (
      if (oracleMethods.nonEmpty)
        Some(
          st"""${(computeSpecRequires ++ requireMethods, "\n\n")}
              |
              |
              |${(oracleMethods, "\n\n")}""")
      else None())
  }

  def processCompute2(compute: GclCompute,
                      context: AadlThreadOrDevice,
                      hasInvariant: ISZ[AadlType]): BehaviorEntryPointProviderPlugin.PartialMethodContributions = {

    var computeSpecRequires: ISZ[ST] = ISZ()
    var oracleComputeSpecCalls: ISZ[ST] = ISZ()

    var requireMethods: ISZ[ST] = ISZ()

    var oracleParams: Set[GGParam] = Set.empty
    var oracleMethods: ISZ[ST] = ISZ()

    for (spec <- compute.specs) {
      val rspec = gclSymbolTable.rexprs.get(spec.exp).get
      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec)

      val descriptor = GumboXGen.processDescriptor(spec.descriptor, "*   ")

      spec match {
        case g: GclAssume =>
        // TODO:

        case g: GclGuarantee =>
          val gg = GumboXGenUtil.rewriteToExpX(rspec, aadlTypes)
          val methodName = st"compute_spec_${g.id}_guarantee"

          oracleParams = oracleParams ++ gg.params.elements

          val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

          oracleComputeSpecCalls = oracleComputeSpecCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          val method =
            st"""/** guarantees ${g.id}
                |  ${descriptor}
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def $methodName(
                |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                |  ${gg.exp}"""

          computeSpecRequires = computeSpecRequires :+ method
      }
    }


    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {

        val rguarantee = gclSymbolTable.rexprs.get(generalCase.guarantees).get
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee)

        val gg = GumboXGenUtil.rewriteToExpX(rguarantee, aadlTypes)
        val methodName = st"compute_case_${generalCase.id}_guarantee"

        oracleParams = oracleParams ++ gg.params.elements

        val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

        oracleComputeSpecCalls = oracleComputeSpecCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

        val descriptor = GumboXGen.processDescriptor(generalCase.descriptor, "*   ")
        val method =
          st"""/** guarantees ${generalCase.id}
              |  ${descriptor}
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def $methodName(
              |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
              |  ${gg.exp}"""

        computeSpecRequires = computeSpecRequires :+ method
      }
    }

    if (context.isSporadic()) {
      val inEventPorts = context.getPorts().filter((p: AadlPort) => p.direction == Direction.In && p.isInstanceOf[AadlFeatureEvent])

      for (eventPort <- inEventPorts) {

        var allHandlerParams: Set[GGParam] = Set.empty
        var oracleCalls: ISZ[ST] = ISZ()

        fetchHandler(eventPort, compute.handlers) match {
          case Some(handler) => {

            if (handler.guarantees.nonEmpty) {

              val handlerEnsures: ISZ[ST] = //eneralElems ++ _cases ++
                handler.guarantees.map((g: GclGuarantee) => {
                  val rexp = gclSymbolTable.rexprs.get(g.exp).get
                  imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp)

                  val gg = GumboXGenUtil.rewriteToExpX(rexp, aadlTypes)
                  val methodName = st"compute_${handler.port.prettyST}_${g.id}_guarantee"

                  allHandlerParams = allHandlerParams ++ gg.params.elements

                  val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

                  oracleCalls = oracleCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

                  val descriptor = GumboXGen.processDescriptor(g.descriptor, "*   ")
                  st"""/** guarantees ${g.id}
                      |  ${descriptor}
                      |  ${(paramsToComment(sortedParams), "\n")}
                      |  */
                      |@strictpure def $methodName(
                      |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                      |  ${gg.exp}"""
                })

              requireMethods = requireMethods ++ handlerEnsures
            }
          }
          case _ =>
        }

        val sortedParams = GumboXGenUtil.sortParam((oracleParams ++ allHandlerParams.elements).elements)

        var invariantBlocks: ISZ[ST] = ISZ()
        for (sortedParam <- sortedParams if ops.ISZOps(hasInvariant).contains(sortedParam.aadlType)) {
          val qualifiedName = s"${sortedParam.aadlType.nameProvider.qualifiedPackageName}.${GumboXGen.createInvariantObjectName(sortedParam.aadlType)}.${GumboXGen.createInvariantMethodName(sortedParam.aadlType)}"
          invariantBlocks = invariantBlocks :+ st"$qualifiedName(${sortedParam.name})"
        }

        val blocks = invariantBlocks ++ oracleComputeSpecCalls ++ oracleCalls
        if (blocks.nonEmpty) {
          oracleMethods = oracleMethods :+
            st"""/**
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def ${eventPort.identifier}_oracle(
                |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                |  ${(blocks, " &\n")}"""
        }
      } // end for loop
    }
    else {
      // periodic component
      val sortedParams = GumboXGenUtil.sortParam(oracleParams.elements)

      var invariantBlocks: ISZ[ST] = ISZ()
      for (sortedParam <- sortedParams if ops.ISZOps(hasInvariant).contains(sortedParam.aadlType)) {
        val qualifiedName = s"${sortedParam.aadlType.nameProvider.qualifiedPackageName}.${GumboXGen.createInvariantObjectName(sortedParam.aadlType)}.${GumboXGen.createInvariantMethodName(sortedParam.aadlType)}"
        invariantBlocks = invariantBlocks :+ st"$qualifiedName(${sortedParam.name})"
      }

      val blocks = invariantBlocks ++ oracleComputeSpecCalls
      if (blocks.nonEmpty) {
        oracleMethods = oracleMethods :+
          st"""/**
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def time_triggered_oracle(
              |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
              |  ${(blocks, " &\n")}"""
      }
    }

    assert(oracleMethods.nonEmpty)

    return (BehaviorEntryPointProviderPlugin.emptyPartialContributions(
      imports = imports,
      preMethodBlocks = ISZ(
        st"""${(computeSpecRequires ++ requireMethods, "\n\n")}
            |
            |
            |${(oracleMethods, "\n\n")}""")))
  }
}