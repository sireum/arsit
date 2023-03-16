// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil.{GGParam, SymbolKind}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.lang.{ast => AST}

object GumboXGen {

  var imports: ISZ[ST] = ISZ()

  def resetImports(): Unit = {
    imports = ISZ()
  }

  def addImports(gen: GumboXGen): Unit = {
    imports = imports ++ gen.imports
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

  def processInvariants(e: AadlType, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): ISZ[ST] = {
    resetImports()
    var ret: ISZ[ST] = ISZ()

    val FIXME = ISZ(e.name)
    val ais = getGclAnnexInfos(FIXME, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for (ai <- ais) {
      val sc = ai.annex
      val gclSymTable = ai.gclSymbolTable
      val gg = GumboXGen(gclSymTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(sc.invariants)
      addImports(gg)
    }

    return ret
  }

  def processCompute(m: AadlThreadOrDevice,
                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     basePackageName: String): Option[ST] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)

    if (ais.nonEmpty) {
      val sc = ais(0).annex
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.compute.nonEmpty) {
        return GumboXGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processCompute(sc.compute.get, m)
      } else {
        return None()
      }
    } else {
      return None()
    }
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
}

@record class GumboXGen(gclSymbolTable: GclSymbolTable,
                        symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        basePackageName: String) {
  var imports: ISZ[ST] = ISZ()

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

  def paramsToComment(params: ISZ[GGParam]): ISZ[ST] = {
    var comments: ISZ[ST] = ISZ()
    for(p <- params) {
      val kind: String = p.kind match {
        case SymbolKind.StateVarPre => "pre-state state variable"
        case SymbolKind.StateVar => "post-state state variable"
        case SymbolKind.ApiVar => "port variable"
        case SymbolKind.Parameter => "parameter to handler method"
      }
      comments = comments :+ st"* @param ${p.name} ${kind}"
    }
    return comments
  }

  def processInvariants(invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()

    for (i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.id)

      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(i.exp)
      // will be placed in data type def so use resolved exp
      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${getRExp(i.exp)}
            |)"""
    }
    return ret
  }

  def processCompute(compute: GclCompute, context: AadlThreadOrDevice): Option[ST] = {

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
          val gg = GumboXGenUtil.rewriteToExpX(rspec)
          val methodName = st"compute_spec_${g.id}_guarantee"

          oracleParams = oracleParams ++ gg.params.elements

          val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)
          val params: ISZ[(String, String)] = for (param <- sortedParams) yield
            ((param.name, GumboXGenUtil.getSlangType(param.typ, aadlTypes)))

          oracleComputeSpecCalls = oracleComputeSpecCalls :+ st"$methodName(${(for (p <- params) yield p._1, ", ")})"

          val method =
            st"""/** guarantees ${g.id}
                |  ${descriptor}
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def $methodName(
                |    ${(for (p <- params) yield s"${p._1}: ${p._2}", ",\n")}): B =
                |  ${gg.exp}"""

          computeSpecRequires = computeSpecRequires :+ method
      }
    }


    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {

        val rguarantee = gclSymbolTable.rexprs.get(generalCase.guarantees).get
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee)

        val gg = GumboXGenUtil.rewriteToExpX(rguarantee)
        val methodName = st"compute_cae_${generalCase.id}_guarantee"

        oracleParams = oracleParams ++ gg.params.elements

        val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)
        val params: ISZ[(String, String)] = for (param <- sortedParams) yield
          ((param.name, GumboXGenUtil.getSlangType(param.typ, aadlTypes)))

        oracleComputeSpecCalls = oracleComputeSpecCalls :+ st"$methodName(${(for (p <- params) yield p._1, ", ")})"

        val descriptor = GumboXGen.processDescriptor(generalCase.descriptor, "*   ")
        val method =
          st"""/** guarantees ${generalCase.id}
              |  ${descriptor}
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def $methodName(
              |    ${(for (p <- params) yield s"${p._1}: ${p._2}", ",\n")}): B =
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

                  val gg = GumboXGenUtil.rewriteToExpX(rexp)
                  val methodName = st"compute_${handler.port.prettyST}_${g.id}_guarantee"

                  allHandlerParams = allHandlerParams ++ gg.params.elements

                  val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)
                  val params: ISZ[(String, String)] = for (param <- sortedParams) yield
                    ((param.name, GumboXGenUtil.getSlangType(param.typ, aadlTypes)))

                  oracleCalls = oracleCalls :+ st"$methodName(${(for (p <- params) yield p._1, ", ")})"

                  val descriptor = GumboXGen.processDescriptor(g.descriptor, "*   ")
                  st"""/** guarantees ${g.id}
                      |  ${descriptor}
                      |  ${(paramsToComment(sortedParams), "\n")}
                      |  */
                      |@strictpure def $methodName(
                      |    ${(for (p <- params) yield s"${p._1}: ${p._2}", ",\n")}): B =
                      |  ${gg.exp}"""
                })

              requireMethods = requireMethods ++ handlerEnsures
            }
          }
          case _ =>
        }

        val sortedParams = GumboXGenUtil.sortParam((oracleParams ++ allHandlerParams.elements).elements)
        val handlerParams: ISZ[(String, String)] =
          for (param <- sortedParams) yield
            ((param.name, GumboXGenUtil.getSlangType(param.typ, aadlTypes)))

        oracleMethods = oracleMethods :+
          st"""/**
              |  ${(paramsToComment(sortedParams), "\n")}
              |  */
              |@strictpure def ${eventPort.identifier}_oracle(
              |    ${(for (p <- handlerParams) yield st"${p._1}: ${p._2}", ",\n")}): B =
              |  ${(oracleComputeSpecCalls ++ oracleCalls, " &\n")}"""
      }
    } else {
      val sortedParams = GumboXGenUtil.sortParam(oracleParams.elements)
      val handlerParams: ISZ[(String, String)] =
        for (param <- sortedParams) yield
          ((param.name, GumboXGenUtil.getSlangType(param.typ, aadlTypes)))

      oracleMethods = oracleMethods :+
        st"""/**
            |  ${(paramsToComment(sortedParams), "\n")}
            |  */
            |@strictpure def time_triggered_oracle(
            |    ${(for (p <- handlerParams) yield st"${p._1}: ${p._2}", ",\n")}): B =
            |  ${(oracleComputeSpecCalls, " &\n")}"""
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
}