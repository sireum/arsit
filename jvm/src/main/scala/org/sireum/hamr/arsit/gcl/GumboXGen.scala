// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.ProjectDirectories
import org.sireum.hamr.arsit.gcl.GumboXGen._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.{ObjectContributions, emptyObjectContributions}
import org.sireum.hamr.arsit.templates.{StubTemplate, TestTemplate}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir._
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter

object GumboXGen {

  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexClauseInfo] = {
    symbolTable.componentMap.get(componentPath) match {
      case Some(aadlComponent) =>
        symbolTable.annexClauseInfos.get(aadlComponent) match {
          case Some(annexInfos) =>
            return annexInfos.filter(f => f.isInstanceOf[GclAnnexClauseInfo]).map(m => m.asInstanceOf[GclAnnexClauseInfo])
          case _ => return ISZ()
        }
      case _ if componentPath == ISZ(TypeUtil.SlangEmbeddedBitTypeName) =>
        return ISZ()
      case _ =>
        halt(s"Infeasible: couldn't find $componentPath")
    }
  }

  def rewriteInvariant(exp: AST.Exp): AST.Exp = {
    InvariantRewriter().transform_langastExp(exp) match {
      case MSome(e) => return e
      case _ => return exp
    }
  }

  @pure def createInvariantMethodName(aadlType: AadlType): (ISZ[String], ISZ[String]) = {
    return ((
      createInvariantObjectName(aadlType) :+ s"D_Inv_${aadlType.nameProvider.typeName}",
      createInvariantObjectName(aadlType) :+ s"D_Inv_Guard_${aadlType.nameProvider.typeName}"))
  }

  @pure def createInvariantObjectName(aadlType: AadlType): ISZ[String] = {
    val o = ops.ISZOps(aadlType.nameProvider.qualifiedReferencedTypeNameI)
    return o.dropRight(1) :+ s"${o.last}_GumboX"
  }

  @pure def createIntegrationMethodName(isGuarantee: B, aadlPort: AadlPort, componentNames: NameProvider): (ISZ[String], ISZ[String]) = {
    val kind: String = if (aadlPort.direction == Direction.In) "I_Assm" else "I_Guar"
    val name = s"${kind}_${aadlPort.identifier}"
    val guardName = s"${kind}_Guard_${aadlPort.identifier}"
    return (
      (createComponentGumboXObjectName(componentNames) :+ name,
        createComponentGumboXObjectName(componentNames) :+ guardName))
  }

  @pure def convertInvariantToMethodName(id: String, aadlType: AadlType): String = {
    return s"${GumboGen.convertToMethodName(id)}_Invariant"
  }

  @pure def getInitialize_IEP_Guar_Methodname(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"initialize_IEP_Guar"
  }

  @pure def createComponentGumboXObjectName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX"
  }

  @pure def getInitializeGuaranteeMethodName(id: String, componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"initialize_${id}"
  }

  @pure def getInitialize_IEP_Post_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"inititialize_IEP_Post"
  }

  @pure def getInitialize_IEP_Post_Container_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"inititialize_IEP_Post_Container"
  }

  @pure def getCompute_CEP_T_Assm_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_T_Assm"
  }

  @pure def getCompute_CEP_T_Guar_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_T_Guar"
  }

  @pure def getCompute_CEP_T_Case_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_T_Case"
  }

  @pure def getCompute_CEP_Pre_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_Pre"
  }

  @pure def getCompute_CEP_Pre_Container_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_Pre_Container"
  }

  @pure def getCompute_CEP_Post_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_Post"
  }

  @pure def getCompute_CEP_Post_Container_MethodName(componentNames: NameProvider): ISZ[String] = {
    return createComponentGumboXObjectName(componentNames) :+ s"compute_CEP_Post_Container"
  }

  @pure def createTestHarnessGumboXClassName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX_TestHarness"
  }

  @pure def createScalaTestGumboXClassName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX_TestHarness_ScalaTest"
  }

  @pure def createScalaTestGumboXGeneratorClassName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX_TestHarness_ScalaTest_Generator"
  }

  @pure def createDSCTestRunnerClassName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX_DSC_TestRunner"
  }

  def createTestCasesGumboXClassName(componentNames: NameProvider): ISZ[String] = {
    return componentNames.packageNameI :+ s"${componentNames.componentSingletonType}_GumboX_Tests"
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

  def getPortInfo(port: AadlFeature): (AadlType, String) = {
    val ret: (AadlType, String) = port match {
      case i: AadlEventDataPort => (i.aadlType, "event data")
      case i: AadlDataPort => (i.aadlType, "data")
      case i: AadlEventPort => (TypeUtil.EmptyType, "event")
      case _ => halt("Infeasible")
    }
    return ret
  }

  @sig trait IntegrationHolder {
    def method: ContractHolder

    def guard: ContractHolder
  }

  @datatype class ContractHolder(val methodName: ISZ[String],
                                 val params: ISZ[GGParam],
                                 val imports: ISZ[String],
                                 val content: ST)

  @datatype class IntegrationAssumeHolder(val method: ContractHolder,
                                          val guard: ContractHolder) extends IntegrationHolder

  @datatype class IntegrationGuaranteeHolder(val method: ContractHolder,
                                             val guard: ContractHolder) extends IntegrationHolder

  @datatype class DataInvariantHolder(val D_Inv_Method_Name: ISZ[String],
                                      val D_Inv_Guard_Method_Name: ISZ[String],
                                      //val D_Inv_Mem_FQ_MethodName: ISZ[String], // TODO
                                      val resource: FileResource)

  @datatype class InitializeEntryPointHolder(val IEP_Guar: ContractHolder)

  @datatype class ComputeEntryPointHolder(val CEP_Pre: Option[ContractHolder],
                                          val CEP_Post: Option[ContractHolder])

  @record class InvariantRewriter() extends org.sireum.hamr.ir.MTransformer {
    override def pre_langastExpIdent(o: AST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(v: AST.ResolvedInfo.Var) =>
          val value = AST.Exp.Ident(id = AST.Id(value = "value", attr = AST.Attr(posOpt = o.posOpt)), attr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None()))
          val select = AST.Exp.Select(receiverOpt = Some(value), id = o.id, targs = ISZ(), attr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None()))
          return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(select))
        case _ =>
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
      }
    }
  }
}


@record class GumboXGen {

  var imports: ISZ[String] = ISZ()


  // {datatype path -> holder}
  var dataInvariants: Map[String, DataInvariantHolder] = Map.empty

  // {component path -> {port path -> holder}}
  var integrationClauses: Map[IdPath, ISZ[(IdPath, IntegrationHolder)]] = Map.empty

  // {component path -> holder}
  var initializeEntryPointHolder: Map[IdPath, InitializeEntryPointHolder] = Map.empty

  // {component path -> holder}
  var computeEntryPointHolder: Map[IdPath, ComputeEntryPointHolder] = Map.empty

  def processAnnex(component: AadlThreadOrDevice, componentNames: NameProvider,
                   gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)],
                   basePackageName: String, symbolTable: SymbolTable, aadlTypes: AadlTypes,
                   projectDirectories: ProjectDirectories, reporter: Reporter): Unit = {

    processIntegerationConstraints(component, componentNames, gclSubclauseInfo, basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)

    processInitializeEntrypoint(component, componentNames, gclSubclauseInfo, basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)

    processComputeEntrypoints(component, componentNames, gclSubclauseInfo, basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)
  }

  def processIntegerationConstraints(component: AadlThreadOrDevice, componentNames: NameProvider,
                                     gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)],
                                     basePackageName: String, symbolTable: SymbolTable, aadlTypes: AadlTypes, directories: ProjectDirectories, reporter: Reporter): Unit = {
    resetImports()

    gclSubclauseInfo match {
      case Some((annex, gclSymbolTable)) =>
        val ports = component.features.filter(p => p.isInstanceOf[AadlPort]).map((m: AadlFeature) => m.asInstanceOf[AadlPort])
        for (port <- ports if gclSymbolTable.integrationMap.contains(port)) {

          val (aadlType, portType) = GumboXGen.getPortInfo(port)

          val clause = gclSymbolTable.integrationMap.get(port).get
          val rexp = getRExp(clause.exp, basePackageName, aadlTypes, gclSymbolTable)

          clause match {
            case i: GclAssume =>
              val (i_assm_name, i_assm_guard_name) = GumboXGen.createIntegrationMethodName(F, port, componentNames)
              val simple = ops.ISZOps(i_assm_name).last
              val I_Assm =
                st"""/** I-Assm: Integration constraint on ${component.identifier}'s incoming $portType port ${port.identifier}
                    |  *
                    |  * assume ${i.id}
                    |  ${GumboXGen.processDescriptor(i.descriptor, "*   ")}
                    |  */
                    |@strictpure def $simple(${port.identifier}: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
                    |  $rexp"""

              val typ: (String, ST) =
                if (!GumboXGenUtil.isDataPort(symbolTable.featureMap.get(port.path).get))
                  (s"Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]", st"${port.identifier}.nonEmpty -->: $simple(${port.identifier}.get)")
                else (aadlType.nameProvider.qualifiedReferencedTypeName, st"$simple(${port.identifier})")

              val I_Assm_Guard =
                st"""// I-Assm-Guard: Integration constraint on ${component.identifier}'s incoming $portType port ${port.identifier}
                    |@strictpure def ${ops.ISZOps(i_assm_guard_name).last}(${port.identifier}: ${typ._1}): B =
                    |  ${typ._2}"""

              integrationClauses = integrationClauses + component.path ~> (
                integrationClauses.getOrElse(component.path, ISZ()) :+
                  (port.path, IntegrationAssumeHolder(
                    ContractHolder(i_assm_name, ISZ(), imports, I_Assm),
                    ContractHolder(i_assm_guard_name, ISZ(), imports, I_Assm_Guard))))
            case i: GclGuarantee =>
              val (i_guar_name, i_guar_guard_name) = GumboXGen.createIntegrationMethodName(T, port, componentNames)
              val simple = ops.ISZOps(i_guar_name).last

              val I_Guar =
                st"""/** I-Guar: Integration constraint on ${component.identifier}'s outgoing $portType port ${port.identifier}
                    |  *
                    |  * guarantee ${i.id}
                    |  ${GumboXGen.processDescriptor(i.descriptor, "*   ")}
                    |  */
                    |@strictpure def $simple(${port.identifier}: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
                    |  $rexp"""

              val I_Guar_Guard =
                st"""// I_Guar-Guard: Integration constraint on ${component.identifier}'s outgoing $portType port ${port.identifier}
                    |@strictpure def ${ops.ISZOps(i_guar_guard_name).last}(${port.identifier}: Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]): B =
                    |  ${port.identifier}.nonEmpty -->: $simple(${port.identifier}.get)"""

              integrationClauses = integrationClauses + component.path ~> (
                integrationClauses.getOrElse(component.path, ISZ()) :+
                  (port.path, IntegrationGuaranteeHolder(
                    ContractHolder(i_guar_name, ISZ(), imports, I_Guar),
                    ContractHolder(i_guar_guard_name, ISZ(), imports, I_Guar_Guard))))
            case _ => halt("Infeasible")
          }
        }
      case _ =>
    }
  }

  def processInitializeEntrypoint(component: AadlThreadOrDevice, componentNames: NameProvider,
                                  gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)],
                                  basePackageName: String, symbolTable: SymbolTable, aadlTypes: AadlTypes, directories: ProjectDirectories, reporter: Reporter): Unit = {
    resetImports()

    var IEP_Guard: ISZ[ST] = ISZ()
    var IEP_Guard_Blocks: ISZ[ST] = ISZ()
    var IEP_Guar_Params: Set[GGParam] = Set.empty[GGParam] ++
      GumboXGenUtil.outPortsToParams(component, componentNames) ++
      GumboXGenUtil.stateVarsToParams(componentNames, gclSubclauseInfo, F, aadlTypes)

    val stateVars: ISZ[GclStateVar] = {
      gclSubclauseInfo match {
        case Some((a, b)) => a.state
        case _ => ISZ()
      }
    }

    gclSubclauseInfo match {
      case Some((GclSubclause(_, _, _, Some(initializes), _, _), gclSymbolTable)) =>

        var requiresMethods: ISZ[(ISZ[String], ST)] = ISZ()
        var combinedSpecCalls: ISZ[ST] = ISZ()

        // process each guarantee clause
        for (spec <- initializes.guarantees) {
          val rspec = getRExp(spec.exp, basePackageName, aadlTypes, gclSymbolTable)
          imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec)

          val descriptor = GumboXGen.processDescriptor(spec.descriptor, "*   ")

          val methodName = GumboXGen.getInitializeGuaranteeMethodName(spec.id, componentNames)
          val simpleMethodName = ops.ISZOps(methodName).last

          val gg = GumboXGenUtil.rewriteToExpX(rspec, component, componentNames, aadlTypes, stateVars)
          IEP_Guar_Params = IEP_Guar_Params ++ gg.params.elements
          val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

          combinedSpecCalls = combinedSpecCalls :+ st"$simpleMethodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          val method =
            st"""/** Initialize Entrypoint Contract
                |  *
                |  * guarantee ${spec.id}
                |  ${descriptor}
                |  ${(paramsToComment(sortedParams), "\n")}
                |  */
                |@strictpure def $simpleMethodName (
                |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                |  ${gg.exp}"""

          requiresMethods = requiresMethods :+ (methodName, method)
        }

        val sorted_IEP_Guar_Params = GumboXGenUtil.sortParam(IEP_Guar_Params.elements)

        // create IEP-Guar that calls the individual guarantee clauses
        val (iep_guar_methodName, iep_guar_content): (ISZ[String], ST) = {
          val combinedInitializeMethodName = GumboXGen.getInitialize_IEP_Guar_Methodname(componentNames)
          val simpleName = ops.ISZOps(combinedInitializeMethodName).last
          val content =
            st"""${(for (r <- requiresMethods) yield r._2, "\n\n")}
                |
                |/** IEP-Guar: Initialize Entrypoint Contracts for ${component.identifier}
                |  *
                |  ${(paramsToComment(sorted_IEP_Guar_Params), "\n")}
                |  */
                |@strictpure def ${simpleName} (
                |    ${(for (p <- sorted_IEP_Guar_Params) yield p.getParamDef, ",\n")}): B =
                |  ${(combinedSpecCalls, " &\n")}"""
          (combinedInitializeMethodName, content)
        }

        // call the invariant
        IEP_Guard = IEP_Guard :+ st"${ops.ISZOps(iep_guar_methodName).last}(${(for (p <- sorted_IEP_Guar_Params) yield p.name, ", ")})"
        IEP_Guard_Blocks = IEP_Guard_Blocks :+ iep_guar_content
      case _ =>
    }

    var I_Guar_Guar_Params: Set[GGParam] = Set.empty
    var I_Guar_Guard: ISZ[ST] = ISZ()
    integrationClauses.get(component.path) match {
      case Some(clauses) =>
        for (pair <- clauses if GumboXGenUtil.isOutPort(symbolTable.featureMap.get(pair._1).get)) {
          val port = symbolTable.featureMap.get(pair._1).get.asInstanceOf[AadlPort]
          val isEvent = !GumboXGenUtil.isDataPort(port)

          val methodToUse: ISZ[String] =
            if (isEvent) pair._2.guard.methodName
            else pair._2.method.methodName

          val (aadlType, _) = GumboXGen.getPortInfo(symbolTable.featureMap.get(pair._1).get)
          val param = GGPortParam(
            port = port,
            componentNames = componentNames,
            aadlType = aadlType)
          I_Guar_Guar_Params = I_Guar_Guar_Params + param
          I_Guar_Guard = I_Guar_Guard :+ st"${ops.ISZOps(methodToUse).last}(${param.name})"
        }
      case _ =>
    }

    val IEP_Post_Params: Set[GGParam] = IEP_Guar_Params ++ I_Guar_Guar_Params.elements
    val sorted_IEP_Post_Params = sortParam(IEP_Post_Params.elements)

    var D_Inv_Guards: ISZ[ST] = ISZ()
    for (sortedParam <- sorted_IEP_Post_Params if dataInvariants.contains(sortedParam.aadlType.name)) {
      val d_inv_guard_MethodName = createInvariantMethodName(sortedParam.aadlType)
      val methodToUse: ISZ[String] =
        if (!sortedParam.isOptional) d_inv_guard_MethodName._1
        else d_inv_guard_MethodName._2
      D_Inv_Guards = D_Inv_Guards :+ st"${(methodToUse, ".")}(${sortedParam.name})"
    }


    // create IEP-Post that:
    //   - calls I-Guar-Guard for each of the output ports of the component
    //   - calls D-Inv-Guard for each datatype associated with the output ports
    //   - calls IEP-Guard
    val iepPostMethodName = GumboXGen.getInitialize_IEP_Post_MethodName(componentNames)
    val simple_IEP_Post = ops.ISZOps(iepPostMethodName).last

    var segments: ISZ[ST] = ISZ()
    if (D_Inv_Guards.nonEmpty) {
      segments = segments :+
        st"""// D-Inv-Guard: Datatype invariants for the types associated with ${component.identifier}'s state variables and outgoing ports
            |${(D_Inv_Guards, " &\n")}"""
    }
    if (I_Guar_Guard.nonEmpty) {
      segments = segments :+
        st"""// I-Guar-Guard: Integration constraints for ${component.identifier}'s outgoing ports"
            |${(I_Guar_Guard, " &\n")}"""
    }
    if (IEP_Guard.nonEmpty) {
      segments = segments :+
        st"""// IEP-Guar: Initialize Entrypoint contract for ${component.identifier}
            |${(IEP_Guard, " &\n")}"""
    }

    if (segments.nonEmpty) {
      val iep_post =
        st"""/** IEP-Post: Initialize Entrypoint Post-Condition
            |  *
            |  ${(paramsToComment(sorted_IEP_Post_Params), "\n")}
            |  */
            |@strictpure def ${simple_IEP_Post} (
            |    ${(for (p <- sorted_IEP_Post_Params) yield p.getParamDef, ",\n")}): B =
            |  (${(segments, " & \n\n")})"""

      val simple_IEP_Post_container = ops.ISZOps(GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)).last
      val postContainerName = GumboXGenUtil.genContainerName(componentNames.componentSingletonType, F, T)

      val iep_post_container =
        st"""/** IEP-Post: Initialize Entrypoint Post-Condition via container
            |  *
            |  * @param post Container holding the value of incoming ports and the pre-state values of state variables
            |  */
            |@strictpure def ${simple_IEP_Post_container} (post: ${postContainerName}): B =
            |  $simple_IEP_Post (
            |    ${(for (p <- sorted_IEP_Post_Params) yield st"${p.name} = post.${p.name}", ",\n")})"""
      val ret = st"${(IEP_Guard_Blocks :+ iep_post :+ iep_post_container, "\n\n")}"

      initializeEntryPointHolder = initializeEntryPointHolder + component.path ~>
        InitializeEntryPointHolder(ContractHolder(iepPostMethodName, sorted_IEP_Post_Params, imports, ret))
    }
  }

  def processComputeEntrypoints(component: AadlThreadOrDevice, componentNames: NameProvider,
                                gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)],
                                basePackageName: String, symbolTable: SymbolTable, aadlTypes: AadlTypes, directories: ProjectDirectories, reporter: Reporter): Unit = {
    resetImports()

    var CEP_T_Case: Option[ContractHolder] = None()

    var CEP_T_Assm: Option[ContractHolder] = None()
    var CEP_T_Guar: Option[ContractHolder] = None()

    var CEP_Pre_Params: Set[GGParam] = Set.empty[GGParam] ++
      GumboXGenUtil.inPortsToParams(component, componentNames) ++
      GumboXGenUtil.stateVarsToParams(componentNames, gclSubclauseInfo, T, aadlTypes)

    var CEP_Post_Params: Set[GGParam] = Set.empty[GGParam] ++
      GumboXGenUtil.outPortsToParams(component, componentNames) ++
      GumboXGenUtil.stateVarsToParams(componentNames, gclSubclauseInfo, T, aadlTypes) ++
      GumboXGenUtil.stateVarsToParams(componentNames, gclSubclauseInfo, F, aadlTypes)

    val stateVars: ISZ[GclStateVar] = {
      gclSubclauseInfo match {
        case Some((a, b)) => a.state
        case _ => ISZ()
      }
    }

    gclSubclauseInfo match {
      case Some((GclSubclause(_, _, _, _, _, Some(gclCompute)), gclSymbolTable)) => {
        { // process top level assume/guarantees

          var CEP_T_Assm_Params: Set[GGParam] = Set.empty
          var topLevelAssumes: ISZ[ST] = ISZ()
          var topLevelAssumeCallsCombined: ISZ[ST] = ISZ()

          var CEP_T_Guar_Params: Set[GGParam] = Set.empty
          var topLevelGuarantees: ISZ[ST] = ISZ()
          var topLevelGuaranteesCombined: ISZ[ST] = ISZ()
          for (spec <- gclCompute.specs) {
            val rspec = gclSymbolTable.rexprs.get(spec.exp).get
            imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec)

            val descriptor = GumboXGen.processDescriptor(spec.descriptor, "*   ")

            spec match {
              case g: GclAssume =>
                val gg = GumboXGenUtil.rewriteToExpX(rspec, component, componentNames, aadlTypes, stateVars)
                val methodName = st"compute_spec_${g.id}_assume"

                CEP_T_Assm_Params = CEP_T_Assm_Params ++ gg.params.elements

                val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

                topLevelAssumeCallsCombined = topLevelAssumeCallsCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

                val method =
                  st"""/** Compute Entrypoint Contract
                      |  *
                      |  * assumes ${g.id}
                      |  ${descriptor}
                      |  ${(paramsToComment(sortedParams), "\n")}
                      |  */
                      |@strictpure def $methodName(
                      |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                      |  ${gg.exp}"""

                topLevelAssumes = topLevelAssumes :+ method

              case g: GclGuarantee =>
                val gg = GumboXGenUtil.rewriteToExpX(rspec, component, componentNames, aadlTypes, stateVars)
                val methodName = st"compute_spec_${g.id}_guarantee"

                CEP_T_Guar_Params = CEP_T_Guar_Params ++ gg.params.elements

                val sortedParams = GumboXGenUtil.sortParam(gg.params.elements)

                topLevelGuaranteesCombined = topLevelGuaranteesCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

                val method =
                  st"""/** Compute Entrypoint Contract
                      |  *
                      |  * guarantee ${g.id}
                      |  ${descriptor}
                      |  ${(paramsToComment(sortedParams), "\n")}
                      |  */
                      |@strictpure def $methodName(
                      |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                      |  ${gg.exp}"""

                topLevelGuarantees = topLevelGuarantees :+ method
            }
          }

          if (topLevelAssumes.nonEmpty) {
            val sorted_CEP_T_Assm_Params = sortParam(CEP_T_Assm_Params.elements)
            val CEP_T_Assm_MethodName = GumboXGen.getCompute_CEP_T_Assm_MethodName(componentNames)
            val simpleName = ops.ISZOps(CEP_T_Assm_MethodName).last
            val content =
              st"""${(topLevelAssumes, "\n\n")}
                  |
                  |/** CEP-T-Assm: Top-level assume contracts for ${component.identifier}'s compute entrypoint
                  |  *
                  |  ${(paramsToComment(sorted_CEP_T_Assm_Params), "\n")}
                  |  */
                  |@strictpure def ${simpleName} (
                  |    ${(for (p <- sorted_CEP_T_Assm_Params) yield p.getParamDef, ",\n")}): B =
                  |  ${(topLevelAssumeCallsCombined, " &\n")}"""
            CEP_T_Assm = Some(
              ContractHolder(CEP_T_Assm_MethodName, sorted_CEP_T_Assm_Params, imports, content)
            )
          }

          if (topLevelGuarantees.nonEmpty) {
            val sorted_CEP_T_Guar_Params = sortParam(CEP_T_Guar_Params.elements)
            val CEP_T_Guar_MethodName = GumboXGen.getCompute_CEP_T_Guar_MethodName(componentNames)
            val simpleName = ops.ISZOps(CEP_T_Guar_MethodName).last
            val content =
              st"""${(topLevelGuarantees, "\n\n")}
                  |
                  |/** CEP-T-Guar: Top-level guarantee contracts for ${component.identifier}'s compute entrypoint
                  |  *
                  |  ${(paramsToComment(sorted_CEP_T_Guar_Params), "\n")}
                  |  */
                  |@strictpure def ${simpleName} (
                  |    ${(for (p <- sorted_CEP_T_Guar_Params) yield p.getParamDef, ",\n")}): B =
                  |  ${(topLevelGuaranteesCombined, " &\n")}"""
            CEP_T_Guar = Some(
              ContractHolder(CEP_T_Guar_MethodName, sorted_CEP_T_Guar_Params, imports, content)
            )
          }
        }

        if (gclCompute.cases.nonEmpty) { // process contract cases
          var CEP_T_Case_Params: Set[GGParam] = Set.empty
          var caseMethods: ISZ[ST] = ISZ()
          var caseCallsCombined: ISZ[ST] = ISZ()

          for (generalCase <- gclCompute.cases) {
            val rexp = gclSymbolTable.rexprs.get(generalCase.assumes).get
            val rrassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rexp)
            imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rrassume)

            val ggAssm = GumboXGenUtil.rewriteToExpX(rrassume, component, componentNames, aadlTypes, stateVars)

            val rguarantee = gclSymbolTable.rexprs.get(generalCase.guarantees).get
            imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee)

            val ggGuar = GumboXGenUtil.rewriteToExpX(rguarantee, component, componentNames, aadlTypes, stateVars)
            val methodName = st"compute_case_${generalCase.id}"

            val combinedAssmGuarParam = ggAssm.params ++ ggGuar.params.elements
            CEP_T_Case_Params = CEP_T_Case_Params ++ combinedAssmGuarParam.elements

            val sortedParams = GumboXGenUtil.sortParam(combinedAssmGuarParam.elements)

            caseCallsCombined = caseCallsCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

            val descriptor = GumboXGen.processDescriptor(generalCase.descriptor, "*   ")
            val method =
              st"""/** guarantee ${generalCase.id}
                  |  ${descriptor}
                  |  ${(paramsToComment(sortedParams), "\n")}
                  |  */
                  |@strictpure def $methodName(
                  |    ${(for (p <- sortedParams) yield p.getParamDef, ",\n")}): B =
                  |  (${ggAssm.exp}) -->:
                  |    (${ggGuar.exp})"""

            caseMethods = caseMethods :+ method
          }

          val sorted_CEP_T_Case_Params = sortParam(CEP_T_Case_Params.elements)
          val CEP_T_Case_MethodName = GumboXGen.getCompute_CEP_T_Case_MethodName(componentNames)
          val simpleName = ops.ISZOps(CEP_T_Case_MethodName).last
          val content =
            st"""${(caseMethods, "\n\n")}
                |
                |/** CEP-T-Case: Top-Level case contracts for ${component.identifier}'s compute entrypoint
                |  *
                |  ${(paramsToComment(sorted_CEP_T_Case_Params), "\n")}
                |  */
                |@strictpure def ${simpleName} (
                |    ${(for (p <- sorted_CEP_T_Case_Params) yield p.getParamDef, ",\n")}): B =
                |  ${(caseCallsCombined, " &\n")}"""
          CEP_T_Case = Some(
            ContractHolder(CEP_T_Case_MethodName, sorted_CEP_T_Case_Params, imports, content)
          )
        }

        // NOTE: gcl symbol resolver disallows handlers for periodic threads

        var CEP_H_Assm: Option[ContractHolder] = None()
        var CEP_H_Guar: Option[ContractHolder] = None()
        if (gclCompute.handlers.nonEmpty) {
          // TODO TBD
        }

      }
      case _ =>
    }

    var CEP_Pre: Option[ContractHolder] = None()

    { // CEP-Pre

      var I_Assm_Guard_Params: Set[GGParam] = Set.empty
      var I_Assm_Guard: ISZ[ST] = ISZ()
      integrationClauses.get(component.path) match {
        case Some(clauses) =>
          for (pair <- clauses if GumboXGenUtil.isInPort(symbolTable.featureMap.get(pair._1).get)) {
            val (aadlType, _) = GumboXGen.getPortInfo(symbolTable.featureMap.get(pair._1).get)
            val port = symbolTable.featureMap.get(pair._1).get.asInstanceOf[AadlPort]
            val param = GGPortParam(
              port = port,
              componentNames = componentNames,
              aadlType = aadlType)
            I_Assm_Guard_Params = I_Assm_Guard_Params + param
            I_Assm_Guard = I_Assm_Guard :+ st"${ops.ISZOps(pair._2.guard.methodName).last}(${param.name})"
          }
        case _ =>
      }

      var D_Inv_Guards: ISZ[ST] = ISZ()
      for (sortedParam <- sortParam(CEP_Pre_Params.elements) if dataInvariants.contains(sortedParam.aadlType.name)) {
        val d_inv_guard_MethodName = createInvariantMethodName(sortedParam.aadlType)
        val nameToUse: ISZ[String] =
          if (sortedParam.isOptional) d_inv_guard_MethodName._2
          else d_inv_guard_MethodName._1
        D_Inv_Guards = D_Inv_Guards :+ st"${(nameToUse, ".")}(${sortedParam.name})"
      }

      if (I_Assm_Guard.nonEmpty || CEP_T_Assm.nonEmpty || D_Inv_Guards.nonEmpty) {
        // CREATE CEP-Pre

        //var CEP_Pre_Params: Set[GGParam] = I_Assm_Guard_Params

        val CEP_Assm_call: Option[ST] = CEP_T_Assm match {
          case Some(i) =>
            CEP_Pre_Params = CEP_Pre_Params ++ i.params
            val sortedParams = sortParam(i.params)
            Some(st"${ops.ISZOps(i.methodName).last} (${(for (p <- sortedParams) yield p.name, ", ")})")
          case _ => None()
        }

        // create CEP-Pre that:
        //   - calls I-Assm-Guard for each input port
        //   - calls D-Inv-Guar for each type used by an input port
        //   - call CEP-Assm
        val cepPreMethodName = GumboXGen.getCompute_CEP_Pre_MethodName(componentNames)
        val simpleCepPre = ops.ISZOps(cepPreMethodName).last

        @strictpure def pre_opt(sts: ISZ[ST], desc: String): ST =
          st"""// $desc
              |${(sts, " & \n")}"""

        val sorted_Cep_Pre_Params = sortParam(CEP_Pre_Params.elements)

        var segments: ISZ[ST] = ISZ()
        if (D_Inv_Guards.nonEmpty) {
          segments = segments :+ pre_opt(D_Inv_Guards, s"D-Inv-Guard: Datatype invariants for the types associated with ${component.identifier}'s state variables and incoming ports")
        }
        if (I_Assm_Guard.nonEmpty) {
          segments = segments :+ pre_opt(I_Assm_Guard, s"I-Assm-Guard: Integration constraints for ${component.identifier}'s incoming ports")
        }
        if (CEP_Assm_call.nonEmpty) {
          segments = segments :+ pre_opt(ISZ(CEP_Assm_call.get), s"CEP-Assm: assume clauses of ${component.identifier}'s compute entrypoint")
        }

        val cep_pre =
          st"""/** CEP-Pre: Compute Entrypoint Pre-Condition for ${component.identifier}
              |  *
              |  ${(paramsToComment(sorted_Cep_Pre_Params), "\n")}
              |  */
              |@strictpure def ${simpleCepPre} (
              |    ${(for (p <- sorted_Cep_Pre_Params) yield p.getParamDef, ",\n")}): B =
              |  (${(segments, " & \n\n")})"""

        val simpleCepPreContainer = ops.ISZOps(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames)).last
        val preContainerName_wL = GumboXGenUtil.genContainerName(componentNames.componentSingletonType, T, T)

        val cep_pre_container =
          st"""/** CEP-Pre: Compute Entrypoint Pre-Condition for ${component.identifier} via container
              |  *
              |  * @param pre Container holding the value of incoming ports and the pre-state values of state variables
              |  */
              |@strictpure def ${simpleCepPreContainer}(pre: ${preContainerName_wL}): B =
              |  ${simpleCepPre}(
              |    ${(for (e <- sorted_Cep_Pre_Params) yield st"${e.name} = pre.${e.name}", ",\n")})"""

        var bbbs: ISZ[ST] = ISZ()
        if (CEP_T_Assm.nonEmpty) {
          bbbs = bbbs :+ CEP_T_Assm.get.content
        }
        bbbs = bbbs :+ cep_pre :+ cep_pre_container

        val ret = st"${(bbbs, "\n\n")}"

        CEP_Pre = Some(ContractHolder(cepPreMethodName, sorted_Cep_Pre_Params, imports, ret))
      }
    }

    var CEP_Post: Option[ContractHolder] = None()

    {
      var I_Guar_Guard_Params: Set[GGParam] = Set.empty
      var I_Guar_Guard: ISZ[ST] = ISZ()
      integrationClauses.get(component.path) match {
        case Some(clauses) =>
          for (pair <- clauses if GumboXGenUtil.isOutPort(symbolTable.featureMap.get(pair._1).get)) {
            val isOptional = !GumboXGenUtil.isDataPort(symbolTable.featureMap.get(pair._1).get)
            val methodToUse: ISZ[String] = if (isOptional) pair._2.guard.methodName else pair._2.method.methodName

            val port = symbolTable.featureMap.get(pair._1).get.asInstanceOf[AadlPort]
            val (aadlType, _) = GumboXGen.getPortInfo(symbolTable.featureMap.get(pair._1).get)
            val param = GGPortParam(
              port = port,
              componentNames = componentNames,
              aadlType = aadlType)
            I_Guar_Guard_Params = I_Guar_Guard_Params + param
            I_Guar_Guard = I_Guar_Guard :+ st"${ops.ISZOps(methodToUse).last}(${param.name})"
          }
        case _ =>
      }

      var D_Inv_Guards: ISZ[ST] = ISZ()
      for (sortedParam <- sortParam(CEP_Post_Params.elements) if dataInvariants.contains(sortedParam.aadlType.name)) {
        val d_inv_guard_MethodName = createInvariantMethodName(sortedParam.aadlType)
        val nameToUse: ISZ[String] =
          if (sortedParam.isOptional) d_inv_guard_MethodName._2
          else d_inv_guard_MethodName._1
        D_Inv_Guards = D_Inv_Guards :+ st"${(nameToUse, ".")}(${sortedParam.name})"
      }

      if (I_Guar_Guard.nonEmpty || CEP_T_Guar.nonEmpty || CEP_T_Case.nonEmpty || D_Inv_Guards.nonEmpty) {
        // CREATE CEP-Post

        //var CEP_Post_Params: Set[GGParam] = I_Guar_Guard_Params
        var CEP_Post_blocks: ISZ[ST] = ISZ()

        // create call to the top level guarantee statements
        val CEP_Guar_call: Option[ST] = CEP_T_Guar match {
          case Some(cep_guar_content) =>
            CEP_Post_blocks = CEP_Post_blocks :+ cep_guar_content.content
            CEP_Post_Params = CEP_Post_Params ++ cep_guar_content.params
            val sortedParams = sortParam(cep_guar_content.params)
            Some(st"${ops.ISZOps(cep_guar_content.methodName).last} (${(for (p <- sortedParams) yield p.name, ", ")})")
          case _ => None()
        }

        // create call to the top level case contracts
        val CEP_T_Case_call: Option[ST] = CEP_T_Case match {
          case Some(cep_T_Case_content) =>
            CEP_Post_blocks = CEP_Post_blocks :+ cep_T_Case_content.content
            CEP_Post_Params = CEP_Post_Params ++ cep_T_Case_content.params
            val sortedParams = sortParam(cep_T_Case_content.params)
            Some(st"${ops.ISZOps(cep_T_Case_content.methodName).last} (${(for (p <- sortedParams) yield p.name, ", ")})")
          case _ => None()
        }


        // create CEP-Post that:
        //  - calls I-Guar-Guard for each output port
        //  - calls D-Inv-Guar for each type used by an output port
        //  - calls CEP-Guar
        //  - calls CEP-T-Case
        val cepPostMethodName = GumboXGen.getCompute_CEP_Post_MethodName(componentNames)
        val simpleCepPost = ops.ISZOps(cepPostMethodName).last


        @strictpure def opt(sts: ISZ[ST], desc: String): ST =
          st"""// $desc
              |${(sts, " & \n")}"""

        val sorted_Cep_Post_Params = sortParam(CEP_Post_Params.elements)

        var segments: ISZ[ST] = ISZ()
        if (D_Inv_Guards.nonEmpty) {
          segments = segments :+ opt(D_Inv_Guards, s"D-Inv-Guard: Datatype invariants for the types associated with ${component.identifier}'s state variables and outgoing ports")
        }
        if (I_Guar_Guard.nonEmpty) {
          segments = segments :+ opt(I_Guar_Guard, s"I-Guar-Guard: Integration constraints for ${component.identifier}'s outgoing ports")
        }
        if (CEP_Guar_call.nonEmpty) {
          segments = segments :+ opt(ISZ(CEP_Guar_call.get), s"CEP-Guar: guarantee clauses of ${component.identifier}'s compute entrypoint")
        }
        if (CEP_T_Case_call.nonEmpty) {
          segments = segments :+ opt(ISZ(CEP_T_Case_call.get), s"CEP-T-Case: case clauses of ${component.identifier}'s compute entrypoint")
        }

        val cep_post =
          st"""/** CEP-Post: Compute Entrypoint Post-Condition for ${component.identifier}
              |  *
              |  ${(paramsToComment(sorted_Cep_Post_Params), "\n")}
              |  */
              |@strictpure def ${simpleCepPost} (
              |    ${(for (p <- sorted_Cep_Post_Params) yield p.getParamDef, ",\n")}): B =
              |  (${(segments, " & \n\n")})"""

        val simpleCepPostContainer = ops.ISZOps(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames)).last
        val preContainerName = GumboXGenUtil.genContainerName(componentNames.componentSingletonType, T, T)
        val postContainerName = GumboXGenUtil.genContainerName(componentNames.componentSingletonType, F, T)

        val args: ISZ[ST] = for (p <- sorted_Cep_Post_Params) yield
          if (p.kind == SymbolKind.StateVarPre || (p.isInstanceOf[GGPortParam] && p.asInstanceOf[GGPortParam].isIn))
            st"${p.name} = pre.${p.name}"
          else st"${p.name} = post.${p.name}"

        val cep_post_container =
          st"""/** CEP-Post: Compute Entrypoint Post-Condition for ${component.identifier} via containers
              |  *
              |  * @param pre Container holding the values of incoming ports and the pre-state values of state variables
              |  * @param post Container holding the values of outgoing ports and the post-state values of state variables
              |  */
              |@strictpure def ${simpleCepPostContainer}(
              |    pre: ${preContainerName},
              |    post: ${postContainerName}): B =
              |  $simpleCepPost(
              |    ${(args, ",\n")})"""

        val ret = st"${(CEP_Post_blocks :+ cep_post :+ cep_post_container, "\n\n")}"

        CEP_Post = Some(ContractHolder(cepPostMethodName, sorted_Cep_Post_Params, imports, ret))
      }
    }
    computeEntryPointHolder = computeEntryPointHolder + component.path ~>
      ComputeEntryPointHolder(CEP_Pre, CEP_Post)
  }

  def processDatatypes(basePackageName: String, symbolTable: SymbolTable, aadlTypes: AadlTypes, directories: ProjectDirectories, reporter: Reporter): Unit = {
    assert(dataInvariants.isEmpty, "Unexpected: processDatatypes should only be called once per project")

    for (aadlType <- aadlTypes.typeMap.values) {
      val ais = getGclAnnexInfos(ISZ(aadlType.name), symbolTable)
      if (ais.nonEmpty) {
        assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")
        val ai = ais(0)

        resetImports()

        var ret: ISZ[ST] = ISZ()

        var methodNames: ISZ[String] = ISZ()
        for (i <- ai.annex.invariants) {
          val methodName = convertInvariantToMethodName(i.id, aadlType)

          imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(i.exp)

          val descriptor = GumboXGen.processDescriptor(i.descriptor, "*   ")
          methodNames = methodNames :+ methodName
          ret = ret :+
            st"""/** invariant ${i.id}
                |  ${descriptor}
                |  */
                |@strictpure def ${methodName}(value: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
                |  ${rewriteInvariant(getRExp(i.exp, basePackageName, aadlTypes, ai.gclSymbolTable))}"""
        }

        val objectName = GumboXGen.createInvariantObjectName(aadlType)
        val (d_inv_method_name, d_inv_guard_method_name) = createInvariantMethodName(aadlType)

        val simple = ops.ISZOps(d_inv_method_name).last
        ret = ret :+
          st"""/** D-Inv Data Invariant for ${aadlType.nameProvider.qualifiedReferencedTypeName}
              |  */
              |@strictpure def $simple(value: ${aadlType.nameProvider.qualifiedReferencedTypeName}): B =
              |  (${(methodNames, "(value) &\n")}(value))"""

        ret = ret :+
          st"""/** D-Inv-Guard Data Invariant for ${aadlType.nameProvider.qualifiedReferencedTypeName}
              |  */
              |@strictpure def ${ops.ISZOps(d_inv_guard_method_name).last}(value: Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]): B =
              |  value.nonEmpty -->: $simple(value.get)"""

        val content =
          st"""// #Sireum
              |
              |package ${aadlType.nameProvider.qualifiedPackageName}
              |
              |import org.sireum._
              |import ${basePackageName}._
              |${StubTemplate.addImports(imports)}
              |
              |${CommentTemplate.doNotEditComment_scala}
              |object ${ops.ISZOps(objectName).last} {
              |  ${(ret, "\n\n")}
              |}"""

        val path = s"${directories.dataDir}/${aadlType.nameProvider.outputDirectory}/${ops.ISZOps(objectName).last}.scala"
        dataInvariants = dataInvariants + aadlType.name ~>
          DataInvariantHolder(
            D_Inv_Method_Name = d_inv_method_name,
            D_Inv_Guard_Method_Name = d_inv_guard_method_name,
            resource = ResourceUtil.createResource(path, content, T)
          )
      }
    }
  }

  def resetImports(): Unit = {
    imports = ISZ()
  }

  def getRExp(e: AST.Exp, basePackageName: String, aadlTypes: AadlTypes, gclSymbolTable: GclSymbolTable): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(e).get)
  }

  def finalise(component: AadlThreadOrDevice, componentNames: NameProvider, projectDirectories: ProjectDirectories): ObjectContributions = {
    var resources = dataInvariants.values.map((i: DataInvariantHolder) => i.resource)

    var blocks: ISZ[ST] = ISZ()
    var imports: Set[String] = Set.empty
    integrationClauses.get(component.path) match {
      case Some(pairs) =>
        for (p <- pairs) {
          val (_, integrationHolder) = p
          blocks = blocks :+ integrationHolder.method.content :+ integrationHolder.guard.content
          imports = imports ++ integrationHolder.method.imports ++ integrationHolder.guard.imports
        }

      case _ =>
    }

    initializeEntryPointHolder.get(component.path) match {
      case Some(iholder) =>
        blocks = blocks :+ iholder.IEP_Guar.content
        imports = imports ++ iholder.IEP_Guar.imports
      case _ =>
    }

    computeEntryPointHolder.get(component.path) match {
      case Some(computeHolder) =>
        if (computeHolder.CEP_Pre.nonEmpty) {
          blocks = blocks :+ computeHolder.CEP_Pre.get.content
          imports = imports ++ computeHolder.CEP_Pre.get.imports
        }
        if (computeHolder.CEP_Post.nonEmpty) {
          blocks = blocks :+ computeHolder.CEP_Post.get.content
          imports = imports ++ computeHolder.CEP_Post.get.imports
        }
      case _ =>
    }
    if (blocks.nonEmpty) {
      val gumboxName = GumboXGen.createComponentGumboXObjectName(componentNames)
      val simpleName = ops.ISZOps(gumboxName).last

      val content =
        st"""// #Sireum
            |
            |package ${componentNames.packageName}
            |
            |import org.sireum._
            |import ${componentNames.basePackage}._
            |${StubTemplate.addImports(imports.elements)}
            |
            |${CommentTemplate.doNotEditComment_scala}
            |object ${simpleName} {
            |  ${(blocks, "\n\n")}
            |}
            |"""

      val path = s"${projectDirectories.bridgeDir}/${componentNames.packagePath}/${simpleName}.scala"
      resources = resources :+ ResourceUtil.createResource(path, content, T)
    }

    return emptyObjectContributions(resources = resources)
  }

  def createTestHarness(component: AadlThreadOrDevice, componentNames: NameProvider,
                        container: Container,
                        annexInfo: Option[(GclSubclause, GclSymbolTable)],
                        runSlangCheck: B,
                        symbolTable: SymbolTable, aadlTypes: AadlTypes, projectDirectories: ProjectDirectories): ObjectContributions = {

    resetImports()

    val testHarnessClassName = GumboXGen.createTestHarnessGumboXClassName(componentNames)
    val simpleTestHarnessName = ops.ISZOps(testHarnessClassName).last
    val simpleTestHarnessSlang2ScalaTestName = ops.ISZOps(GumboXGen.createScalaTestGumboXClassName(componentNames)).last

    var testingBlocks: ISZ[ST] = ISZ()

    var resources: ISZ[FileResource] = ISZ()

    var dscAllRandLibs: Set[String] = Set.empty

    var scalaTests: ISZ[ST] = ISZ()
    var generatorProfileEntries: ISZ[ST] = ISZ()
    var runnerProfileEntries: ISZ[ST] = ISZ()
    var nextProfileMethods: ISZ[ST] = ISZ[ST]()
    var dscTestRunners: ISZ[ST] = ISZ[ST]()


    def process(testMethodName: String,
                _dscRunnerSimpleName: String,

                suffix: String,

                entrypoint: String,
                isInitialize: B,

                captureStateVars: B,
                stateVars: ISZ[GclStateVar],

                preMethodName: Option[String], preParams: ISZ[GGParam],
                postMethodName: Option[String], postParams: ISZ[GGParam]): Unit = {

      assert(isInitialize -->: stateVars.isEmpty)

      val testCBMethodName = s"${testMethodName}CB$suffix"
      val testCBMethodNameJson = s"${testMethodName}CB${suffix}J"
      val testCBMethodNameVector = s"${testMethodName}CB${suffix}V"
      val dscRunnerSimpleName = s"${_dscRunnerSimpleName}$suffix"

      val inPortParams: ISZ[GGParam] =
        if (isInitialize) ISZ()
        else GumboXGenUtil.inPortsToParams(component, componentNames)

      var preStateParams: Set[GGParam] = Set.empty[GGParam]

      var saveInLocalAsVar: ISZ[ST] = ISZ()
      var setInLocal: ISZ[ST] = ISZ()

      for (stateVar <- stateVars) {
        val stateParam =
          GGStateVarParam(
            stateVar = stateVar,
            isPreState = T,
            aadlType = aadlTypes.typeMap.get(stateVar.classifier).get,

            componentNames = componentNames)

        preStateParams = preStateParams + stateParam

        saveInLocalAsVar = saveInLocalAsVar :+
          st"val ${stateParam.getParamDef} = ${componentNames.componentSingletonTypeQualifiedName}.${stateVar.name}"

        setInLocal = setInLocal :+
          st"${componentNames.componentSingletonTypeQualifiedName}.${stateVar.name} = ${stateParam.name}"
      }

      // ignored for initialize entrypoint and when state vars are passed in
      val step1: Option[ST] = {
        if (!isInitialize && captureStateVars)
          (if (saveInLocalAsVar.nonEmpty)
            Some(
              st"""// [SaveInLocal]: retrieve and save the current (input) values of GUMBO-declared local state variables as retrieved from the component state
                  |${(saveInLocalAsVar, "\n")}""")
          else
            Some(
              st"""// [SaveInLocal]: retrieve and save the current (input) values of GUMBO-declared local state variables as retrieved from the component state
                  |//   ${component.identifier} does not have incoming ports or state variables"""))
        else None()
      }


      val step2: Option[ST] = {
        if (!isInitialize) {
          if (preParams.nonEmpty) {
            val sortedPreParams = sortParam(preParams)
            Some(
              st"""// [CheckPre]: check/filter based on pre-condition.
                  |val CEP_Pre_Result: B = ${preMethodName.get} (${(for (sortedParam <- sortedPreParams) yield sortedParam.name, ", ")})
                  |if (!CEP_Pre_Result) {
                  |  return GumboXResult.Pre_Condition_Unsat
                  |}""")
          } else {
            Some(
              st"""// [CheckPre]: check/filter based on pre-condition.
                  |//   ${component.identifier}'s compute entry point does not have top level assume clauses""")
          }
        }
        else {
          None()
        }
      }

      var step3: Option[ST] = None()
      if (!isInitialize) {
        val putInPorts: ST = {
          if (inPortParams.nonEmpty) {
            var putters: ISZ[ST] = ISZ()
            for (inPort <- sortParam(inPortParams)) {
              val cport = symbolTable.featureMap.get(component.path :+ inPort.originName).get.asInstanceOf[AadlPort]

              if (cport.isInstanceOf[AadlDataPort]) {
                putters = putters :+ st"put_${inPort.originName}(${inPort.name})"
              } else if (cport.isInstanceOf[AadlEventPort]) {
                putters = putters :+
                  st"""if (${inPort.name}.nonEmpty) {
                      |  put_${inPort.originName}()
                      |}"""
              } else {
                putters = putters :+
                  st"""if (${inPort.name}.nonEmpty) {
                      |  put_${inPort.originName}(${inPort.name}.get)
                      |}"""
              }
            }

            st"""// [PutInPorts]: put values on the input ports
                |${(putters, "\n")}"""
          } else {
            st"""// [PutInPorts]: put values on the input ports
                |//   ${component.identifier} does not have incoming ports"""
          }
        }

        val s3: ST =
          if (captureStateVars) {
            putInPorts
          } else {
            if (setInLocal.nonEmpty) {
              st"""$putInPorts
                  |
                  |// [SetInStateVars]: set the pre-state values of state variables
                  |${(setInLocal, "\n")}"""
            } else {
              st"""$putInPorts
                  |
                  |// [SetInStateVars]: set the pre-state values of state variables
                  |//   ${component.identifier} does not contain state variables"""
            }
          }

        step3 = Some(s3)
      }

      val tq: String = "\"\"\""

      val stateAndInPorts = sortParam(inPortParams ++ preStateParams.elements)

      val step4: ST =
        st"""if (verbose) {
            |  println(st${tq}Pre State Values:
            |              ${(for (p <- stateAndInPorts) yield s"|  ${p.name} = $${${p.name}.string}", "\n")}${tq}.render)
            |}
            |
            |// [InvokeEntryPoint]: invoke the entry point test method
            |$testMethodName()"""

      var postOracleParams: Set[GGParam] = Set.empty
      var step5PostValues: ISZ[ST] = ISZ()
      var step5PostValuePrinters: ISZ[ST] = ISZ()

      if (postParams.nonEmpty) {
        for (outPortParam <- GumboXGenUtil.filterOutPorts(postParams)) {
          postOracleParams = postOracleParams + outPortParam
          val optGet: String = if (!outPortParam.isOptional) ".get" else ""
          step5PostValues = step5PostValues :+ st"val ${outPortParam.getParamDef} = get_${outPortParam.originName}()${optGet}"
          step5PostValuePrinters = step5PostValuePrinters :+ st"|  ${outPortParam.name} = $${${outPortParam.name}.string}"
        }
      }

      annexInfo match {
        case Some((annex, _)) =>
          for (stateVar <- annex.state) {
            val postSVGG = GGStateVarParam(
              stateVar = stateVar,
              isPreState = F,
              aadlType = aadlTypes.typeMap.get(stateVar.classifier).get,
              componentNames = componentNames)
            postOracleParams = postOracleParams + postSVGG
            step5PostValues = step5PostValues :+ st"val ${postSVGG.getParamDef} = ${componentNames.componentSingletonTypeQualifiedName}.${stateVar.name}"
            step5PostValuePrinters = step5PostValuePrinters :+ st"|  ${postSVGG.name} = $${${postSVGG.name}.string}"
          }
        case _ =>
      }

      val step5: ST =
        if (step5PostValues.nonEmpty)
          st"""// [RetrieveOutState]: retrieve values of the output ports via get operations and GUMBO declared local state variable
              |${(step5PostValues, "\n")}
              |
              |if (verbose) {
              |  println(st${tq}Post State Values:
              |              ${(step5PostValuePrinters, "\n")}${tq}.render)
              |}"""
        else
          st"""// [RetrieveOutState]: retrieve values of the output ports via get operations and GUMBO declared local state variable
              |//   ${component.identifier} does not have outgoing ports or state variables"""

      val step6: ST = {
        if (postParams.nonEmpty) {
          val sortedCepPostParams = sortParam(postParams)
          st"""// [CheckPost]: invoke the oracle function
              |val postResult = ${postMethodName.get}(${(for (p <- sortedCepPostParams) yield p.name, ", ")})
              |val result: GumboXResult.Type =
              |  if (!postResult) GumboXResult.Post_Condition_Fail
              |  else GumboXResult.Post_Condition_Pass"""
        } else {
          st"""// [CheckPost]: invoke the oracle function
              |//   ${component.identifier} does not contain guarantee clauses for its compute entrypoint
              |val result: GumboXResult.Type = GumboXResult.Post_Condition_Pass"""
        }
      }

      val combinedParams = sortParam(inPortParams ++ (
        if (captureStateVars) ISZ[GGParam]() else preStateParams.elements))

      val steps: ISZ[ST] = ISZ[Option[ST]](step1, step2, step3, Some(step4), Some(step5), Some(step6)).filter(f => f.nonEmpty).map(m => m.get)
      val symContainerExtractors: ISZ[ST] = for (param <- combinedParams) yield st"o.${param.name}"

     val containerSigName = container.preStateContainerSigName
      val containerType: String =
        if (captureStateVars) container.preStateContainerName_P
        else container.preStateContainerName_PS

      val jsonName: ST =
        if (!captureStateVars) DSCTemplate.jsonMethod(F, componentNames, containerType)
        else DSCTemplate.jsonMethod(F, componentNames, containerSigName)

      val _containerType: String =
        if (!captureStateVars) containerType
        else containerSigName

      val vectorExtractor: Option[ST] =
        if (!isInitialize)
          Some(
            st"""def $testCBMethodNameJson(json: String): GumboXResult.Type = {
                |  ${jsonName}(json) match {
                |    case Either.Left(o) => return $testCBMethodNameVector(o)
                |    case Either.Right(msg) => halt(msg.string)
                |  }
                |}
                |
                |def $testCBMethodNameVector(o: ${_containerType}): GumboXResult.Type = {
                |  return $testCBMethodName(${(symContainerExtractors, ", ")})
                |}
                |""")
        else None()


      val sortedParams = sortParam(inPortParams ++ (if (captureStateVars) ISZ[GGParam]() else preStateParams.elements))
      val testComputeCB =
        st"""$vectorExtractor
            |/** Contract-based test harness for the $entrypoint entry point
            |  ${(paramsToComment(sortedParams), "\n")}
            |  */
            |def $testCBMethodName(
            |    ${(for (sortedParam <- sortedParams) yield sortedParam.getParamDef, ",\n")}): GumboXResult.Type = {
            |
            |  ${(steps, "\n\n")}
            |
            |  return result
            |}"""

      testingBlocks = testingBlocks :+ testComputeCB

      // initialize entrypoints shouldn't access in ports or state variables so
      // running DSC would be pointless
      if (isInitialize) {
        scalaTests = scalaTests :+ DSCTemplate.genInitializeScalaTests(testCBMethodName, testCBMethodName)
      } else {
        var dscLocalRandLibs: ISZ[ST] = ISZ(st"var seedGen: Gen64 = Random.Gen64Impl(Xoshiro256.create)")

        var defaultProfileConstructorParams: ISZ[ST] = ISZ()

        var dscNextEntries: ISZ[ST] = ISZ()
        var nextProfileEntries: ISZ[ST] = ISZ()

        var symActuals: ISZ[ST] = ISZ()
        var symActualsPretty: ISZ[ST] = ISZ()

        generatorProfileEntries = generatorProfileEntries :+ container.genGetProfilesMethodSig(!captureStateVars)
        generatorProfileEntries = generatorProfileEntries :+ container.genGetDefaultProfile(!captureStateVars)
        runnerProfileEntries = runnerProfileEntries :+ container.genGetProfilesMethodDefault(!captureStateVars)

        for (param <- combinedParams) {

          @strictpure def wrapS(s: String, opt: B): String = if (opt) s"Option$s" else s

          dscLocalRandLibs = dscLocalRandLibs :+ st"val ranLib${param.originName}: RandomLib = RandomLib(Random.Gen64Impl(Xoshiro256.createSeed(seedGen.genU64())))"
          dscNextEntries = dscNextEntries :+ st"val ${param.name} = ranLib${param.originName}.next${wrapS(param.ranGenName, param.isOptional)}()"

          defaultProfileConstructorParams = defaultProfileConstructorParams :+ st"${param.originName} = RandomLib(Random.Gen64Impl(Xoshiro256.createSeed(seedGen.genU64())))"
          nextProfileEntries = nextProfileEntries :+ st"val ${param.name} = profile.${param.name}.next${wrapS(param.ranGenName, param.isOptional)}()"

          symActuals = symActuals :+ st"${param.name}"
          symActualsPretty = symActualsPretty :+ st"|  ${param.name} = $${o.${param.name}.string}"
        }

        dscAllRandLibs = dscAllRandLibs ++ (for (s <- dscLocalRandLibs) yield s.render)

        val nextProfileMethodName = s"next$suffix"
        val profileType = GumboXGenUtil.genProfileName(componentNames.componentSingletonType, !captureStateVars)

        nextProfileMethods = nextProfileMethods :+ DSCTemplate.genNextProfileMethod(
          nextProfileMethodName, profileType, containerType, nextProfileEntries, symActuals)

        val cbMethodToInvoke: String = s"${testCBMethodNameVector}(o)"

        val profileRunnerName = s"${testCBMethodName}_Profile_${if (captureStateVars) portsSuffix else portStateVarSuffix}"
        scalaTests = scalaTests :+
          st"""for (profile <- ${container.genGetProfilesMethodName(!captureStateVars)}) {
              |  ${profileRunnerName}(profile)
              |}"""
        scalaTests = scalaTests :+ DSCTemplate.genProfileRunner(
          profileRunnerName,
          profileType,

          testCBMethodName,
          testCBMethodNameVector,

          componentNames,
          containerType,

          nextProfileMethodName,
          symActualsPretty,
          cbMethodToInvoke)

        val nextDSCMethod = DSCTemplate.genNextDSCMethod(dscLocalRandLibs, containerType, dscNextEntries, combinedParams)
        dscTestRunners = dscTestRunners :+ DSCTemplate.dscTestRunner(
          componentNames, dscRunnerSimpleName, containerType,
          simpleTestHarnessName, nextDSCMethod, cbMethodToInvoke)
      }
    } // end process method

    val dscTestRunnerSimpleName = ops.ISZOps(GumboXGen.createDSCTestRunnerClassName(componentNames)).last

    if (component.isPeriodic() && computeEntryPointHolder.contains(component.path)) {

      val stateVars: ISZ[GclStateVar] = annexInfo match {
        case Some((annex, _)) => annex.state
        case _ => ISZ()
      }

      val (postInitMethodName, postInitParams): (Option[String], ISZ[GGParam]) = {
        initializeEntryPointHolder.get(component.path) match {
          case Some(InitializeEntryPointHolder(ContractHolder(mName, params, _, _))) => (Some(st"${(mName, ".")}".render), params)
          case _ => (None(), ISZ())
        }
      }

      val (preComputeMethodName, preComputeParams, postComputeMethodName, postComputeParams): (Option[String], ISZ[GGParam], Option[String], ISZ[GGParam]) = {
        val (_preMethodName, _preParams): (Option[String], ISZ[GGParam]) =
          computeEntryPointHolder.get(component.path).get.CEP_Pre match {
            case Some(ContractHolder(mName, params, _, _)) => (Some(st"${(mName, ".")}".render), params)
            case _ => (None(), ISZ())
          }

        val (_postMethodName, _postParams): (Option[String], ISZ[GGParam]) =
          computeEntryPointHolder.get(component.path).get.CEP_Post match {
            case Some(ContractHolder(mName, params, _, _)) => (Some(st"${(mName, ".")}".render), params)
            case _ => (None(), ISZ())
          }

        (_preMethodName, _preParams, _postMethodName, _postParams)
      }

      if (postInitMethodName.nonEmpty) {
        process(
          testMethodName = "testInitialise",
          _dscRunnerSimpleName = dscTestRunnerSimpleName,
          suffix = "",
          entrypoint = "initialise",
          isInitialize = T,
          captureStateVars = F,
          stateVars = ISZ(),
          preMethodName = None(),
          preParams = ISZ(),
          postMethodName = postInitMethodName,
          postParams = postInitParams)
      }

      process(
        testMethodName = "testCompute",
        _dscRunnerSimpleName = dscTestRunnerSimpleName,
        suffix = "",
        entrypoint = "compute",
        isInitialize = F,
        captureStateVars = T,
        stateVars = stateVars,
        preMethodName = preComputeMethodName,
        preParams = preComputeParams,
        postMethodName = postComputeMethodName,
        postParams = postComputeParams)

      if (stateVars.nonEmpty) {
        process(
          testMethodName = "testCompute",
          _dscRunnerSimpleName = dscTestRunnerSimpleName,
          suffix = "wL",
          entrypoint ="compute",
          isInitialize = F,
          captureStateVars = F,
          stateVars = stateVars,
          preMethodName = preComputeMethodName,
          preParams = preComputeParams,
          postMethodName = postComputeMethodName,
          postParams = postComputeParams)
      }
    }

    if (testingBlocks.nonEmpty) {

      val testHarnessContent =
        st"""// #Sireum
            |
            |package ${componentNames.packageName}
            |
            |import org.sireum._
            |import ${componentNames.basePackage}._
            |import ${componentNames.basePackage}.GumboXUtil.GumboXResult
            |${StubTemplate.addImports(imports)}
            |
            |${CommentTemplate.doNotEditComment_scala}
            |@msig trait ${simpleTestHarnessName} extends ${componentNames.testApisName} {
            |  def verbose: B
            |
            |  ${(testingBlocks, "\n\n")}
            |}
            |"""

      val testHarnessPath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${simpleTestHarnessName}.scala"
      resources = resources :+ ResourceUtil.createResource(testHarnessPath, testHarnessContent, T)

      resources = resources :+ TestTemplate.slang2ScalaTestWrapper(projectDirectories, componentNames, Some((simpleTestHarnessSlang2ScalaTestName, simpleTestHarnessName)))
    }

    if (scalaTests.nonEmpty) {

      val gumboxGeneratorClassName = GumboXGen.createScalaTestGumboXGeneratorClassName(componentNames)
      val simpleGeneratorName = ops.ISZOps(gumboxGeneratorClassName).last

      val testCaseContent =
        st"""package ${componentNames.packageName}
            |
            |import org.sireum._
            |import ${componentNames.packageName}._
            |import ${componentNames.basePackage}.GumboXUtil
            |import ${componentNames.basePackage}.GumboXUtil.GumboXResult
            |import ${componentNames.basePackage}.RandomLib
            |import org.sireum.Random.Gen64
            |import org.sireum.Random.Impl.Xoshiro256
            |import org.scalatest.BeforeAndAfterAll
            |
            |${CommentTemplate.doNotEditComment_scala}
            |
            |object $simpleGeneratorName {
            |
            |  // CB entrypoint id ~> (testName ~> (retry num ~> preStateContainer)))
            |  var report: HashSMap[String, HashSMap[String, Map[Z, Option[String]]]] = _
            |
            |  def resetReport(): Unit = report = HashSMap.empty
            |
            |  def updateReport(entryPoint: String, result: String, testName: String, retry: Z, jsonContainer: Option[String]): Unit = {
            |    val key = s"$$entryPoint : $$result"
            |    val cbEntries: HashSMap[String, Map[Z, Option[String]]] =
            |      if (report.contains(key)) report.get(key).get
            |      else HashSMap.empty
            |    val testNameEntries: Map[Z, Option[String]] =
            |      if (cbEntries.contains(testName)) cbEntries.get(testName).get
            |      else Map.empty
            |    assert (!testNameEntries.contains(retry))
            |
            |    report = report + key ~> (cbEntries + (testName ~> (testNameEntries + (retry ~> jsonContainer))))
            |  }
            |
            |  def emitReport(): Unit = {
            |    println("Test Results")
            |    for(e <- report.entries) {
            |      println(s"  $${e._1} = $${e._2.size}")
            |      val attempts = e._2.values.elements.foldLeft(z"0")((o, m) => o + m.size)
            |      println(s"    Total Attempts = $$attempts")
            |    }
            |  }
            |
            |  def deserializeReport(p: Os.Path): HashSMap[String, HashSMap[String, Map[Z, Option[String]]]] = {
            |    halt("Not yet")
            |  }
            |
            |  def serializeReport(dir: Os.Path): Unit = {
            |    val filename = dir / s"${simpleGeneratorName}_$${System.currentTimeMillis()}.json"
            |    //filename.write(JSON.from_artDataContent(GumboXUtil.Report(report), T))
            |    var map1: HashSMap[String, String] = HashSMap.empty
            |    for(e <- report.entries) {
            |      var map2: HashSMap[String, String] = HashSMap.empty
            |      for (t <- e._2.entries) {
            |        var map3: Map[Z, String] = Map.empty
            |        for (r <- t._2.entries) {
            |          val s: String =
            |            if (r._2.isEmpty) ""
            |            else r._2.get
            |          map3 = map3 + (r._1 ~> s)
            |        }
            |        map2 = map2 + (t._1 ~>
            |          org.sireum.Json.Printer.printMap(T, map3, org.sireum.Json.Printer.printZ _, org.sireum.Json.Printer.printString _).render)
            |      }
            |      map1 = map1 + (e._1 ~>
            |        org.sireum.Json.Printer.printHashSMap(T, map2, org.sireum.Json.Printer.printString _, org.sireum.Json.Printer.printString _).render)
            |    }
            |    val sreport = org.sireum.Json.Printer.printHashSMap(T, map1, org.sireum.Json.Printer.printString _, org.sireum.Json.Printer.printString _).render
            |    filename.write(sreport)
            |    println(s"Wrote: $${filename.toUri}")
            |  }
            |}
            |
            |import ${simpleGeneratorName}._
            |
            |trait ${simpleGeneratorName} extends ${simpleTestHarnessSlang2ScalaTestName}
            |  with BeforeAndAfterAll {
            |
            |  // return a directory path if you want to serialize the report
            |  def serializeReportPath: Option[Os.Path] = None()
            |
            |  def failOnUnsatPreconditions: B = F
            |
            |  def seedGen: Gen64 = Random.Gen64Impl(Xoshiro256.create)
            |
            |  ${(generatorProfileEntries, "\n\n")}
            |
            |  ${(nextProfileMethods, "\n\n")}
            |
            |  override def beforeAll(): Unit = resetReport()
            |
            |  override def afterAll(): Unit = {
            |    emitReport()
            |    serializeReportPath match {
            |      case Some (p) => serializeReport(p)
            |      case _ =>
            |    }
            |  }
            |
            |  ${(scalaTests, "\n\n")}
            |}"""

      val genTestCasesPath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${simpleGeneratorName}.scala"
      resources = resources :+ ResourceUtil.createResource(genTestCasesPath, testCaseContent, T)

      val gumboxTestCasesClassName = GumboXGen.createTestCasesGumboXClassName(componentNames)
      val simpleTestCasesName = ops.ISZOps(gumboxTestCasesClassName).last

      val simpleTestRunnerContent =
        st"""package ${componentNames.packageName}
            |
            |import org.sireum._
            |import ${componentNames.packageName}._
            |import ${componentNames.basePackage}._
            |import org.sireum.Random.Impl.Xoshiro256
            |
            |${CommentTemplate.safeToEditComment_scala}
            |
            |class ${simpleTestCasesName} extends ${simpleGeneratorName} {
            |
            |  // set verbose to T to see pre/post state values and generated unit tests
            |  // that can be copied/pasted to replay a test
            |  override val verbose: B = F
            |
            |  // set failOnUnsatPreconditions to T if the unit tests should fail when either
            |  // SlangCheck is never able to satisfy a datatype's filter or the generated
            |  // test vectors are never able to satisfy an entry point's assume pre-condition
            |  override val failOnUnsatPreconditions: B = F
            |
            |  ${(runnerProfileEntries, "\n\n")}
            |}"""
      val cbTestRunnerPath = s"${projectDirectories.testBridgeDir}/${componentNames.packagePath}/${simpleTestCasesName}.scala"
      resources = resources :+ ResourceUtil.createResource(cbTestRunnerPath, simpleTestRunnerContent, F)

      val dscRunnerContent = DSCTemplate.dscRunnerClass(componentNames.packageName, componentNames.basePackage, dscTestRunners)

      val testRunnerPath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${dscTestRunnerSimpleName}s.scala"
      resources = resources :+ ResourceUtil.createResource(testRunnerPath, dscRunnerContent, T)

      resources = resources :+ ResourceUtil.createResource(
        s"${projectDirectories.testUtilDir}/${componentNames.basePackage}/${DSCTemplate.recordUnsatPreObjectName}.scala",
        DSCTemplate.dscRecordUnsatPreArtifacts(componentNames.basePackage), F)
    }

    return emptyObjectContributions(resources = resources)
  }

  def getR2Exp(e: AST.Exp.Ref, basePackageName: String, aadlTypes: AadlTypes, gclSymbolTable: GclSymbolTable): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(e.asExp).get)
  }
}
