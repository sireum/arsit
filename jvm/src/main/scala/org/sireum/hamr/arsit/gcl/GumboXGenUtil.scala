// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, GclStateVar, GclSubclause}
import org.sireum.lang.ast.Typed
import org.sireum.lang.symbol.Resolver
import org.sireum.lang.{ast => AST}

object GumboXGenUtil {

  @pure def getRangenMethodName(a: AadlType): String = {
    a match {
      case i: BaseType => return i.slangType.name
      case i: EnumType =>
        val qname: ISZ[String] = i.nameProvider.basePackageName +: (i.nameProvider.qualifiedTypeNameI :+ "Type")
        return Resolver.typeName(ISZ(i.nameProvider.basePackageName), qname).render
      case i =>
        val qname: ISZ[String] =
          if (i == TypeUtil.EmptyType) i.nameProvider.qualifiedReferencedTypeNameI
          else i.nameProvider.basePackageName +: i.nameProvider.qualifiedReferencedTypeNameI
        return Resolver.typeName(ISZ(i.nameProvider.basePackageName), qname).render
    }
  }

  @datatype class Container(val componentSingletonType: String,
                            val packageName: String,
                            val packageNameI: ISZ[String],
                            val basePackage: String,

                            val inPorts: ISZ[GGParam],
                            val inStateVars: ISZ[GGParam],
                            val outPorts: ISZ[GGParam],
                            val outStateVars: ISZ[GGParam]) {

    val preStateContainerName: String = genContainerName(componentSingletonType, T, F)
    val preStateContainerName_wL: String = genContainerName(componentSingletonType, T, T)
    val postStateContainerName: String = genContainerName(componentSingletonType, F, F)
    val postStateContainerName_wL: String = genContainerName(componentSingletonType, F, T)

    val fqPreStateContainerName: String = s"$packageName.$preStateContainerName"
    val fqPreStateContainerName_wL: String = s"$packageName.$preStateContainerName_wL"
    val fqPostStateContainerName: String = s"$packageName.$postStateContainerName"
    val fqPostStateContainerName_wL: String = s"$packageName.$postStateContainerName_wL"

    @pure def jsonFrom(name: String): ST = {
      return st"JSON.from${Resolver.typeName(ISZ(basePackage), packageNameI :+ name)}"
    }

    val preStateContainerJsonFrom: ST = jsonFrom(preStateContainerName)
    val preStateContainerJsonFrom_wL: ST = jsonFrom(preStateContainerName_wL)
    val postStateContainerJsonFrom: ST = jsonFrom(postStateContainerName)
    val postStateContainerJsonFrom_wL: ST = jsonFrom(postStateContainerName_wL)

    def observePreStateH(containerName: String, params: ISZ[GGParam]): ST = {

      var entries: ISZ[ST] = ISZ()
      for (param <- sortParam(params)) {
        entries = entries :+ st"${param.name} = ${param.preFetch}"
      }
      return if (entries.isEmpty) st"$containerName()"
      else
        st"""${containerName}(
            |  ${(entries, ", \n")})"""
    }

    def observePreState(): ST = {
      return observePreStateH(preStateContainerName, inPorts)
    }

    def observePreState_wL(): ST = {
      return observePreStateH(preStateContainerName_wL, inPorts ++ inStateVars)
    }

    def observePostStateH(containerName: String, params: ISZ[GGParam]): ST = {
      var entries: ISZ[ST] = ISZ()
      for (outPort <- sortParam(params)) {
        entries = entries :+ st"${outPort.name} = ${outPort.postFetch}"
      }
      return if (entries.isEmpty) st"$containerName()"
      else
        st"""${containerName}(
            |  ${(entries, ",\n")})"""
    }

    def observePostState(): ST = {
      return observePostStateH(postStateContainerName, outPorts)
    }

    def observePostState_wL(): ST = {
      return observePostStateH(postStateContainerName_wL, outPorts ++ outStateVars)
    }

    def lastDataPortVars: Option[ST] = {
      var entries: ISZ[ST] = ISZ()
      for (outPort <- outPorts if outPort.kind == SymbolKind.ApiVarOutData) {
        entries = entries :+ st"var last_${outPort.name}: Option[${outPort.aadlType.nameProvider.qualifiedReferencedTypeName}] = None()"
        entries = entries :+ outPort.asInstanceOf[GGPortParam].dataPortFetch
      }
      return (if (entries.nonEmpty) Some(st"${(entries, "\n")}")
      else None())
    }

    @pure def genContainer(containerName: String,
                           params: ISZ[GGParam]): ST = {

      @strictpure def wrapOption(s: String, opt: B): String = if (opt) s"Option[$s]" else s

      val fieldDecls: ISZ[ST] = for (p <- sortParam(params)) yield
        st"val ${p.name}: ${wrapOption(getSlangTypeName(p.aadlType), p.isOptional)}"

      return DSCTemplate.genTestVectorContainer(containerName, fieldDecls)
    }

    def genContainers(): ST = {
      val containers: ISZ[ST] = ISZ(
        genContainer(preStateContainerName, inPorts),
        genContainer(preStateContainerName_wL, inPorts ++ inStateVars),
        genContainer(postStateContainerName, outPorts),
        genContainer(postStateContainerName_wL, outPorts ++ outStateVars))

      return DSCTemplate.genTestVectorContainerClass(
        packageName = packageName,
        imports = ISZ(s"$basePackage._"),
        containers = containers)
    }
  }

  @strictpure def genContainerName(singletonType: String, isPre: B, withL: B): String =
    s"${singletonType}_${if (isPre) "Pre" else "Post"}State${if (withL) "_wL" else "_"}Container"

  def generateContainer(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): Container = {
    val inPorts = inPortsToParams(component, componentNames)
    val inStateVars = stateVarsToParams(componentNames, annexInfo, T, aadlTypes)
    val outPorts = outPortsToParams(component, componentNames)
    val outStateVars = stateVarsToParams(componentNames, annexInfo, F, aadlTypes)
    return Container(
      componentSingletonType = componentNames.componentSingletonType,
      packageName = componentNames.packageName,
      packageNameI = componentNames.packageNameI,
      basePackage = componentNames.basePackage,
      inPorts = inPorts, inStateVars = inStateVars, outPorts = outPorts, outStateVars = outStateVars)
  }

  @pure def getSlangTypeName(a: AadlType): String = {
    a match {
      case i: BaseType => return i.slangType.name
      case i: EnumType => return i.nameProvider.qualifiedReferencedTypeName
      case i => return i.nameProvider.qualifiedReferencedTypeName
    }
  }

  @pure def sortParam(params: ISZ[GGParam]): ISZ[GGParam] = {
    return (for (partition <-
                   (for (kind <- SymbolKind.elements) yield ops.ISZOps(params).filter(p => p.kind == kind))) yield
      ops.ISZOps(partition).sortWith((a, b) => a.name <= b.name)).flatMap(a => a)
  }

  @pure def inPortsToParams(component: AadlThreadOrDevice, componentNames: NameProvider): ISZ[GGParam] = {
    return portsToParams(component, componentNames, T, isInPort _)
  }

  @pure def isInPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.In
  }

  @pure def portsToParams(component: AadlThreadOrDevice, componentNames: NameProvider,
                          isIn: B, filter: AadlFeature => B): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- component.features.filter(f => filter(f))) yield p.asInstanceOf[AadlPort]
    var ret: ISZ[GGParam] = ISZ()
    for (o <- ports) {
      o match {
        case i: AadlEventPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = TypeUtil.EmptyType)
        case i: AadlEventDataPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = i.aadlType)
        case i: AadlDataPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = i.aadlType)
        case _ => halt("Infeasible")
      }
    }
    return ret
  }

  @pure def outPortsToParams(component: AadlThreadOrDevice, componentNames: NameProvider): ISZ[GGParam] = {
    return portsToParams(component, componentNames, F, isOutPort _)
  }

  @pure def isOutPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.Out
  }

  def stateVarsToParams(componentNames: NameProvider, gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)], isPre: B, aadlTypes: AadlTypes): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    gclSubclauseInfo match {
      case Some((GclSubclause(stateVars, _, _, _, _, _), _)) =>

        for (stateVar <- stateVars) {
          ret = ret :+
            GGStateVarParam(
              stateVar = stateVar,
              isPreState = isPre,
              aadlType = aadlTypes.typeMap.get(stateVar.classifier).get,
              componentNames = componentNames)
        }
      case _ =>
    }
    return ret
  }

  @pure def filterOutPorts(params: ISZ[GGParam]): ISZ[GGParam] = {
    return params.filter(p => p.kind == SymbolKind.ApiVarOutData || p.kind == SymbolKind.ApiVarOutEvent || p.kind == SymbolKind.ApiVarOutEventData)
  }

  @pure def filterInPorts(params: ISZ[GGParam]): ISZ[GGParam] = {
    return params.filter(p => p.kind == SymbolKind.ApiVarInData || p.kind == SymbolKind.ApiVarInEvent || p.kind == SymbolKind.ApiVarInEventData)
  }

  @pure def getPort(portId: String, context: AadlThreadOrDevice): AadlPort = {
    context.getPorts().filter(p => p.identifier == portId) match {
      case ISZ(p) => return p
      case _ => halt(s"Couldn't find $portId")
    }
  }

  @strictpure def isDataPort(p: AadlFeature): B = p.isInstanceOf[AadlDataPort]

  @strictpure def isEventPort(p: AadlFeature): B = p.isInstanceOf[AadlEventPort]

  def getSlangType(typ: Typed, aadlTypes: AadlTypes): String = {
    @pure def toAadl(ids: ISZ[String]): String = {
      return st"${((if (ops.ISZOps(ids).last == "Type") ops.ISZOps(ids).dropRight(1) else ids), "::")}".render
    }

    typ match {
      case i: AST.Typed.Name =>
        if (i.ids == AST.Typed.optionName) {
          val typeKey = toAadl(i.args(0).asInstanceOf[AST.Typed.Name].ids)
          return (
            if (typeKey == "art::Empty")
              "Option[art.Empty]"
            else
              s"Option[${aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName}]")
        } else {
          val typeKey = toAadl(i.ids)
          return aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName
        }
      case _ => halt(s"Unexpected ${typ}")
    }
  }

  def rewriteToExpX(exp: AST.Exp, context: AadlThreadOrDevice,
                    componentNames: NameProvider, aadlTypes: AadlTypes,
                    stateVars: ISZ[GclStateVar]): GGExpParamHolder = {
    val e = EE(context, componentNames, aadlTypes, stateVars)
    val ret: GGExpParamHolder = e.transform_langastExp(exp) match {
      case MSome(x) => GGExpParamHolder(e.params, x)
      case _ => GGExpParamHolder(e.params, exp)
    }
    return ret
  }

  def paramsToComment(params: ISZ[GGParam]): ISZ[ST] = {
    var comments: ISZ[ST] = ISZ()
    for (p <- params) {
      val kind: String = p.kind match {
        case SymbolKind.Integration => "integration variable"

        case SymbolKind.StateVarPre => "pre-state state variable"
        case SymbolKind.StateVar => "post-state state variable"

        case SymbolKind.ApiVarInData => "incoming data port"
        case SymbolKind.ApiVarInEvent => "incoming event port"
        case SymbolKind.ApiVarInEventData => "incoming event data port"

        case SymbolKind.ApiVarOutData => "outgoing data port"
        case SymbolKind.ApiVarOutEvent => "outgoing event port"
        case SymbolKind.ApiVarOutEventData => "outgoing event data port"

        case SymbolKind.Parameter => "parameter to handler method"
      }
      comments = comments :+ st"* @param ${p.name} ${kind}"
    }
    return comments
  }

  @datatype trait GGParam {
    def name: String

    def originName: String

    def slangType: ST

    def preFetch: ST

    def postFetch: ST

    def isOptional: B

    def aadlType: AadlType

    def kind: SymbolKind.Type

    def ranGenName: String = {
      return GumboXGenUtil.getRangenMethodName(aadlType)
    }

    @pure def getParamDef: ST = {
      return st"$name: $slangType"
    }
  }

  @datatype class GGStateVarParam(val stateVar: GclStateVar,
                                  val isPreState: B,
                                  val aadlType: AadlType,

                                  val componentNames: NameProvider) extends GGParam {

    val name: String = s"${if (isPreState) "In_" else ""}${stateVar.name}"
    val originName: String = stateVar.name

    val isOptional: B = F

    val slangType: ST = st"${aadlType.nameProvider.qualifiedReferencedTypeName}"

    val preFetch: ST = st"${componentNames.componentSingletonTypeQualifiedName}.${originName}"

    val postFetch: ST = preFetch

    val kind: SymbolKind.Type = if (isPreState) SymbolKind.StateVarPre else SymbolKind.StateVar
  }

  @datatype class GGPortParam(val port: AadlPort,

                              val componentNames: NameProvider,
                              val aadlType: AadlType) extends GGParam {

    val name: String = s"api_${port.identifier}"
    val originName: String = port.identifier

    val isIn: B = port.direction == Direction.In

    val isData: B = port.isInstanceOf[AadlFeatureData]
    val isEvent: B = port.isInstanceOf[AadlFeatureEvent]

    val kind: SymbolKind.Type =
      if (isIn) {
        port match {
          case i: AadlEventPort => SymbolKind.ApiVarInEvent
          case i: AadlDataPort => SymbolKind.ApiVarInData
          case i: AadlEventDataPort => SymbolKind.ApiVarInEventData
          case _ => halt("Infeasible")
        }
      } else {
        port match {
          case i: AadlEventPort => SymbolKind.ApiVarOutEvent
          case i: AadlDataPort => SymbolKind.ApiVarOutData
          case i: AadlEventDataPort => SymbolKind.ApiVarOutEventData
          case _ => halt("Infeasible")
        }
      }

    val isOptional: B = isEvent

    val archPortId: ST = st"${componentNames.archInstanceName}.operational_api.${originName}_Id"

    val payloadType: String = aadlType.nameProvider.qualifiedPayloadName

    val slangType: ST =
      if (isEvent) st"Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]"
      else st"${aadlType.nameProvider.qualifiedReferencedTypeName}"

    @pure def observeInPortValue: ST = {
      return st"Art.observeInPortValue($archPortId)"
    }

    @pure def observeOutPortVariable: ST = {
      return st"Art.observeOutPortVariable($archPortId)"
    }

    @pure def preFetch: ST = {
      if (!isEvent) {
        // incoming data port so we'll assume it was initialized in the init phase
        return st"$observeInPortValue.get.asInstanceOf[$payloadType].value"
      } else if (isData) {
        // incoming event data port so need to unpack the payload if non-empty
        return (
          st"""
              |  if (${observeInPortValue}.nonEmpty)
              |    Some(${observeInPortValue}.get.asInstanceOf[${aadlType.nameProvider.qualifiedPayloadName}].value)
              |  else None()""")
      } else {
        // incoming event port so no need to unpack
        return st"$observeInPortValue.asInstanceOf[$slangType]"
      }
    }

    @pure def postFetch: ST = {
      if (!isEvent) {
        // outgoing data port that may not have received a value on this dispatch
        return (
          st"""get_$name""")
      } else if (isData) {
        // outgoing event data port so need to unpack the payload if non-empty
        return (
          st"""
              |  if (${observeOutPortVariable}.nonEmpty)
              |    Some(${observeOutPortVariable}.get.asInstanceOf[${aadlType.nameProvider.qualifiedPayloadName}].value)
              |  else None()""")
      } else {
        // outgoing event port so no need to unpack
        return st"$observeOutPortVariable.asInstanceOf[$slangType]"
      }
    }

    @pure def dataPortFetch: ST = {
      return (
        st"""def get_${name}: ${aadlType.nameProvider.qualifiedReferencedTypeName} = {
            |  $observeOutPortVariable match {
            |    case Some(${aadlType.nameProvider.qualifiedPayloadName}(value)) =>
            |      last_$name = Some(value)
            |      return value
            |    case _ => return last_$name.get
            |  }
            |}""")
    }
  }

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: AST.Exp)

  @record class EE(context: AadlThreadOrDevice,
                   componentNames: NameProvider,
                   aadlTypes: AadlTypes,
                   stateVars: ISZ[GclStateVar]) extends ir.MTransformer {

    var params: Set[GGParam] = Set.empty

    override def pre_langastExpSelect(o: AST.Exp.Select): ir.MTransformer.PreResult[AST.Exp] = {
      o match {
        case AST.Exp.Select(Some(AST.Exp.Ident(AST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[AST.Typed.Name]
          val (typ, _) = getAadlType(typed)
          val ports = context.getPorts().filter(p => p.identifier == id.value)
          assert(ports.size == 1)
          val param = GGPortParam(
            port = ports(0),
            componentNames = componentNames,
            aadlType = typ)
          params = params + param
          return ir.MTransformer.PreResult(F,
            MSome(AST.Exp.Ident(id = AST.Id(value = param.name, attr = AST.Attr(None())), attr = o.attr)))
        case _ =>
          return ir.MTransformer.PreResult(T, MNone[AST.Exp]())
      }
    }

    override def pre_langastExpInput(o: AST.Exp.Input): ir.MTransformer.PreResult[AST.Exp] = {
      val ret: AST.Exp.Ident = o.exp match {
        case i: AST.Exp.Ident =>
          val name = s"In_${i.id.value}"

          val typed: AST.Typed.Name = i.attr.typedOpt match {
            case Some(atn: AST.Typed.Name) => atn
            case x => halt(s"Infeasible ${x}")
          }

          val (typ, _) = getAadlType(typed)

          val stateVar = stateVars.filter(s => s.name == i.id.value)
          assert(stateVar.size == 1)

          params = params +
            GGStateVarParam(
              stateVar = stateVar(0),
              isPreState = T,
              aadlType = typ,
              componentNames = componentNames)
          AST.Exp.Ident(id = AST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    def getAadlType(typ: AST.Typed.Name): (AadlType, B) = {
      var isOptional: B = F
      val ids: ISZ[String] = typ match {
        case AST.Typed.Name(AST.Typed.optionName, ISZ(i: AST.Typed.Name)) =>
          isOptional = T
          i.ids
        case _ => typ.ids
      }
      val _ids: ISZ[String] =
        if (ids(ids.size - 1) == "Type") ops.ISZOps(typ.ids).dropRight(1)
        else ids

      if (_ids.size == 2 && _ids(0) == "art" && _ids(1) == "Empty") {
        return (TypeUtil.EmptyType, isOptional)
      } else if (_ids.size == 3 && _ids(0) == "org" && _ids(1) == "sireum") {
        val aadlType = TypeResolver.getAadlBaseFromSlangType(_ids)
        return (aadlTypes.typeMap.get(aadlType).get, isOptional)
      } else {
        val key = st"${(_ids, "::")}".render
        return (aadlTypes.typeMap.get(key).get, isOptional)
      }
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.typedOpt match {
        case Some(typed: AST.Typed.Name) =>
          val (typ, _) = getAadlType(typed)
          val stateVar = stateVars.filter(s => s.name == o.id.value)
          assert(stateVar.size == 1)
          params = params +
            GGStateVarParam(
              stateVar = stateVar(0),
              isPreState = F,
              aadlType = typ,
              componentNames = componentNames)
        case _ =>
      }
      return ir.MTransformer.PreResult(F, MNone[AST.Exp]())
    }
  }

  @enum object SymbolKind {
    "Integration"

    "StateVarPre"
    "StateVar"

    "ApiVarInEvent"
    "ApiVarInEventData"
    "ApiVarInData"

    "ApiVarOutEvent"
    "ApiVarOutEventData"
    "ApiVarOutData"

    "Parameter"
  }
}
