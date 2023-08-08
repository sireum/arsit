// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, GclSubclause}
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

  @pure def genContainers(component: AadlThreadOrDevice,
                          componentNames: NameProvider,
                          gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)],
                          aadlTypes: AadlTypes): ((String, ST), (String, ST)) = {
    val inPorts = inPortsToParams(component)
    val inStateVars = stateVarsToParams(gclSubclauseInfo, T, aadlTypes)
    val preContainerName = genContainerName(componentNames, T)
    val inContainer = genContainer(preContainerName, inPorts, inStateVars)

    val outPorts = outPortsToParams(component)
    val outStateVars = stateVarsToParams(gclSubclauseInfo, F, aadlTypes)
    val postContainerName = genContainerName(componentNames, F)
    val outContainer = genContainer(postContainerName, outPorts, outStateVars)

    return ((preContainerName, inContainer), (postContainerName, outContainer))
  }

  @strictpure def genContainerName(componentNames: NameProvider, isPre: B): String =
    s"${componentNames.componentSingletonType}_${if (isPre) "Pre" else "Post"}StateContainer"

  @pure def genContainer(containerName: String,
                         ports: ISZ[GGParam],
                         stateVars: ISZ[GGParam]): ST = {

    @strictpure def wrapOption(s: String, opt: B): String = if (opt) s"Option[$s]" else s

    val fieldDecls: ISZ[ST] = for (p <- sortParam(ports ++ stateVars)) yield
      st"val ${p.name}: ${wrapOption(getSlangTypeName(p.aadlType), p.isOptional)}"

    return DSCTemplate.genTestVectorContainer(containerName, fieldDecls)
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

  @pure def inPortsToParams(component: AadlThreadOrDevice): ISZ[GGParam] = {
    return portsToParams(component, T, isInPort _)
  }

  @pure def isInPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.In
  }

  @pure def outPortsToParams(component: AadlThreadOrDevice): ISZ[GGParam] = {
    return portsToParams(component, F, isOutPort _)
  }

  @pure def portsToParams(component: AadlThreadOrDevice, isIn: B, filter: AadlFeature => B): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- component.features.filter(f => filter(f))) yield p.asInstanceOf[AadlPort]
    var ret: ISZ[GGParam] = ISZ()
    for (o <- ports) {
      o match {
        case i: AadlEventPort =>
          val kind: SymbolKind.Type = if (isIn) SymbolKind.ApiVarInEvent else SymbolKind.ApiVarOutEvent
          ret = ret :+ GGParam(s"api_${o.identifier}", o.identifier, TypeUtil.EmptyType, T, kind, None(), None())
        case i: AadlEventDataPort =>
          val kind: SymbolKind.Type = if (isIn) SymbolKind.ApiVarInEventData else SymbolKind.ApiVarOutEventData
          ret = ret :+ GGParam(s"api_${o.identifier}", o.identifier, i.aadlType, T, kind, None(), None())
        case i: AadlDataPort =>
          val kind: SymbolKind.Type = if (isIn) SymbolKind.ApiVarInData else SymbolKind.ApiVarOutData
          ret = ret :+ GGParam(s"api_${o.identifier}", o.identifier, i.aadlType, F, kind, None(), None())
        case _ => halt("Infeasible")
      }
    }
    return ret
  }

  @pure def isOutPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.Out
  }

  def stateVarsToParams(gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)], isPre: B, aadlTypes: AadlTypes): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    gclSubclauseInfo match {
      case Some((GclSubclause(stateVars, _, _, _, _, _), _)) =>

        for (stateVar <- stateVars) {
          val typ = aadlTypes.typeMap.get(stateVar.classifier).get
          val kind: SymbolKind.Type = if (isPre) SymbolKind.StateVarPre else SymbolKind.StateVar
          val name = s"${if (isPre) "In_" else ""}${stateVar.name}"
          ret = ret :+ GGParam(name, stateVar.name, typ, F, kind, None(), None())
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

  @pure def getPortKind(port: AadlPort): SymbolKind.Type = {
    val ret: SymbolKind.Type = port match {
      case i: AadlEventPort => if (port.direction == Direction.In) SymbolKind.ApiVarInEvent else SymbolKind.ApiVarOutEvent
      case i: AadlEventDataPort => if (port.direction == Direction.In) SymbolKind.ApiVarInEventData else SymbolKind.ApiVarOutEventData
      case i: AadlDataPort => if (port.direction == Direction.In) SymbolKind.ApiVarInData else SymbolKind.ApiVarOutData
      case _ => halt("Infeasible")
    }
    return ret
  }

  @pure def isDataPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlDataPort]
  }

  @pure def isEventPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlEventPort]
  }

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

  def rewriteToExpX(exp: AST.Exp, context: AadlThreadOrDevice, aadlTypes: AadlTypes): GGExpParamHolder = {
    val e = EE(context, aadlTypes)
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

  @datatype class GGParam(val name: String,
                          val originName: String,
                          val aadlType: AadlType,
                          val isOptional: B,
                          val kind: SymbolKind.Type,
                          @hidden val slangType: Option[AST.Typed],
                          @hidden val origin: Option[AST.Exp]) {
    val isInPort: B = kind == SymbolKind.ApiVarInData || kind == SymbolKind.ApiVarInEventData || kind == SymbolKind.ApiVarInEvent
    val isOutPort: B = kind == SymbolKind.ApiVarOutData || kind == SymbolKind.ApiVarOutEventData || kind == SymbolKind.ApiVarOutEvent
    val isStateVar: B = kind == SymbolKind.StateVar || kind == SymbolKind.StateVarPre

    @pure def getParamDef: String = {
      return s"$name: ${getType}"
    }

    @pure def getType: String = {
      return if (isOptional) s"Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]" else aadlType.nameProvider.qualifiedReferencedTypeName
    }
  }

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: AST.Exp)

  @record class EE(context: AadlThreadOrDevice, aadlTypes: AadlTypes) extends ir.MTransformer {
    var params: Set[GGParam] = Set.empty

    override def pre_langastExpSelect(o: AST.Exp.Select): ir.MTransformer.PreResult[AST.Exp] = {
      o match {
        case AST.Exp.Select(Some(AST.Exp.Ident(AST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[AST.Typed.Name]
          val paramName = s"api_${id.value}"
          val (typ, isOptional) = getAadlType(typed)
          params = params + GGParam(paramName, id.value, typ, isOptional, getPortKind(getPort(id.value, context)), Some(typed), Some(o))
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

          val typed: AST.Typed.Name = i.attr.typedOpt match {
            case Some(atn: AST.Typed.Name) => atn
            case x => halt(s"Infeasible ${x}")
          }
          val (typ, isOptional) = getAadlType(typed)
          params = params + GGParam(name, i.id.value, typ, isOptional, kind, Some(typed), Some(o))
          AST.Exp.Ident(id = AST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.typedOpt match {
        case Some(typed: AST.Typed.Name) =>
          val (typ, isOptional) = getAadlType(typed)
          params = params + GGParam(o.id.value, o.id.value, typ, isOptional, SymbolKind.StateVar, Some(typed), Some(o))
        case _ =>
      }
      return ir.MTransformer.PreResult(F, MNone[AST.Exp]())
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
  }

  @enum object SymbolKind {
    "Integration"

    "StateVarPre"
    "StateVar"

    //"ApiVar"
    "ApiVarInEvent"
    "ApiVarInEventData"
    "ApiVarInData"

    "ApiVarOutEvent"
    "ApiVarOutEventData"
    "ApiVarOutData"

    "Parameter"
  }
}
