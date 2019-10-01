package org.sireum.hamr.arsit

import java.io.File
import org.sireum._
import org.sireum.hamr.ir._
import scala.language.implicitConversions

class ArtStubGenerator(dir: File,
                       m: Aadl,
                       packageName: String,
                       arsitOptions: Cli.ArsitOption,
                       types: AadlTypes)  {
  var outDir : File = _
  var toImpl : ISZ[(ST, ST)] = ISZ()
  var basePackage: String = _
  var seenComponents : HashSet[String] = HashSet.empty

  def generator() : Unit = {
    assert(dir.exists)

    outDir = dir
    basePackage = Util.sanitizeName(packageName)

    for(c <- m.components)
      gen(c)
  }

  def gen(m: Component) : Unit = {
    m.category match {
      case ComponentCategory.Process | ComponentCategory.System =>
        genContainer(m)
      case ComponentCategory.ThreadGroup =>
        genThreadGroup(m)
      case _ =>
        for(_c <- m.subComponents)
          gen(_c)
    }
  }

  def genContainer(m: Component) : Unit = {
    assert(m.category == ComponentCategory.Process || m.category == ComponentCategory.System)

    if(!Util.isThread(m)) {
      genSubprograms(m)
    }

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.Process | ComponentCategory.System => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device =>
          if(Util.isThread(c) || arsitOptions.devicesAsThreads) {
            genThread(c)
          }
        case ComponentCategory.Subprogram => // ignore
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor=>
          Util.report(s"Skipping: ${c.category} component: ${Util.getName(c.identifier)}", T)
        case _ => throw new RuntimeException(s"Not handling ${c.category}: ${m}")
      }
    }
  }

  def genThreadGroup(m: Component) : Unit = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for (c <- m.subComponents) {
      assert(c.category == ComponentCategory.Thread)
      genThread(c)
    }
  }

  def genThread(m: Component) : Unit = {
    assert(Util.isDevice(m) || Util.isThread(m))

    val names = Util.getNamesFromClassifier(m.classifier.get, basePackage)
    val filename = s"component/${names.packagePath}/${names.componentImpl}.scala"

    if(seenComponents.contains(filename)) {
      return
    }

    val componentName = "component"
    var ports: ISZ[Port] = ISZ()

    for(f <- Util.getFeatureEnds(m.features) if Util.isPort(f)) {
      val pType = Util.getFeatureEndType(f, types)
      ports :+= Port(f, m, pType, basePackage)
    }

    val dispatchProtocol = Util.getSlangEmbeddedDispatchProtocol(m)

    val b = Template.bridge(basePackage, names.packageName, names.bridge, dispatchProtocol, componentName, names.component, names.componentImpl, ports)
    Util.writeFile(new File(outDir, s"bridge/${names.packagePath}/${names.bridge}.scala"), b)

    if(!arsitOptions.bless) {
      val c = Template.componentTrait(basePackage, names.packageName, dispatchProtocol, names.component, names.bridge, ports)
      Util.writeFile(new File(outDir, s"component/${names.packagePath}/${names.component}.scala"), c)
    }

    val block = Template.componentImplBlock(names.component, names.bridge, names.componentImpl, dispatchProtocol, ports)
    val ci = Template.slangPreamble(T, names.packageName, basePackage,
      genSubprograms(m) match {
        case Some(x) => ISZ(block, x)
        case _ => ISZ(block)
      })

    Util.writeFile(new File(outDir, filename), ci, F)

    seenComponents = seenComponents + filename
  }

  def genSubprograms(m: Component) : Option[ST] = {
    val subprograms: ISZ[(ST, ST)] = m.subComponents.filter(p => p.category == ComponentCategory.Subprogram).map(p => {
      // only expecting in or out parameters
      Util.getFeatureEnds(p.features).elements.forall(f => f.category == FeatureCategory.Parameter && f.direction != Direction.InOut)

      val methodName = Util.getLastName(p.identifier)
      val params: ISZ[String] = Util.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && Util.isInFeature(f))
        .map(param => {
          val pType = Util.getFeatureEndType(param, types)
          s"${Util.getLastName(param.identifier)} : ${Util.getDataTypeNames(pType, basePackage).referencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = Util.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && Util.isOutFeature(f))
      assert(rets.size == 1)
      val rType = Util.getFeatureEndType(rets(0), types)
      val returnType = Util.getDataTypeNames(rType, basePackage).referencedTypeName

      Template.subprogram(methodName, params, returnType)
    })

    if (subprograms.nonEmpty) {
      val names = Util.getNamesFromClassifier(m.classifier.get, basePackage)
      val objectName = s"${names.component}_subprograms"

      val body = Template.slangBody("@ext ", objectName, subprograms.map(_._1))

      if(!Util.isThread(m)) {
        val a = Template.slangPreamble(T, basePackage, names.packageName, ISZ(body))
        Util.writeFile(new File(outDir, s"component/${names.packagePath}/${objectName}.scala"), a)
      }

      val b = Template.slangPreamble(F, basePackage, names.packageName,
        ISZ(Template.slangBody("", s"${objectName}_Ext", subprograms.map(_._2))))
      Util.writeFile(new File(outDir, s"component/${names.packagePath}/${objectName}_Ext.scala"), b, F)

      return Some(body)
    } else {
      return None[ST]()
    }
  }

  // @formatter:off
  object Template {
    @pure def bridge(topLevelPackageName: String,
                     packageName : String,
                     bridgeName : String,
                     dispatchProtocol : DispatchProtocol.Type,
                     componentName : String,
                     componentType : String,
                     componentImplType : String,
                     ports : ISZ[Port]) : ST = {

      val entryPoints = EntryPoints.elements.filter(f => f != EntryPoints.compute).map(m =>
        st"""def ${m.string}: Unit = {
            |  ${componentName}.${m.string}()
            |}""")

      return st"""// #Sireum
                  |
                  |package $packageName
                  |
                  |import org.sireum._
                  |import art._
                  |import ${topLevelPackageName}._
                  |
                  |${Util.doNotEditComment()}
                  |
                  |@record class $bridgeName(
                  |  val id: Art.BridgeId,
                  |  val name: String,
                  |  val dispatchProtocol: DispatchPropertyProtocol,
                  |
                  |  ${(ports.map(p => s"${p.name}: Port[${p.portType.qualifiedReferencedTypeName}]"), ",\n")}
                  |  ) extends Bridge {
                  |
                  |  val ports : Bridge.Ports = Bridge.Ports(
                  |    all = ISZ(${(ports.map(_.name), ",\n")}),
                  |
                  |    dataIns = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isInFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    dataOuts = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isOutFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventIns = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isInFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventOuts = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isOutFeature(v.feature)).map(_.name), ",\n")})
                  |  )
                  |
                  |  val api : ${bridgeName}.Api =
                  |    ${bridgeName}.Api(
                  |      id,
                  |      ${(ports.map(p => s"${p.name}.id"), ",\n")}
                  |    )
                  |
                  |  val entryPoints : Bridge.EntryPoints =
                  |    ${bridgeName}.EntryPoints(
                  |      id,
                  |
                  |      ${(ports.map(p => s"${p.name}.id"), ",\n")},
                  |
                  |      ${componentImplType}(api)
                  |    )
                  |}
                  |
                  |object $bridgeName {
                  |
                  |  @record class Api(
                  |    id : Art.BridgeId,
                  |    ${(ports.map(p => s"${addId(p.name)} : Art.PortId"), ",\n")}) {
                  |
                  |    ${(ports.filter(p => Util.isEventPort(p.feature)).map(p => Template.eventPortApi(p).render), "\n\n")}
                  |
                  |    ${(ports.filter(p => Util.isDataPort(p.feature)).map(p => Template.dataPortApi(p).render), "\n\n")}
                  |
                  |    def logInfo(msg: String): Unit = {
                  |      Art.logInfo(id, msg)
                  |    }
                  |
                  |    def logDebug(msg: String): Unit = {
                  |      Art.logDebug(id, msg)
                  |    }
                  |
                  |    def logError(msg: String): Unit = {
                  |      Art.logError(id, msg)
                  |    }
                  |  }
                  |
                  |  @record class EntryPoints(
                  |    ${bridgeName}Id : Art.BridgeId,
                  |
                  |    ${(ports.map(p => s"${addId(p.name)} : Art.PortId"), ",\n")},
                  |
                  |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
                  |
                  |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isInFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventInPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isInFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isOutFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isOutFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    def compute(): Unit = {
                  |      ${computeBody(bridgeName + "Id", componentName, ports, dispatchProtocol)}
                  |    }
                  |
                  |    ${(entryPoints, "\n\n")}
                  |  }
                  |}"""
    }

    @pure def computeBody(bridgeName: String,
                          componentName: String,
                          ports: ISZ[Port],
                          dispatchProtocol: DispatchProtocol.Type) : ST = {
      if(!arsitOptions.bless) {
        dispatchProtocol match {
          case DispatchProtocol.Sporadic =>
            return st"""val EventTriggered(portIds) = Art.dispatchStatus(${bridgeName})
                       |Art.receiveInput(portIds, dataInPortIds)
                       |
                       |for(portId <- portIds) {
                       |  ${var s = ""
                            for (p <- ports if Util.isEventPort(p.feature) && Util.isInFeature(p.feature))
                              s += "\n" + Template.portCase(componentName, p, s == "").render
                            s
                          }
                       |}
                       |
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
          case DispatchProtocol.Periodic =>
            return st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                       |${componentName}.timeTriggered()
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        }
      } else {
        return st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                   |${componentName}.Compute_Entrypoint()
                   |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
      }
    }

    @pure def demo() : ST =
      return st"""object Demo extends App {
                 |  art.Art.run(Arch.ad)
                 |}"""

    @pure def componentTrait(topLevelPackageName: String,
                             packageName : String,
                             dispatchProtocol : DispatchProtocol.Type,
                             componentType : String,
                             bridgeName : String,
                             ports : ISZ[Port]) : ST = {
      val entryPoints = EntryPoints.elements.filter(f => f != EntryPoints.compute).map(m => st"def ${m.string}(): Unit = {}")

      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import ${topLevelPackageName}._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@msig trait ${componentType} {
                 |
                 |  def api : ${bridgeName}.Api
                 |  ${dispatchProtocol match {
                        case DispatchProtocol.Sporadic =>
                          var s = ""
                          for (p <- ports if Util.isEventPort(p.feature) && Util.isInFeature(p.feature))
                            s += "\n" + Template.portCaseMethod(p, F).render + "\n"
                          s
                        case DispatchProtocol.Periodic => "\ndef timeTriggered() : Unit = {}\n"
                      }
                    }
                 |  ${(entryPoints, "\n\n")}
                 |}"""
    }

    @pure def addId(s: String) : String = s + "_Id"

    @pure def putValue(p: Port) : ST =
      return st"""Art.putValue(${addId(p.name)}, ${p.portType.qualifiedPayloadName}${if(Util.isEmptyType(p.portType)) "()" else "(value)"})"""

    @pure def apiCall(componentName : String, portName: String): String =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def getterApi(p: Port): ST = {
      val typeName = p.portType.qualifiedReferencedTypeName
      return st"""def get${p.name}() : Option[${typeName}] = {
                 |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
                 |    case Some(${typeName.toString.replace(".Type", "")}${if (Util.isEmptyType(p.portType)) "()) => Some(art.Empty())" else "_Payload(v)) => Some(v)"}
                 |    case _ => None[${typeName}]()
                 |  }
                 |  return value
                 |}"""
    }

    @pure def eventPortApi(p: Port) : ST = {
      if(Util.isInFeature(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def send${p.name}(${if (Util.isEmptyType(p.portType)) "" else s"value : ${p.portType.qualifiedReferencedTypeName}"}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
        }
    }

    @pure def dataPortApi(p: Port) : ST = {
      if(Util.isInFeature(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def set${p.name}(value : ${p.portType.qualifiedReferencedTypeName}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
      }
    }

    @pure def portApiUsage(p: Port): ST = {
      if(Util.isInFeature(p.feature)) {
        val typeName = p.portType.qualifiedReferencedTypeName
        return st"val apiUsage_${p.name}: Option[${typeName}] = api.get${p.name}()"
      } else {
        val payload = if(Util.isEmptyType(p.portType)) {
          ""
        } else {
          p.portType.empty()
        }
        val methodName = if(Util.isDataPort(p.feature)) {
          "set"
        } else {
          "send"
        }
        return st"api.${methodName}${p.name}($payload)"
      }
    }

    @pure def portCase(cname:String, v: Port, first : B) : ST = {
      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}){
                     |  val Some(${v.portType.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
                     |  ${cname}.handle${v.name}(value)
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}) {
                     |  ${cname}.handle${v.name}()
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def portCaseMethod(v: Port, isImpl: B) : ST = {
      val or = if(isImpl){ "override "} else { "" }
      val ed = if(isImpl) { "example" } else { "default" }
      val methodName = s"handle${v.name}"

      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""${or}def $methodName(value : ${v.portType.qualifiedReferencedTypeName}): Unit = {
                     |  api.logInfo(s"received ${"${value}"}")
                     |  api.logInfo("${ed} $methodName implementation")
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""${or}def $methodName(): Unit = {
                     |  api.logInfo("received ${v.name}")
                     |  api.logInfo("${ed} $methodName implementation")
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def componentImplBlock(componentType : String,
                                 bridgeName : String,
                                 componentImplType : String,
                                 dispatchProtocol: DispatchProtocol.Type,
                                 ports: ISZ[Port]) : ST = {

      val init =  st"""override def ${EntryPoints.initialise.string}(): Unit = {
                      |  // example api usage
                      |
                      |  api.logInfo("Example info logging")
                      |  api.logDebug("Example debug logging")
                      |  api.logError("Example error logging")
                      |
                      |  ${(ports.map(p => portApiUsage(p)), "\n")}
                      |}"""

      val entryPoints = EntryPoints.elements.filter(f => f != EntryPoints.compute && f != EntryPoints.initialise).map(m => {
          st"""override def ${m.string}(): Unit = {
              |  // example override of ${m.string}
              |}"""
        })

      val eventHandlers = if(dispatchProtocol == DispatchProtocol.Periodic) {
        ISZ(st"""override def timeTriggered(): Unit = {
                |  // example override of timeTriggered
                |}""")
      } else {
        ports.filter(p => Util.isInFeature(p.feature)).map(m => portCaseMethod(m, T))
      }

      return st"""// the contents of this file will not be overwritten
                 |@record class $componentImplType (val api : ${bridgeName}.Api) ${if(arsitOptions.bless) "" else s"extends ${componentType}"} {
                 |
                 |  ${init}
                 |
                 |  ${(eventHandlers, "\n\n")}
                 |
                 |  ${(entryPoints, "\n\n")}
                 |}"""
    }

    @pure def subprogram(methodName: String,
                         params: ISZ[String],
                         returnType : String): (ST, ST) = {
      return (st"""def ${methodName}(${(params, ",\n")}): ${returnType} = ${"$"}""",
              st"""def ${methodName}(${(params, ",\n")}): ${returnType} = {
                  |  ${if(returnType != org.sireum.String("")) s"return ${returnType}()" else ""}
                  |}""")
    }

    @pure def slangPreamble(inSlang: B,
                            packageName: String,
                            topLevelPackageName: String,
                            blocks: ISZ[ST]): ST = {
      return st"""${if(inSlang) { "// #Sireum\n\n"} else ""}package $packageName
                 |
                 |import org.sireum._
                 |import ${basePackage}._
                 |
                 |${(blocks, "\n\n")}
                 |"""
    }

    @pure def slangBody(slangAnnotation: String,
                        objectName: String,
                        body: ISZ[ST]) : ST = {
      return st"""${slangAnnotation}object ${objectName} {
                 |
                 |  ${(body, "\n\n")}
                 |}"""
    }
  }
  // @formatter:on
}


object ArtStubGenerator {
  def apply(dir: File, m: Aadl, packageName: String, o: Cli.ArsitOption, types: AadlTypes) =
    new ArtStubGenerator(dir, m, packageName, o, types).generator()
}