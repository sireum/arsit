package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.cli.Cli.ArsitOption
import org.sireum.ops.ISZOps

import scala.language.implicitConversions

class ArtStubGenerator {

  var outDir : File = _
  var toImpl : ISZ[(ST, ST)] = ISZ()
  var topLevelPackage: String = _
  var arsitOptions : ArsitOption = _

  def generator(dir: File, m: Aadl, packageName: String, o: ArsitOption) : Unit = {
    assert(dir.exists)

    outDir = dir
    topLevelPackage = Util.sanitizeName(packageName)
    arsitOptions = o

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

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.Process | ComponentCategory.System => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device => genThread(c)
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor=>
          println(s"Skipping: ${c.category} component: ${Util.getName(c.identifier)}")
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
    assert(m.category == ComponentCategory.Device || m.category == ComponentCategory.Thread)

    val names = Util.getNamesFromClassifier(m.classifier.get, topLevelPackage)
    val componentName = "component"
    var ports: ISZ[Port] = ISZ()

    for(f <- m.features if Util.isPort(f)) {
      ports :+= Port(f, m)
    }

    val dispatchProtocol: String = {
      Util.getDiscreetPropertyValue[ValueProp](m.properties, Util.Prop_DispatchProtocol) match {
        case Some(x) => x.value
        case _ =>
          if(m.category == ComponentCategory.Device) "Periodic"
          else ???
      }
    }

    val b = Template.bridge(topLevelPackage, names.packageName, names.bridge, dispatchProtocol, componentName, names.component, names.componentImpl, ports)
    Util.writeFile(new File(outDir, s"bridge/${names.packagePath}/${names.bridge}.scala"), b)

    if(!arsitOptions.bless) {
      val c = Template.componentTrait(topLevelPackage, names.packageName, dispatchProtocol, names.component, names.bridge, ports)
      Util.writeFile(new File(outDir, s"component/${names.packagePath}/${names.component}.scala"), c)
    }
    val ci = Template.componentImpl(topLevelPackage, names.packageName, names.component, names.bridge, names.componentImpl)
    Util.writeFile(new File(outDir, s"component/${names.packagePath}/${names.componentImpl}.scala"), ci, false)
  }

  object Template {
    @pure def bridge(topLevelPackageName: String,
                     packageName : String,
                     bridgeName : String,
                     dispatchProtocol : String,
                     componentName : String,
                     componentType : String,
                     componentImplType : String,
                     ports : ISZ[Port]) : ST = {
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
                  |  ${(ports.map(p => s"${p.name}: Port[${p.typeName}]"), ",\n")}
                  |  ) extends Bridge {
                  |
                  |  val ports : Bridge.Ports = Bridge.Ports(
                  |    all = ISZ(${(ports.map(_.name), ",\n")}),
                  |
                  |    dataIns = ISZ(${
                         (ports.withFilter(v => Util.isDataPort(v.feature) && Util.isInPort(v.feature)).map(_.name), ",\n")}),
                  |
                  |    dataOuts = ISZ(${
                         (ports.withFilter(v => Util.isDataPort(v.feature) && Util.isOutPort(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventIns = ISZ(${
                         (ports.withFilter(v => Util.isEventPort(v.feature) && Util.isInPort(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventOuts = ISZ(${
                         (ports.withFilter(v => Util.isEventPort(v.feature) && Util.isOutPort(v.feature)).map(_.name), ",\n")})
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
                  |    ${(ports.withFilter(p => Util.isEventPort(p.feature)).map(p => Template.eventPortApi(p).render), "\n\n")}
                  |
                  |    ${(ports.withFilter(p => Util.isDataPort(p.feature)).map(p => Template.dataPortApi(p).render), "\n\n")}
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
                         (ports.withFilter(v => Util.isDataPort(v.feature) && Util.isInPort(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventInPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.withFilter(v => Util.isEventPort(v.feature) && Util.isInPort(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.withFilter(v => Util.isDataPort(v.feature) && Util.isOutPort(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.withFilter(v => Util.isEventPort(v.feature) && Util.isOutPort(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    def initialise(): Unit = {
                  |      ${componentName}.${if(arsitOptions.bless) "Initialize_Entrypoint()" else "initialise()"}
                  |    }
                  |
                  |    def compute(): Unit = {
                  |      ${computeBody(bridgeName + "Id", componentName, ports, dispatchProtocol)}
                  |    }
                  |
                  |    def activate(): Unit = {
                  |      ${componentName}.${if(arsitOptions.bless) "Activate_Entrypoint()" else "activate()"}
                  |    }
                  |
                  |    def deactivate(): Unit = {
                  |      ${componentName}.${if(arsitOptions.bless) "Deactivate_Entrypoint()" else "deactivate()"}
                  |    }
                  |
                  |    def recover(): Unit = {
                  |      ${componentName}.${if(arsitOptions.bless) "Recover_Entrypoint()" else "recover()"}
                  |    }
                  |
                  |    def finalise(): Unit = {
                  |      ${componentName}.${if(arsitOptions.bless) "Finalize_Entrypoint()" else "finalise()"}
                  |    }
                  |  }
                  |}"""
    }

    @pure def computeBody(bridgeName: String, componentName: String,
                          ports: ISZ[Port], dispatchProtocol:String) : ST = {
      if(!arsitOptions.bless) {
        dispatchProtocol.toString match {
          case "Sporadic" =>
            return st"""val EventTriggered(portIds) = Art.dispatchStatus(${bridgeName})
                       |Art.receiveInput(portIds, dataInPortIds)
                       |
                       |for(portId <- portIds) {
                       |  ${var s = ""
                            for (p <- ports if Util.isEventPort(p.feature) && Util.isInPort(p.feature))
                              s += "\n" + Template.portCase(componentName, p, s == "").render
                            s
                          }
                       |}
                       |
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
          case "Periodic" =>
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
                             dispatchProtocol : String,
                             componentType : String,
                             bridgeName : String,
                             ports : ISZ[Port]) : ST = {
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
                 |
                 |  def initialise(): Unit = {}
                 |
                 |  def finalise(): Unit = {}
                 |  ${dispatchProtocol.toString match {
                        case "Sporadic" =>
                          var s = ""
                          for (p <- ports if Util.isEventPort(p.feature) && Util.isInPort(p.feature))
                            s += "\n" + Template.portCaseMethod(p).render + "\n"
                          s
                        case "Periodic" => "\ndef timeTriggered() : Unit = {}"
                      }
                    }
                 |  def activate(): Unit = {}
                 |
                 |  def deactivate(): Unit = {}
                 |
                 |  def recover(): Unit = {}
                 |}"""
    }

    @pure def addId(s: String) : String = s + "_Id"

    @pure def putValue(p: Port) : ST =
      return st"""Art.putValue(${addId(p.name)}, ${p.typeName.toString.replace(".Type", "")}${if(p.typeName == Util.EmptyType) "()" else "_Payload(value)"})"""

    @pure def apiCall(componentName : String, portName: String): String =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def getterApi(p: Port): ST = {
      return st"""def get${p.name}() : Option[${p.typeName}] = {
                 |  val value : Option[${p.typeName}] = Art.getValue(${addId(p.name)}) match {
                 |    case Some(${p.typeName.toString.replace(".Type", "")}${if (p.typeName == Util.EmptyType) "()) => Some(art.Empty())" else "_Payload(v)) => Some(v)"}
                 |    case _ => None[${p.typeName}]()
                 |  }
                 |  return value
                 |}"""
    }

    @pure def eventPortApi(p: Port) : ST = {
      if(Util.isInPort(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def send${p.name}(${if (p.typeName == Util.EmptyType) "" else s"value : ${p.typeName}"}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
        }
    }

    @pure def dataPortApi(p: Port) : ST = {
      if(Util.isInPort(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def set${p.name}(value : ${p.typeName}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
      }
    }

    @pure def portCase(cname:String, v: Port, first : B) : ST = {
      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}){
                     |  val Some(${v.typeName.toString.replace(".Type", "")}_Payload(value)) = Art.getValue(${addId(v.name)})
                     |  ${cname}.handle${v.name}(value)
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}) {
                     |  ${cname}.handle${v.name}()
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def portCaseMethod(v: Port) : ST = {
      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""def handle${v.name}(value : ${v.typeName}): Unit = {
                     |  api.logInfo(s"received ${"${value}"}")
                     |  api.logInfo("default ${v.name} implementation")
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""def handle${v.name}(): Unit = {
                     |  api.logInfo("received ${v.name}")
                     |  api.logInfo("default ${v.name} implementation")
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def componentImpl(topLevelPackageName: String,
                            packageName : String,
                            componentType : String,
                            bridgeName : String,
                            componentImplType : String) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import ${topLevelPackageName}._
                 |
                 |@record class $componentImplType (val api : ${bridgeName}.Api) ${if(arsitOptions.bless) "" else s"extends ${componentType}"} {
                 |  ${if(arsitOptions.bless) {
                      st"""def Initialize_Entrypoint(): Unit = {}
                          |
                          |def Compute_Entrypoint(): Unit = {}
                          |
                          |def Activate_Entrypoint(): Unit = {}
                          |
                          |def Deactivate_Entrypoint(): Unit = {}
                          |
                          |def Recover_Entrypoint(): Unit = {}
                          |
                          |def Finalize_Entrypoint(): Unit = {}""".render.toString
                    } else { "" }}
                 |}"""
    }

    @pure def componentImplBless(topLevelPackageName: String,
                                 packageName : String,
                                 componentType : String,
                                 bridgeName : String,
                                 componentImplType : String) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import ${topLevelPackageName}._
                 |
                 |@record class $componentImplType (val api : ${bridgeName}.Api) {
                 |
                 |  def Initialize_Entrypoint(): Unit = {}
                 |
                 |  def Compute_Entrypoint(): Unit = {}
                 |
                 |  def Activate_Entrypoint(): Unit = {}
                 |
                 |  def Deactivate_Entrypoint(): Unit = {}
                 |
                 |  def Recover_Entrypoint(): Unit = {}
                 |
                 |  def Finalize_Entrypoint(): Unit = {}
                 |}"""
    }
  }
}

object ArtStubGenerator {
  def apply(dir: File, m: Aadl, packageName: String, o: ArsitOption) = new ArtStubGenerator().generator(dir, m, packageName, o)
}