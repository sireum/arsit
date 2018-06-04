package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops.ISZOps

import scala.language.implicitConversions

class ArtStubGenerator {

  var outDir : File = _
  var toImpl : ISZ[(ST, ST)] = ISZ()
  var topLevelPackage: String = _

  def generator(dir: File, m: Aadl, packageName: String) : Unit = {
    assert(dir.exists)

    outDir = dir
    topLevelPackage = Util.sanitizeName(packageName)

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
    var ports: ISZ[(String, String, Feature)] = ISZ()

    for(f <- m.features if Util.isPort(f)) {
      val id = Util.getLastName(f.identifier)
      val ptype: String = f.classifier match {
        case Some(c) =>
          Util.cleanName(c.name) + (if (Util.isEnum(f.properties)) ".Type" else "")
        case _ => Util.EmptyType
      }

      ports :+= (id, ptype, f)
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

    val c = Template.componentTrait(topLevelPackage, names.packageName, dispatchProtocol, names.component, names.bridge, ports)
    Util.writeFile(new File(outDir, s"component/${names.packagePath}/${names.component}.scala"), c)

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
                     ports : ISZ[(String, String, Feature)]) : ST = {
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
                  |  ${var s = ""
                       for(p <- ports)
                         s += s"${p._1}: Port[${p._2}],\n"
                       s.dropRight(2)
                     }
                  |  ) extends Bridge {
                  |
                  |  val ports : Bridge.Ports = Bridge.Ports(
                  |    all = ISZ(${ISZOps(ports).foldLeft[scala.Predef.String]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    dataIns = ISZ(${
                        ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isInPort(v._3))
                        ).foldLeft[scala.Predef.String]((r, v) => s"${r}${v._1},\n", "").dropRight(2)}),
                  |
                  |    dataOuts = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isOutPort(v._3))
                       ).foldLeft[scala.Predef.String]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    eventIns = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isInPort(v._3))
                       ).foldLeft[scala.Predef.String]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    eventOuts = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isOutPort(v._3))
                       ).foldLeft[scala.Predef.String]((r, v) => s"${r}${v._1},\n", "").dropRight(2) })
                  |  )
                  |
                  |  val api : ${bridgeName}.Api =
                  |    ${bridgeName}.Api(
                  |      ${var s = "id,\n"
                           for(p <- ports if Util.isEventPort(p._3) && Util.isOutPort(p._3)) {
                             s += s"${p._1}.id,\n"
                           }

                           for(p <- ports if Util.isDataPort(p._3)) {
                             s += s"${p._1}.id,\n"
                           }

                           s.dropRight(2)
                         }
                  |    )
                  |
                  |  val entryPoints : Bridge.EntryPoints =
                  |    ${bridgeName}.EntryPoints(
                  |      id,
                  |
                  |      ${ISZOps(ports).foldLeft[String]((r, v) => s"${r}${v._1}.id,\n", "")}
                  |      ${componentImplType}(api)
                  |    )
                  |}
                  |
                  |object $bridgeName {
                  |
                  |  @record class Api(
                  |    ${var s = "id : Art.BridgeId,\n"
                         for(p <- ports if Util.isEventPort(p._3) && Util.isOutPort(p._3))
                           s += s"${addId(p._1)} : Art.PortId,\n"

                         for(p <- ports if Util.isDataPort(p._3))
                           s += s"${addId(p._1)} : Art.PortId,\n"

                         s.dropRight(2) + ") {"
                       }
                  |
                  |    ${var s = ""
                         for (p <- ports if Util.isEventPort(p._3) && Util.isOutPort(p._3))
                           s += Template.eventPortApi(p).render + "\n\n"

                         for (p <- ports if Util.isDataPort(p._3))
                           s += Template.dataPortApi(p).render + "\n\n"
                         s
                       }
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
                  |    ${ISZOps(ports).foldLeft[String]((r, v) => s"$r\n${addId(v._1)} : Art.PortId,", "") }
                  |
                  |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
                  |
                  |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isInPort(v._3))
                         ).foldLeft[scala.Predef.String]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2)})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isOutPort(v._3))
                         ).foldLeft[scala.Predef.String]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2) })
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isOutPort(v._3))
                         ).foldLeft[scala.Predef.String]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2) })
                  |
                  |    def initialise(): Unit = {
                  |      ${componentName}.initialise()
                  |    }
                  |
                  |    def compute(): Unit = {
                  |      ${computeBody(bridgeName + "Id", componentName, ports, dispatchProtocol)}
                  |    }
                  |
                  |    def activate(): Unit = {}
                  |
                  |    def deactivate(): Unit = {}
                  |
                  |    def recover(): Unit = {}
                  |
                  |    def finalise(): Unit = {
                  |      ${componentName}.finalise()
                  |    }
                  |  }
                  |}"""
    }

    @pure def computeBody(bridgeName: String, componentName: String,
                          ports: ISZ[(String, String, Feature)], dispatchProtocol:String) : ST = {
      dispatchProtocol.toString match {
        case "Sporadic" =>
          return st"""val EventTriggered(portIds) = Art.dispatchStatus(${bridgeName})
                     |Art.receiveInput(portIds, dataInPortIds)
                     |
                     |for(portId <- portIds) {
                     |  ${var s = ""
                          for (p <- ports if Util.isEventPort(p._3) && Util.isInPort(p._3))
                            s += "\n" + Template.portCase(componentName, p, s == "").render
                          s
                        }
                     |}
                     |
                     |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        case "Periodic" =>
          return st"""val TimeTriggered() = Art.dispatchStatus(${bridgeName})
                     |Art.receiveInput(ISZ[Art.PortId](), dataInPortIds)
                     |${componentName}.timeTriggered()
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
                             ports : ISZ[(String, String, Feature)]) : ST = {
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
                          for (p <- ports if Util.isEventPort(p._3) && Util.isInPort(p._3))
                            s += "\n" + Template.portCaseMethod(p).render + "\n"
                          s
                        case "Periodic" => "\ndef timeTriggered() : Unit = {}"
                      }
                    }
                 |}"""
    }

    @pure def addId(s: String) : String = s + "_Id"

    @pure def putValue(p:(String, String, Feature)) : ST =
      return st"""Art.putValue(${addId(p._1)}, ${p._2.toString.replace(".Type", "")}${if(p._2 == Util.EmptyType) "()" else "_Payload(value)"})"""

    @pure def apiCall(componentName : String, portName: String): String =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def eventPortApi(p: (String, String, Feature)) : ST = {
      return st"""def send${p._1}(${if(p._2 == Util.EmptyType) "" else s"value : ${p._2}"}) : Unit = {
                  |  ${putValue(p)}
                  |}"""
    }

    @pure def dataPortApi(p: (String, String, Feature)) : ST = {
      if(Util.isInPort(p._3)) {
        return st"""def get${p._1}() : Option[${p._2}] = {
                   |  val value : Option[${p._2}] = Art.getValue(${addId(p._1)}) match {
                   |    case Some(${p._2.toString.replace(".Type", "")}_Payload(v)) => Some(v)
                   |    case _ => None[${p._2}]()
                   |  }
                   |  return value
                   |}"""
      } else {
        return st"""def set${p._1}(value : ${p._2}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
      }
    }

    @pure def portCase(cname:String, v: (String, String, Feature), first : B) : ST = {
      v._3.category match {
        case FeatureCategory.EventDataPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v._1)}){
                     |  val Some(${v._2.toString.replace(".Type", "")}_Payload(value)) = Art.getValue(${addId(v._1)})
                     |  ${cname}.handle${v._1}(value)
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v._1)}) {
                     |  ${cname}.handle${v._1}()
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v._3.category)
      }
    }

    @pure def portCaseMethod(v: (String, String, Feature)) : ST = {
      v._3.category match {
        case FeatureCategory.EventDataPort =>
          return st"""def handle${v._1}(value : ${v._2}): Unit = {
                     |  api.logInfo(s"received ${"${value}"}")
                     |  api.logInfo("default ${v._1} implementation")
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""def handle${v._1}(): Unit = {
                     |  api.logInfo("received ${v._1}")
                     |  api.logInfo("default ${v._1} implementation")
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v._3.category)
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
                 |@record class $componentImplType (val api : ${bridgeName}.Api) extends $componentType {}"""
    }
  }
}

object ArtStubGenerator {
  def apply(dir: File, m: Aadl, packageName: String) = new ArtStubGenerator().generator(dir, m, packageName)
}