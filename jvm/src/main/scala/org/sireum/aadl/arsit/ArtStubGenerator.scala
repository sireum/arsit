package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.Some
import org.sireum.ops._
import org.sireum.ops.ISZOps._
import org.sireum.aadl.skema.ast._
import scala.language.implicitConversions

class ArtStubGenerator {

  var outDir : File = null
  var toImpl : ISZ[(ST, ST)] = ISZ()

  type sString = scala.Predef.String
  type aString = org.sireum.String

  def last(s:Name) : String = ISZOps(s.name).last
  implicit def sireumString2ST(s:aString) : ST = st"""$s"""
  implicit def string2ST(s:sString) : ST = st"""$s"""
  implicit def string2SireumString(s:sString) : aString = org.sireum.String(s)

  def generator(dir: File, m: Aadl) : Unit = {
    assert(dir.exists)

    outDir = dir

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

  def genContainer(m: Component) : ST = {
    assert(m.category == ComponentCategory.Process || m.category == ComponentCategory.System)

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.Process | ComponentCategory.System => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device => genThread(c)
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor=>
          println(s"Skipping: ${c.category} component: ${last(c.identifier)}")
        case _ => throw new RuntimeException(s"Not handling ${c.category}: ${m}")
      }
    }

    return st""" """
  }

  def genThreadGroup(m: Component) : ST = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents) {
      assert (c.category == ComponentCategory.Thread)
      genThread(c)
    }

    return st""" """
  }

  def genThread(m: Component) : ST = {
    assert(m.category == ComponentCategory.Device || m.category == ComponentCategory.Thread)

    val packages: Seq[sString] = m.classifier match {
      case Some(x) => x.name.toString.split("::").toSeq.dropRight(1)
      case _ => Seq()
    }

    val bridgeName = Util.getBridgeName(last(m.identifier))
    val componentName = "component"
    val componentType = Util.getTypeName(last(m.identifier))
    val componentImplType = componentType + "_impl"

    var ports: ISZ[(String, String, Feature)] = ISZ()

    for(f <- m.features if Util.isPort(f)) {
      val id: aString = Util.cleanName(last(f.identifier))
      val ptype: aString = f.classifier match {
        case Some(c) =>
          Util.cleanName(c.name) + (if (Util.isEnum(f.properties)) ".Type" else "")
        case _ => Util.EmptyType
      }

      ports :+= (id, ptype, f)
    }

    val dispatchProtocol: String = {
      Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.DispatchProtocol) match {
        case Some(x) => x.value
        case _ =>
          if(m.category == ComponentCategory.Device) "Periodic"
          else ???
      }
    }

    val packageName = packages.mkString(".")

    val b = Template.bridge(packageName, bridgeName, dispatchProtocol, componentName, componentType, componentImplType, ports)
    Util.writeFile(new File(outDir, "bridge/" + packages.mkString("/") + "/" + bridgeName + ".scala"), b.render.toString)

    val c = Template.componentTrait(packageName, dispatchProtocol, componentType, bridgeName, ports)
    Util.writeFile(new File(outDir, "component/" + packages.mkString("/") + "/" + componentType + ".scala"), c.render.toString)

    val ci = Template.componentImpl(packageName, componentType, bridgeName, componentImplType)
    Util.writeFile(new File(outDir, "component/" + packages.mkString("/") + "/" + componentImplType + ".scala"), ci.render.toString, false)

    return """ """
  }

  object Template {
    @pure def bridge(packageName : String,
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
                  |    all = ISZ(${ISZOps(ports).foldLeft[sString]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    dataIns = ISZ(${
                        ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isIn(v._3))
                        ).foldLeft[sString]((r, v) => s"${r}${v._1},\n", "").dropRight(2)}),
                  |
                  |    dataOuts = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isOut(v._3))
                       ).foldLeft[sString]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    eventIns = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isIn(v._3))
                       ).foldLeft[sString]((r, v) => s"${r}${v._1},\n", "").dropRight(2) }),
                  |
                  |    eventOuts = ISZ(${
                       ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isOut(v._3))
                       ).foldLeft[sString]((r, v) => s"${r}${v._1},\n", "").dropRight(2) })
                  |  )
                  |
                  |  val api : ${bridgeName}.Api =
                  |    ${bridgeName}.Api(
                  |      ${var s = "id,\n"
                           var outEventPorts = ""
                           var outDataPorts = ""
                           for(p <- ports if Util.isEventPort(p._3) && Util.isOut(p._3)) {
                             s += s"${p._1}.id,\n"
                             outEventPorts += s"${p._1}.id, "
                           }

                           for(p <- ports if Util.isDataPort(p._3)) {
                             s += s"${p._1}.id,\n"
                             outDataPorts += s"${p._1}.id, "
                           }

                           s += s"${bridgeName}.Timer(id,\n" +
                                s"  ISZ(${outDataPorts.dropRight(2)}), \n" +
                                s"  ISZ(${outEventPorts.dropRight(2)}))"
                           s
                         }
                  |    )
                  |
                  |  val entryPoints : Bridge.EntryPoints =
                  |    ${bridgeName}.EntryPoints(
                  |      id,
                  |
                  |      ${ISZOps(ports).foldLeft[sString]((r, v) => s"${r}${v._1}.id,\n", "")}
                  |      ${componentImplType}(api)
                  |    )
                  |}
                  |
                  |object $bridgeName {
                  |
                  |  @record class Timer(val bridge: Art.BridgeId,
                  |                      val dataOutPortIds: ISZ[Art.PortId],
                  |                      val eventOutPortIds: ISZ[Art.PortId]) extends art.TimerApi {
                  |    def init(e: String): art.TimerApi.EventId = {
                  |      ${"return s\"$e$hash\""}
                  |    }
                  |  }
                  |
                  |  @record class Api(
                  |    ${var s = "id : Art.BridgeId,"
                         for(p <- ports if Util.isEventPort(p._3) && Util.isOut(p._3))
                           s += s"${addId(p._1)} : Art.PortId,\n"
                           //s += ",\n" + Template.apiSig(p._1, p._2).render

                         for(p <- ports if Util.isDataPort(p._3))
                           s += s"${addId(p._1)} : Art.PortId,\n"

                         s += s"timer : Timer) {"
                         s
                       }
                  |
                  |    ${var s = ""
                         for (p <- ports if Util.isEventPort(p._3) && Util.isOut(p._3))
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
                  |      Art.logInfo(id, msg)
                  |    }
                  |  }
                  |
                  |  @record class EntryPoints(
                  |    $bridgeName : Art.BridgeId,
                  |    ${ISZOps(ports).foldLeft[sString]((r, v) => s"$r\n${addId(v._1)} : Art.PortId,", "") }
                  |
                  |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
                  |
                  |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isIn(v._3))
                         ).foldLeft[sString]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2)})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isDataPort(v._3) && Util.isOut(v._3))
                         ).foldLeft[sString]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2) })
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${
                         ISZOps(ports.withFilter(v => Util.isEventPort(v._3) && Util.isOut(v._3))
                         ).foldLeft[sString]((r, v) => r + addId(v._1) + ",\n", "").dropRight(2) })
                  |
                  |    def initialise(): Unit = {
                  |      ${componentName}.initialise()
                  |    }
                  |
                  |    def compute(): Unit = {
                  |      ${computeBody(bridgeName, componentName, ports, dispatchProtocol)}
                  |    }
                  |
                  |    def activate(): Unit = {}
                  |
                  |    def deactivate(): Unit = {}
                  |
                  |    def recover(): Unit = {}
                  |
                  |    def finalise(): Unit = {
                  |      ${componentName}.api.timer.finalise()
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
                          for (p <- ports if Util.isEventPort(p._3) && Util.isIn(p._3))
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

    @pure def componentTrait(packageName : String,
                             dispatchProtocol : String,
                             componentType : String,
                             bridgeName : String,
                             ports : ISZ[(String, String, Feature)]) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
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
                          for (p <- ports if Util.isEventPort(p._3) && Util.isIn(p._3))
                            s += "\n" + Template.portCaseMethod(p).render + "\n"
                          s
                        case "Periodic" => "\ndef timeTriggered() : Unit = {}"
                      }
                    }
                 |}"""
    }

    @pure def addId(s: String) = s + "_Id"

    // Option[${p._2.toString.replace(".Type", "")}_Payload] =
    @pure def getValue(p:(String, String, Feature)) : ST =
      return st"""val value : Option[${p._2}] = Art.getValue(${addId(p._1)}) match {
                 |  case Some(${p._2.toString.replace(".Type", "")}_Payload(v)) => Some(v)
                 |  case _ => None[${p._2}]()
                 |}"""

    @pure def putValue(p:(String, String, Feature)) : ST =
      return st"""Art.putValue(${addId(p._1)}, ${p._2.toString.replace(".Type", "")}_Payload(value))"""

    @pure def apiCall(componentName : String, portName: String): aString =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def api(p:(String, String, Feature)) : ST = {
      return st"""@record class ${p._1}Api(${addId(p._1)} : Art.PortId) {
                 |  def send(value : ${p._2}): Unit = {
                 |    ${putValue(p)}
                 |  }
                 |}"""
    }

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def eventPortApi(p: (String, String, Feature)) : ST = {
      return st"""def send${p._1}(value : ${p._2}) : Unit = {
                 |  ${putValue(p)}
                 |}"""
    }

    @pure def dataPortApi(p: (String, String, Feature)) : ST = {
      if(Util.isIn(p._3)) {
        return st"""def get${p._1}() : Option[${p._2}] = {
                   |  ${getValue(p)}
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
                     |  ${getValue(v)}
                     |  assert(value.nonEmpty)
                     |  ${cname}.handle${v._1}(value.get)
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
                     |  api.logInfo("received ${v._1} " + value.toString)
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

    @pure def componentImpl(packageName : String,
                            componentType : String,
                            bridgeName : String,
                            componentImplType : String) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |
                 |@record class $componentImplType (val api : ${bridgeName}.Api) extends $componentType {}"""
    }
  }
}

object ArtStubGenerator {
  def apply(dir: File, m: Aadl) = new ArtStubGenerator().generator(dir, m)
}