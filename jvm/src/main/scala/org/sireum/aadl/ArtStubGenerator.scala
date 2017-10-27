package org.sireum.aadl

import java.io.File

import org.sireum._
import org.sireum.Some
import org.sireum.ops._
import org.sireum.ops.ISZOps._
import org.sireum.aadl.ast.{ComponentCategory, FeatureCategory, JSON}
import scala.language.implicitConversions
import scala.collection.immutable.{Set => mSet}

object ArtStubGenerator {

  var outDir : File = null
  var toImpl : ISZ[(ST, ST)] = ISZ()
  var replaceUserImpl: Boolean = false

  type sString = scala.Predef.String
  type aString = org.sireum.String

  implicit def sireumString2ST(s:aString) : ST = st"""$s"""
  implicit def string2ST(s:sString) : ST = st"""$s"""
  implicit def string2SireumString(s:sString) : aString = org.sireum.String(s)

  def generator(dir: File, m: ast.AadlXml, replaceUserImpl : Boolean) : Unit = {
    assert(dir.exists)

    outDir = dir
    this.replaceUserImpl = replaceUserImpl

    for(c <- m.components)
      gen(c)
  }

  def gen(m: ast.Component) : ST = {
    m.category match {
      case ComponentCategory.ThreadGroup =>
        return genThreadGroup(m)
      case ComponentCategory.Thread =>
        return genThread(m)
      case _ =>
        throw new RuntimeException(s"Not handling: " + m)
    }
  }

  def genThreadGroup(m: ast.Component) : ST = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents)
      gen(c)

    return st""" """
  }

  def genThread(m: ast.Component) : ST = {
    assert(m.category == ComponentCategory.Thread)

    val packages: Seq[sString] = m.classifier match {
      case Some(x) => x.name.toString.split("::").toSeq.dropRight(1)
      case _ => Seq()
    }

    val bridgeName = Util.getBridgeName(m.identifier.get)
    val componentName = "component"
    val componentType = Util.getTypeName(m.identifier.get)
    val componentImplType = componentType + "_impl"

    var ports: ISZ[(String, String, String)] = ISZ()
    import FeatureCategory._
    for(f <- m.features) {
      val id: aString = Util.cleanName(f.identifier)
      val ptype: aString = f.classifier match {
        case Some(c) =>
          Util.cleanName(c.name) + (if (Util.isEnum(f.properties)) ".Type" else "")
        case _ => Util.EmptyType
      }

      val dir = f.direction match {
        case ast.Direction.In => "In"
        case ast.Direction.Out => "Out"
        case ast.Direction.InOut | ast.Direction.None => "???"
      }

      f.category match {
        case DataPort | EventDataPort | EventPort =>
          val kind = if(f.category == EventPort) "Event" else "Data"
          ports :+= (id, ptype, kind + dir)
        case _ =>
      }
    }

    val packageName = packages.mkString(".")

    val b = Template.bridge(
      packageName,
      bridgeName,
      componentName,
      componentType,
      componentImplType,
      ports,
    )
    Util.writeFile(new File(outDir, "bridge/" + packages.mkString("/") + "/" + bridgeName + ".scala"), b.render.toString)

    val c = Template.componentTrait(packageName, componentType, bridgeName, ports)
    Util.writeFile(new File(outDir, "component/" + packages.mkString("/") + "/" + componentType + ".scala"), c.render.toString)

    if(replaceUserImpl) {
      val ci = Template.componentImpl(packageName, componentType, bridgeName, componentImplType)
      Util.writeFile(new File(outDir, "component/" + packages.mkString("/") + "/" + componentImplType + ".scala"), ci.render.toString)
    }

    return """ """
  }

  object Template {
  @pure def apiCall(componentName : String, portName: String): aString =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def api(portName:String, portType:String) : ST = {
      return st"""@record class ${portName}Api( ${portName}Id : Art.PortId) {
                 |  def send(${portName}: ${portType}): Unit = {
                 |    Art.putValue(${portName}Id, ${portType.toString.replace(".Type", "")}_Payload(${portName}))
                 |  }
                 |}"""
    }

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def portCase(cname:String, v: (String, String, String)) : ST = {
      if(v._3.toString.contains("Data")) {
        return st"""if(portId == ${v._1}){
                   |  val ${v._2.toString.replace(".Type", "")}_Payload(value) = Art.getValue(${v._1})
                   |  ${cname}.handle${v._1}(value)
                   |}"""
      } else {
        return st"""if(portId == ${v._1}) {
                   |  ${cname}.handle${v._1}()
                   |}"""
      }
    }

    @pure def portCaseMethod(v: (String, String, String)) : ST = {
      if(v._3.toString.contains("Data")) {
        return st"""def handle${v._1}(value : ${v._2}): Unit = {
                   |  api.logInfo("received ${v._1} " + value.toString)
                   |  api.logInfo("default ${v._1} implementation")
                   |}"""
      } else {
        return st"""def handle${v._1}(): Unit = {
                   |  api.logInfo("received ${v._1}")
                   |  api.logInfo("default ${v._1} implementation")
                   |}"""
      }
    }

    @pure def bridge(packageName : String,
                     bridgeName : String,
                     componentName : String,
                     componentType : String,
                     componentImplType : String,
                     ports : ISZ[(String, String, String)]) : ST = {
      return st"""// #Sireum
                  |
                  |package $packageName
                  |
                  |import org.sireum._
                  |import art._
                  |
                  |@record class $bridgeName(
                  |  id : Art.BridgeId,
                  |  name : String,
                  |  dispatchProtocol : DispatchPropertyProtocol,
                  |
                  |  ${var s = ""
                       for(p <- ports)
                         s += s"${p._1} : Port[${p._2}],\n"
                       s.dropRight(2)
                     }
                  |  ) extends Bridge {
                  |
                  |  val ports : Bridge.Ports = Bridge.Ports(
                  |    all = ISZ(${(ISZOps(ports).foldLeft[sString]((r, v) => s"$r ${v._1},\n", "")).dropRight(2) }),
                  |
                  |    dataIns = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "DataIn")).foldLeft[sString]((r, v) => s"$r ${v._1},\n", "")).dropRight(2)}),
                  |
                  |    dataOuts = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "DataOut")).foldLeft[sString]((r, v) => s"$r ${v._1},\n", "")).dropRight(2) }),
                  |
                  |    eventIns = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "EventIn")).foldLeft[sString]((r, v) => s"$r ${v._1},\n", "")).dropRight(2) }),
                  |
                  |    eventOuts = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "EventOut")).foldLeft[sString]((r, v) => s"$r ${v._1},\n", "")).dropRight(2) })
                  |  )
                  |
                  |  val api : ${bridgeName}.Api =
                  |    ${bridgeName}.Api(
                  |      ${var s = "id"
                           for(p <- ports if p._3.toString.endsWith("Out"))
                             s += s",\n${Template.apiCall(bridgeName, p._1)}"
                           s
                         }
                  |    )
                  |
                  |  val entryPoints : Bridge.EntryPoints =
                  |    ${bridgeName}.EntryPoints(
                  |      id,
                  |
                  |      ${(ISZOps(ports).foldLeft[sString]((r, v) => s"${r}${v._1}.id,\n", ""))}
                  |      ${componentImplType}(api)
                  |    )
                  |}
                  |
                  |object $bridgeName {
                  |  ${var s = ""
                       for(p <- ports if p._3.toString.endsWith("Out"))
                         s += Template.api(p._1, p._2).render + "\n\n"
                       s
                     }
                  |  @record class Api(
                  |    ${var s = "id : Art.BridgeId"
                         for(p <- ports if p._3.toString.endsWith("Out"))
                           s += ",\n" + Template.apiSig(p._1, p._2).render
                         s
                       }
                  |    ) {
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
                  |       Art.logInfo(id, msg)
                  |    }
                  |  }
                  |
                  |  @record class EntryPoints(
                  |    $bridgeName : Art.BridgeId,
                  |    ${ISZOps(ports).foldLeft[sString]((r, v) => s"$r\n${v._1} : Art.PortId,", "") }
                  |
                  |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
                  |
                  |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "DataIn")).foldLeft[sString]((r, v) => r + v._1 + ",\n", "")).dropRight(2)})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "DataOut")).foldLeft[sString]((r, v) => r + v._1 + ",\n", "")).dropRight(2) })
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${(ISZOps(ports.withFilter(v => v._3.toString == "EventOut")).foldLeft[sString]((r, v) => r + v._1 + ",\n", "")).dropRight(2) })
                  |
                  |    def initialise(): Unit = {
                  |      ${componentName}.initialise()
                  |    }
                  |
                  |    def compute(): Unit = {
                  |      val EventTriggered(portIds) = Art.dispatchStatus(${bridgeName})
                  |      Art.receiveInput(portIds, dataInPortIds)
                  |
                  |      for(portId <- portIds) {
                  |        ${var s = ""
                             for (p <- ports if p._3.toString.endsWith("In"))
                               s += "\n" + Template.portCase(componentName, p).render
                             s
                           }
                  |      }
                  |
                  |      Art.sendOutput(eventOutPortIds, dataOutPortIds)
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

    @pure def demo() : ST =
      return st"""object Demo extends App {
                 |  art.Art.run(Arch.ad)
                 |}"""

    @pure def componentTrait(packageName : String,
                             componentType : String,
                             bridgeName : String,
                             ports : ISZ[(String, String, String)]) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |
                 |@sig trait ${componentType} {
                 |
                 |  def api : ${bridgeName}.Api
                 |
                 |  def initialise(): Unit = {}
                 |  def finalise(): Unit = {}
                 |
                 |  ${var s = ""
                      for (p <- ports if p._3.toString.endsWith("In"))
                        s += "\n" + Template.portCaseMethod(p).render
                      s
                    }
                 |}"""
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
                 |@record class $componentImplType (api : ${bridgeName}.Api) extends $componentType {}"""
    }
  }
}
