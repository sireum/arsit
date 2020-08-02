// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.StringTemplate
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.types.{AadlTypes, DataTypeNames}
import org.sireum.hamr.ir

object ArtNixTemplate {
  // @formatter:off
  @pure def portDef(portName: String,
                    portType: String): ST = {
    return st"""$portName: Option[$portType]"""
  }

  @pure def portOpt(portId: String,
                    portIdOpt: String,
                    portType: String,
                    mutable: B): ST = {
    if (mutable) {
      return st"""var ${portDef(portIdOpt, portType)} = None[$portType]()"""
    } else {
      return st"""val ${portDef(portIdOpt, portType)} = Some(${portId})"""
    }
  }

  @pure def portId(portIdName: String,
                   archPortInstanceName: String): ST = {
    return st"""val $portIdName = Arch.${archPortInstanceName}.id"""
  }

  @pure def portOptReset(portOptName: String,
                         portType: String): ST = {
    return st"""$portOptName = None[$portType]()"""
  }

  @pure def appCases(portOptName: String,
                     portId: String,
                     payloadType: DataTypeNames): ST = {
    val ret: ST =
      st"""${portOptName} match {
          |  case Some(v) => ArtNix.updateData(${portId}, ${if (payloadType.isEmptyType()) "v" else s"${payloadType.qualifiedPayloadName}(v)"})
          |  case _ =>
          |}"""
    return ret
  }

  @pure def platformPortDecl(portName: String,
                             portId: Z): ST = {
    return st"""val $portName: Art.PortId = $portId"""
  }

  @pure def platformPayload(payloadName: String,
                            fields: ISZ[ST]): ST = {
    val ret: ST =
      st"""@datatype class $payloadName(
          |  ${(fields, ",\n")}
          |) extends DataContent"""
    return ret
  }

  @pure def artNixCases(srcArchPortId: String,
                        cases: ISZ[ST]): ST = {
    val ret: ST =
      st"""r(Arch.$srcArchPortId.id) = ISZ(
          |  ${(cases, ",\n")}
          |)"""
    return ret
  }

  @pure def artNixCase(portId: String,
                       dstArchPortId: String): ST = {
    return st"(IPCPorts.$portId, Arch.$dstArchPortId.id)"
  }

  @pure def mainSend(portId: String): ST = {
    return st"""Platform.sendAsync(IPCPorts.${portId}, IPCPorts.${s"$portId"}, empty)"""
  }

  @pure def app(packageName: String,
                objectName: String,
                IPCPort_Id: String,
                period: Z,
                bridge: String,
                portIds: ISZ[ST],
                cases: ISZ[ST],
                component: ir.Component,
                isPeriodic: B,
                types: AadlTypes,
                basePackage: String
               ): ST = {
    // @formatter:off

    def localPortId(p: Port): String = {
      return s"${p.name}PortId"
    }

    def localPortIdOpt(p: Port): String = {
      return s"${localPortId(p)}Opt"
    }

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)
    val inPorts: ISZ[Port] = Util.getFeatureEnds(component.features).filter((p: ir.FeatureEnd) => CommonUtil.isInPort(p)).map((f: ir.FeatureEnd) => {
      val portName = CommonUtil.getLastName(f.identifier)
      val isTrigger: B =
        if (dispatchTriggers.isEmpty) T
        else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
      Util.getPort(f, component, types, basePackage, isTrigger, z"-1000")
    })

    var globals: ISZ[ST] = ISZ(
      st"""val entryPoints: Bridge.EntryPoints = Arch.${bridge}.entryPoints
          |val appPortId: Art.PortId = IPCPorts.${IPCPort_Id}
          |val appPortIdOpt: Option[Art.PortId] = Some(appPortId)""")

    var inits: ISZ[ST] = ISZ(st"Platform.initialise(seed, appPortIdOpt)")

    for (p <- inPorts) {
      globals = globals :+ st"val ${localPortId(p)}: Art.PortId = Arch.${bridge}.${p.name}.id"

      globals = globals :+ st"val ${localPortIdOpt(p)}: Option[Art.PortId] = Some(${localPortId(p)})"
      inits = inits :+ st"Platform.initialise(seed, ${localPortIdOpt(p)})"
    }

    var computeBody: ST = st""

    // @formatter:off
    val body: ST = {

      val loopBody: ST = {

        val receiveOnInPorts: ISZ[ST] =
          inPorts.map((p: Port) => {
            val dispatch: String = if (CommonUtil.isAadlDataPort(p.feature)) {
              "F"
            } else {
              "T"
            }
            st"""{
                |  val out = IPCPorts.emptyReceiveAsyncOut
                |  Platform.receiveAsync(${localPortIdOpt(p)}, out)
                |  out.value2 match {
                |    case Some(v: ${p.getPortTypeNames.qualifiedPayloadName}) => ArtNix.updateData(${localPortId(p)}, v)${if (!isPeriodic) s"; dispatch = ${dispatch}" else ""}
                |    case Some(v) => halt(s"Unexpected payload on port ${p.name}.  Expecting something of type ${p.getPortTypeNames.qualifiedPayloadName} but received $${v}")
                |    case None() => // do nothing
                |  }
                |}"""
          })

        if (isPeriodic) {
          st"""
              |${(receiveOnInPorts, "\n")}
              |entryPoints.compute()
              |Process.sleep($period)"""
        } else {

          st"""var dispatch = F
              |
              |${(receiveOnInPorts, "\n")}
              |if (dispatch) {
              |  entryPoints.compute()
              |  Process.sleep($period)
              |} else {
              |  Process.sleep(10)
              |}"""
        }
      }

      computeBody = loopBody

      st"""var terminated = F
          |while (!terminated) {
          |  val out = IPCPorts.emptyReceiveAsyncOut
          |  Platform.receiveAsync(appPortIdOpt, out)
          |  if (out.value2.isEmpty) {
          |    compute()
          |  } else {
          |    terminated = T
          |  }
          |}
          |exit()"""
    }

    val ret: ST =
      st"""// #Sireum

          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object ${objectName} extends App {
          |
          |  ${(globals, "\n")}
          |
          |  def initialiseArchitecture(seed: Z): Unit = {
          |    ${(inits, "\n")}
          |
          |    Art.run(Arch.ad)
          |  }
          |
          |  def initialise(): Unit = {
          |    entryPoints.initialise()
          |  }
          |
          |  def compute(): Unit = {
          |    ${computeBody}
          |  }
          |
          |  def finalise(): Unit = {
          |    entryPoints.finalise()
          |  }
          |
          |  def main(args: ISZ[String]): Z = {
          |
          |    val seed: Z = if (args.size == z"1") {
          |      val n = Z(args(0)).get
          |      if (n == z"0") 1 else n
          |    } else {
          |      1
          |    }
          |
          |    initialiseArchitecture(seed)
          |
          |    Platform.receive(appPortIdOpt, IPCPorts.emptyReceiveOut) // pause after setting up component
          |
          |    initialise()
          |
          |    Platform.receive(appPortIdOpt, IPCPorts.emptyReceiveOut) // pause after component init
          |
          |    println("${objectName} starting ...")
          |
          |    ${if (isPeriodic) "ArtNix.timeDispatch()" else "ArtNix.eventDispatch()"}
          |
          |    ${body}
          |
          |    return 0
          |  }
          |
          |  def exit(): Unit = {
          |    finalise()
          |    Platform.finalise()
          |  }
          |
          |  override def atExit(): Unit = {
          |    exit()
          |  }
          |}"""
    return ret
    // @formatter:on
  }

  @pure def ipc(packageName: String,
                ports: ISZ[ST]): ST = {

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object IPCPorts {
          |  ${(ports, "\n")}
          |
          |  def emptyReceiveOut: MBox2[Art.PortId, DataContent] = {
          |    return MBox2(-1, art.Empty())
          |  }
          |
          |  def emptyReceiveAsyncOut: MBox2[Art.PortId, Option[DataContent]] = {
          |    return MBox2(-1, None())
          |  }
          |}
          |"""
    return ret
  }

  @pure def artNix(packageName: String,
                   cases: ISZ[ST],
                   eventInPorts: ISZ[String]): ST = {

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object ArtNix {
          |
          |  val maxPortIds: Art.PortId = IPCPorts.Main + 1
          |  val timeTriggered: TimeTriggered = TimeTriggered()
          |  val noData: Option[DataContent] = None()
          |  val data: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  val connection: MS[Art.PortId, ISZ[(Art.PortId, Art.PortId)]] = {
          |    val r = MS.create[Art.PortId, ISZ[(Art.PortId, Art.PortId)]](maxPortIds, ISZ())
          |
          |    ${(cases, "\n")}
          |
          |    r
          |  }
          |  val eventInPorts: MS[Z, Art.PortId] = MSZ(
          |    ${(eventInPorts, ",\n")}
          |  )
          |  var frozen: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  var outgoing: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  var isTimeDispatch: B = F
          |
          |  def updateData(port: Art.PortId, d: DataContent): Unit = {
          |    data(port) = Some(d)
          |  }
          |
          |  def timeDispatch(): Unit = {
          |    isTimeDispatch = T
          |  }
          |
          |  def eventDispatch(): Unit = {
          |    isTimeDispatch = F
          |  }
          |
          |  def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
          |    if (isTimeDispatch) {
          |      return timeTriggered
          |    } else {
          |      var r = ISZ[Art.PortId]()
          |      for (i <- eventInPorts if data(i).nonEmpty) {
          |        r = r :+ i
          |      }
          |      return EventTriggered(r)
          |    }
          |  }
          |
          |  def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |    frozen = data
          |    for (i <- eventPortIds) {
          |      data(i) = noData
          |    }
          |  }
          |
          |  def putValue(portId: Art.PortId, data: DataContent): Unit = {
          |    outgoing(portId) = Some(data)
          |  }
          |
          |  def getValue(portId: Art.PortId): Option[DataContent] = {
          |    return frozen(portId)
          |  }
          |
          |  def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |    for (p <- dataPortIds) {
          |      outgoing(p) match {
          |        case Some(d) =>
          |          outgoing(p) = noData
          |          for(e <- connection(p)){
          |            Platform.sendAsync(e._1, e._2, d)
          |          }
          |        case _ =>
          |      }
          |    }
          |
          |    for (p <- eventPortIds) {
          |      outgoing(p) match {
          |        case Some(d) =>
          |          outgoing(p) = noData
          |          for(e <- connection(p)){
          |            Platform.sendAsync(e._1, e._2, d)
          |          }
          |        case _ =>
          |      }
          |    }
          |  }
          |
          |  def logInfo(title: String, msg: String): Unit = {
          |    print(title)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def logError(title: String, msg: String): Unit = {
          |    eprint(title)
          |    eprint(": ")
          |    eprintln(msg)
          |  }
          |
          |  def logDebug(title: String, msg: String): Unit = {
          |    print(title)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def run(): Unit = {
          |  }
          |
          |  def time(): Art.Time = {
          |    return Process.time()
          |  }
          |}
          |"""
    return ret
  }

  @pure def main(packageName: String,
                 sends: ISZ[ST]): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object Main extends App {
          |  def main(args: ISZ[String]): Z = {
          |
          |    val seed: Z = if (args.size == z"1") {
          |      val n = Z(args(0)).get
          |      if (n == z"0") 1 else n
          |    } else {
          |      1
          |    }
          |
          |    Platform.initialise(seed, None())
          |
          |    val empty = art.Empty()
          |
          |    ${(sends, "\n")}
          |
          |    Platform.finalise()
          |    return 0
          |  }
          |}
          | """
    return ret
  }

  @pure def SharedMemory(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@ext object SharedMemory {
          |  def create(id: Z): Z = $$
          |  def get(id: Z): Z = $$
          |  def send(id: Z, port: Art.PortId, d: DataContent): Unit = $$
          |  def receive(port: Art.PortId, out: MBox2[Art.PortId, DataContent]): Unit = $$
          |  def sendAsync(id: Z, port: Art.PortId, d: DataContent): B = $$
          |  def receiveAsync(port: Art.PortId, out: MBox2[Art.PortId, Option[DataContent]]): Unit = $$
          |  def remove(id: Z): Unit = $$
          |}"""
    return ret
  }

  @pure def SharedMemory_Ext(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object SharedMemory_Ext {
          |  def create(id: Z): Z = halt("stub")
          |  def get(id: Z): Z = halt("stub")
          |  def send(id: Z, port: Art.PortId, d: DataContent): Unit = halt("stub")
          |  def receive(port: Art.PortId, out: MBox2[Art.PortId, DataContent]): Unit = halt("stub")
          |  def sendAsync(id: Z, port: Art.PortId, d: DataContent): B = halt("stub")
          |  def receiveAsync(port: Art.PortId, out: MBox2[Art.PortId, Option[DataContent]]): Unit = halt("stub")
          |  def remove(id: Z): Unit = halt("stub")
          |}"""
    return ret
  }

  @pure def platform(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@ext object Platform {
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = $$
          |  def receive(portOpt: Option[Art.PortId],  out: MBox2[Art.PortId, DataContent]): Unit = $$
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = $$
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = $$
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = $$
          |  def finalise(): Unit = $$
          |}
          |"""
    return ret
  }

  @pure def PlatformExt(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object Platform_Ext {
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = halt("stub")
          |  def receive(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, DataContent]) = halt("stub")
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = halt("stub")
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = halt("stub")
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = halt("stub")
          |  def finalise(): Unit = halt("stub")
          |}
          |"""
    return ret
  }

  @pure def PlatformNix(packageName: String): ST = {

    val init: ST =
      st"""val id = seed + port
          |SharedMemory.create(id)
          |ids = ids :+ id"""

    val receive: ST =
      st"""portOpt match {
          |  case Some(port) =>
          |    out.value1 = port
          |    SharedMemory.receive(seed + port, out)
          |  case _ => halt("Unsupported receive operation without port.")
          |}"""

    val send: String = "SharedMemory.send(port, seed + port, data)"

    val sendAsync: String = "SharedMemory.sendAsync(port, seed + port, data)"

    val finalise: ST =
      st"""for (id <- ids) {
          |  SharedMemory.remove(id)
          |}"""

    val receiveAsync: ST =
      st"""portOpt match {
          |  case Some(port) => SharedMemory.receiveAsync(seed + port, out)
          |  case _ => halt("Unsupported receive operation without port.")
          |}"""

    val ret: ST =
      st"""// #Sireum
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object PlatformNix {
          |
          |  var seed: Z = 0
          |  var ids: ISZ[Z] = ISZ()
          |
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = {
          |    PlatformNix.seed = seed
          |    portOpt match {
          |      case Some(port) =>
          |        $init
          |      case _ =>
          |    }
          |  }
          |
          |  def receive(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, DataContent]): Unit = {
          |    ${receive}
          |  }
          |
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = {
          |    ${send}
          |  }
          |
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = {
          |    val r = ${sendAsync}
          |    return r
          |  }
          |
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = {
          |    ${receiveAsync}
          |  }
          |
          |  def finalise(): Unit = {
          |    ${finalise}
          |  }
          |}
          |"""
    return ret
  }

  @pure def Process(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |package $packageName
          |
          |import org.sireum._
          |import art.Art
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@ext object Process {
          |  def sleep(n: Z): Unit = $$
          |
          |  def time(): Art.Time = $$
          |}
          |"""
    return ret
  }

  @pure def ProcessExt(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art.Art
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object Process_Ext {
          |  def sleep(millis: Z): Unit = halt("stub")
          |
          |  def time(): Art.Time = halt("stub")
          |}"""
    return ret
  }

  @pure def compile(arch: Cli.ArsitPlatform.Type,
                    dirs: ProjectDirectories,
                    cOutputDir: String): ST = {

    val script_home = "${SCRIPT_HOME}"
    val cOutputDirRel = Util.relativizePaths(dirs.binDir, cOutputDir, script_home)

    val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
    val mv: ST = if (arch == Cli.ArsitPlatform.Cygwin) {
      st"mv *.exe ${script_home}/${buildDir}/"
    } else {
      st"""mv *_App ${script_home}/${buildDir}/
          |mv Main ${script_home}/${buildDir}/"""
    }
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd ${script_home}
          |mkdir -p ${buildDir}
          |mkdir -p ${cOutputDirRel}/${buildDir}
          |cd ${cOutputDirRel}/${buildDir}
          |cmake -DCMAKE_BUILD_TYPE=Release ..
          |make $$MAKE_ARGS
          |$mv"""
    return ret
  }

  @pure def run(apps: ISZ[String],
                arch: Cli.ArsitPlatform.Type): ST = {

    val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
    val ext: String = if (arch == Cli.ArsitPlatform.Cygwin) ".exe" else ""
    val stapp: ISZ[ST] = apps.map(st => {
      val prefix: String = arch match {
        case Cli.ArsitPlatform.Cygwin => "cygstart mintty /bin/bash"
        case Cli.ArsitPlatform.Linux => "x-terminal-emulator -e sh -c"
        case Cli.ArsitPlatform.MacOS => "open -a Terminal"
        case _ => halt(s"Unexpected platform ${arch}")
      }
      st"""$prefix "${buildDir}/${st}$ext" &"""
    })
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |${(stapp, "\n")}
          |read -p "Press enter to initialise components ..."
          |${buildDir}/Main$ext
          |read -p "Press enter again to start ..."
          |${buildDir}/Main$ext"""
    return ret
  }

  @pure def stop(apps: ISZ[String]): ST = {
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |APPS="${(apps, " ")}"
          |for APP in $${APPS}; do
          |  pkill -f $$APP
          |  pkill -9 -f $$APP
          |done
          |ME=`whoami`
          |
          |# message queue
          |IPCS_Q=`ipcs -q | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -q $$id;
          |done
          |
          |# shared memory
          |IPCS_Q=`ipcs -m | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -m $$id;
          |done
          |
          |# semaphores
          |IPCS_Q=`ipcs -s | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -s $$id;
          |done
          |"""
    return ret
  }


  @pure def transpiler(apps: ISZ[String],
                       forwards: ISZ[String],
                       numBits: Z,
                       maxSequenceSize: Z,
                       maxStringSize: Z,
                       customArraySizes: ISZ[String],
                       customConstants: ISZ[String],
                       stackSizeInBytes: Z,
                       extensions: ISZ[String],
                       excludes: ISZ[String],
                       buildApps: B,
                       additionalInstructions: Option[ST],
                       dirs: ProjectDirectories,
                       cOutputDir: String): (ST, CTranspilerOption) = {

    val _stackSizeInBytes: String = if (stackSizeInBytes < 0) {
      "16*1024*1024" // default set in org.sireum.transpilers.cli.cTranspiler
    } else {
      stackSizeInBytes.string
    }

    val ops = CTranspilerOption(
      help = "",
      args = ISZ(),
      sourcepath = ISZ(dirs.srcMainDir),
      output = Some(cOutputDir),
      verbose = T,
      projectName = Some("main"), // default set in org.sireum.transpilers.cli.cTranspiler
      apps = apps,
      unroll = F, // default set in org.sireum.transpilers.cli.cTranspiler
      fingerprint = 3, // default set in org.sireum.transpilers.cli.cTranspiler
      bitWidth = numBits,
      maxStringSize = maxStringSize,
      maxArraySize = maxSequenceSize,
      customArraySizes = customArraySizes,
      customConstants = customConstants,
      plugins = ISZ(),
      exts = extensions,
      forwarding = forwards,
      stackSize = Some(_stackSizeInBytes),
      excludeBuild = excludes,
      libOnly = !buildApps,
      stableTypeId = T,
      save = None(),
      load = None(),
      cmakeIncludes = ISZ()
    )

    return (transpilerX(ops, additionalInstructions, dirs), ops)
  }

  @pure def transpilerX(opts: CTranspilerOption,
                        additionalInstructions: Option[ST],
                        dirs: ProjectDirectories): ST = {

    val script_home = s"$${${Util.SCRIPT_HOME}}"

    val projHomesRel = opts.sourcepath.map((s: String) => Util.relativizePaths(dirs.binDir, s, script_home))
    val cOutputDirRel = Util.relativizePaths(dirs.binDir, opts.output.get, script_home)

    val path_sep = s"$${PATH_SEP}"

    val ret =
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |
          |if [ -z "$${SIREUM_HOME}" ]; then
          |  echo "SIREUM_HOME not set. Refer to https://github.com/sireum/kekinian/#installing"
          |  exit 1
          |fi
          |
          |PATH_SEP=":"
          |if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then
          |  PATH_SEP=";"
          |fi
          |
          |${Util.SCRIPT_HOME}=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |OUTPUT_DIR="${cOutputDirRel}"
          |
          |$${SIREUM_HOME}/bin/sireum slang transpilers c \
          |  --sourcepath "${(projHomesRel, path_sep)}" \
          |  --output-dir "$${OUTPUT_DIR}" \
          |  --name "${opts.projectName.get}" \
          |  --apps "${(opts.apps, ",")}" \
          |  --fingerprint ${opts.fingerprint} \
          |  --bits ${opts.bitWidth} \
          |  --string-size ${opts.maxStringSize} \
          |  --sequence-size ${opts.maxArraySize} \
          |  --sequence "${(opts.customArraySizes, ";")}" \
          |  --constants "${(opts.customConstants, ";")}" \
          |  --forward "${(opts.forwarding, ",")}" \
          |  --stack-size "${opts.stackSize.get}" \
          |  --stable-type-id"""

    var extras: ISZ[ST] = ISZ()

    if (opts.exts.nonEmpty) {
      val extsRel = opts.exts.map((s: String) => Util.relativizePaths(dirs.binDir, s, script_home))
      extras = extras :+
        st""" \
            |  --exts "${(extsRel, path_sep)}""""
    }

    if (opts.excludeBuild.nonEmpty) {
      extras = extras :+
        st""" \
            |  --exclude-build "${(opts.excludeBuild, ",")}""""
    }

    if (opts.libOnly) {
      extras = extras :+
        st""" \
            |  --lib-only"""
    }

    if (opts.verbose) {
      extras = extras :+
        st""" \
            |  --verbose"""
    }

    if (additionalInstructions.nonEmpty) {
      extras = extras :+
        st"""
            |
            |${additionalInstructions}"""
    }

    return st"""${ret}${(extras, "")}"""
  }

  // @formatter:on
}
