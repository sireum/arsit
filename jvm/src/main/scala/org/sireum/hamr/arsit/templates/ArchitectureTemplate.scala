// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.ir.{Direction, FeatureCategory}

object ArchitectureTemplate {

  @pure def doNotEditComment(from: Option[String]): ST = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return st"// This file was auto-generated${_from}.  Do not edit"
  }

  @pure def dispatchProtocol(dp: Dispatch_Protocol.Type, period: Z): ST = {
    val ret: ST = dp match {
      case Dispatch_Protocol.Sporadic => st"Sporadic(min = $period)"
      case Dispatch_Protocol.Periodic => st"Periodic(period = $period)"
    }
    return ret
  }


  @pure def connection(from: String, to: String): ST = {
    return st"""Connection(from = $from, to = $to)"""
  }

  @pure def demo(packageName: String,
                 architectureName: String,
                 architectureDescriptionName: String): ST = {
    val ad = s"${architectureName}.${architectureDescriptionName}"

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art.scheduling.Scheduler
          |
          |${StringTemplate.safeToEditComment()}
          |
          |object Demo extends App {
          |
          |  /** the scheduler to use for JVM based simulation as well as the
          |    * 'default' scheduler that will be used when taking this program
          |    * down to C/Linux.  Refer to 'bin/run.sh -h' if you want to use a
          |    * specific scheduler for C.  If the scheduler accepts a schedule
          |    * and you want to provide that in C then just pass None()
          |    *
          |    * If you want to use the legacy scheduler for C then you must use
          |    *   transpile.cmd --legacy
          |    * and
          |    *   run --legacy
          |    */
          |  def defaultScheduler(): Scheduler = {
          |    return Schedulers.getRoundRobinScheduler(None())
          |  }
          |
          |  def main(args: ISZ[String]): Z = {
          |    val scheduler: Scheduler = Cli(' ').parseRun(args, 0) match {
          |      case Some(o: Cli.RunOption) =>
          |        o.scheduler match {
          |          case Cli.RunChoice.Default => defaultScheduler()
          |          case Cli.RunChoice.RoundRobin => Schedulers.getRoundRobinScheduler(None())
          |          case Cli.RunChoice.Static => Schedulers.getStaticScheduler(None())
          |          case Cli.RunChoice.Legacy => Schedulers.getLegacyScheduler()
          |        }
          |      case Some(o: Cli.HelpOption) =>
          |        Os.exit(0)
          |        halt("")
          |      case _ =>
          |        eprintln("Could not recognize arguments")
          |        Os.exit(-1)
          |        halt("")
          |    }
          |
          |    art.Art.run(Arch.ad, scheduler)
          |
          |    return 0
          |  }
          |}
          |
          |object Cli {
          |
          |  @datatype trait RunTopOption
          |
          |  @datatype class HelpOption extends RunTopOption
          |
          |  @enum object RunChoice {
          |    'Default
          |    'RoundRobin
          |    'Static
          |    'Legacy
          |  }
          |
          |  @datatype class RunOption(
          |                             val help: String,
          |                             val args: ISZ[String],
          |                             val scheduler: RunChoice.Type
          |                           ) extends RunTopOption
          |}
          |
          |import Cli._
          |
          |@record class Cli(val pathSep: C) {
          |  val help: String = ""
          |
          |  def parseRun(args: ISZ[String], i: Z): Option[RunTopOption] = {
          |    if (args.isEmpty) {
          |      return Some(Cli.RunOption(help = help, args = args, scheduler = RunChoice.Default))
          |    } else if (args.size == 2) {
          |      if (args(0) == "-s" || args(0) == "--scheduler") {
          |        val runChoice: RunChoice.Type = args(1) match {
          |          case "default" => RunChoice.Default
          |          case "roundRobin" => RunChoice.RoundRobin
          |          case "static" => RunChoice.Static
          |          case "legacy" => RunChoice.Legacy
          |          case x =>
          |            eprintln(s"Unknown scheduler: $${x}")
          |            Os.exit(1)
          |            halt("")
          |        }
          |        return Some(Cli.RunOption(help = "", args = args, scheduler = runChoice))
          |      } else {
          |        eprintln(s"Unknown option: $${args(0)}")
          |        Os.exit(1)
          |      }
          |      halt("")
          |    } else {
          |      halt("Invalid")
          |    }
          |  }
          |}
          |"""
    return ret
  }

  @pure def portType(name: String,
                     typ: String,
                     id: Z,
                     identifier: String,
                     mode: String,
                     urgency: Option[Z]): ST = {
    val artPortType: String = if (urgency.nonEmpty) "UrgentPort" else "Port"
    val _urgency: String = if (urgency.nonEmpty) s", urgency = ${urgency.get}" else ""
    return st"""val $name = ${artPortType}[$typ] (id = $id, name = "$identifier", mode = $mode${_urgency})"""
  }

  def genPort(port: Port): ST = {
    val id = port.portId
    val prefix: String = port.feature.category match {
      case FeatureCategory.EventPort => "Event"
      case FeatureCategory.EventDataPort => "Event"
      case FeatureCategory.DataPort => "Data"
      case _ => halt(s"Not handling ${port.feature.category}")
    }

    val dir: String = port.feature.direction match {
      case Direction.In => "In"
      case Direction.Out => "Out"
      case _ => "???"
    }

    val mode: String = s"${prefix}${dir}"

    return portType(
      port.name,
      port.getPortTypeNames.qualifiedReferencedTypeName,
      id,
      port.path,
      mode,
      port.urgency)
  }

  @pure def bridge(bridgeIdentifier: String,
                   instanceName: String,
                   typeName: String,
                   id: Z,
                   dispatchProtocol: ST,
                   dispatchTriggers: Option[ISZ[String]],
                   ports: ISZ[ST],
                   portArguments: ISZ[ST]
                  ): ST = {
    val _dispatchTriggers: ST =
      if (dispatchTriggers.isEmpty) st"None()"
      else st"Some(ISZ(${(dispatchTriggers.get.map((f: String) => s"${f}.id"), ", ")}))"

    val ret: ST =
      st"""val ${bridgeIdentifier} : ${typeName} = {
          |  ${(ports, "\n")}
          |
          |  ${typeName}(
          |    id = $id,
          |    name = "$instanceName",
          |    dispatchProtocol = $dispatchProtocol,
          |    dispatchTriggers = ${_dispatchTriggers},
          |
          |    ${(portArguments, ",\n")}
          |  )
          |}"""
    return ret
  }

  @pure def architectureDescription(packageName: String,
                                    imports: ISZ[String],
                                    architectureName: String,
                                    architectureDescriptionName: String,
                                    bridges: ISZ[ST],
                                    components: ISZ[String],
                                    connections: ISZ[ST],
                                    touchMethod: Option[ST]): ST = {
    val _imports = imports.map((m: String) => st"import ${m}")

    val touches: (Option[String], Option[ST]) =
      if(touchMethod.nonEmpty)
        (Some("TranspilerUtil.touch()"), Some(
          st"""
              |object TranspilerUtil {
              |  ${touchMethod.get}
              |}
              |"""
        ))
      else (None(), None())

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import art.PortMode._
          |import art.DispatchPropertyProtocol._
          |${(_imports, "\n")}
          |
          |${doNotEditComment(None())}
          |
          |object $architectureName {
          |  ${(bridges, "\n")}
          |
          |  val $architectureDescriptionName : ArchitectureDescription = {
          |    ${touches._1}
          |
          |    ArchitectureDescription(
          |      components = ISZ (${(components, ", ")}),
          |
          |      connections = ISZ (${(connections, ",\n")})
          |    )
          |  }
          |}
          |${touches._2}
          |"""
    return ret
  }
}
