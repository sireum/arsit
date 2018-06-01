package org.sireum.aadl.arsit

import org.sireum._

class ArtMinixGen {

  object Template {
    @pure def aep(packageName: String,
                  imports: ISZ[ST],
                  objectName: String,
                  portOpts: ISZ[(String, String, String)], // portOpt name, portOpt type, portOpt None value
                  portIds: ISZ[ST],
                  cases: ISZ[ST],
                  AEP_Id: String,
                  IPCPort_Id: String,
                  AEP_payload: String): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |${(imports, "\n")}
                 |
                 |object ${objectName} extends App {
                 |
                 |  var state: AEPState.Type = AEPState.Start
                 |  ${ var s = ""
                       for(po <- portOpts)
                         s += s"var ${po._1} : ${po._2} = ${po._3}\n"
                       s
                    }
                 |
                 |  def main(args: ISZ[String]): Z = {
                 |
                 |    val anyPortOpt: Option[art.Art.PortId] = None()
                 |    ${(portIds, "\n")}
                 |
                 |    Platform.receive(Some(IPCPorts.Main))
                 |
                 |    while (true) {
                 |      val (port, d) = Platform.receive(anyPortOpt)
                 |      port match {
                 |        ${(cases, "\n")}
                 |        case IPCPorts.${AEP_Id} =>
                 |          requested()
                 |        case _ => halt("Infeasible")
                 |      }
                 |    }
                 |
                 |    return 0
                 |  }
                 |
                 |  def eventArrived(): Unit = {
                 |    if (state == AEPState.EventRequested) {
                 |      sendEvent()
                 |    } else {
                 |      state = AEPState.EventArrived
                 |    }
                 |  }
                 |
                 |  def requested(): Unit = {
                 |    if (state == AEPState.EventArrived) {
                 |      sendEvent()
                 |    } else {
                 |      state = AEPState.EventRequested
                 |    }
                 |  }
                 |
                 |  def sendEvent(): Unit = {
                 |    Platform.send(${IPCPort_Id}, ${AEP_payload})
                 |
                 |    ${ var s = ""
                         for (po <- portOpts)
                           s += s"${po._1} = ${po._3}\n"
                         s
                      }
                 |
                 |    state = AEPState.Start
                 |  }
                 |}"""

    }
  }
}
