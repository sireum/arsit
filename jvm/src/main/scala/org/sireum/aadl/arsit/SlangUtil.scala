// #Sireum

package org.sireum.aadl.arsit

import org.sireum._

@enum object IPCMechanism {
  'MessageQueue
  'SharedMemory
}

object Cli {

  @datatype trait ArsitTopOption

  @datatype class HelpOption extends ArsitTopOption

  @enum object Ipcmech {
    'MessageQueue
    'SharedMemory
  }

  @datatype class ArsitOption(
                               help: String,
                               args: ISZ[String],
                               json: B,
                               outputDir: Option[String],
                               packageName: Option[String],
                               noart: B,
                               bless: B,
                               genTrans: B,
                               ipc: Ipcmech.Type
                             ) extends ArsitTopOption
}

@ext object Library {
  def getFiles: ISZ[(String, String)] = $
}