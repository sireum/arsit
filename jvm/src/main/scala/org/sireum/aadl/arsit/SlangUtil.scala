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

@enum object DispatchProtocol {
  'Periodic
  'Sporadic
}



@datatype class AadlTypes (typeMap : Map[String, AadlType])

@sig trait AadlType {
  def name: String
  def slangTypeName: String
  def container: org.sireum.aadl.ir.Component
}

@datatype class EnumType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val slangTypeName: String,
                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val container: org.sireum.aadl.ir.Component,
                          val slangTypeName: String) extends AadlType

@datatype class RecordType(val name: String,
                           val container: org.sireum.aadl.ir.Component,
                           val slangTypeName: String,
                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val slangTypeName: String
                         ) extends AadlType

@datatype class TODOType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val slangTypeName: String) extends AadlType