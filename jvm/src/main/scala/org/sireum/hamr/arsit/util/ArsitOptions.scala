// #Sireum

package org.sireum.hamr.arsit.util

import org.sireum._

@enum object IpcMechanism {
  'SharedMemory
}

@enum object ArsitPlatform {
  'JVM
  'Linux
  'Cygwin
  'MacOS
  'SeL4
}

@datatype class ArsitOptions(outputDir: String,
                             packageName: String,
                             embedArt: B,
                             bless: B,
                             verbose: B,
                             devicesAsThreads: B,
                             ipc: IpcMechanism.Type,
                             auxCodeDir: ISZ[String],
                             outputCDir: Option[String],
                             excludeImpl: B,
                             platform: ArsitPlatform.Type,
                             bitWidth: Z,
                             maxStringSize: Z,
                             maxArraySize: Z,
                             pathSeparator: C,
                             experimentalOptions: ISZ[String])
