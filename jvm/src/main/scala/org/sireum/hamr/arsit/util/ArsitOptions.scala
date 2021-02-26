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

@datatype class ArsitOptions(outputDir: Os.Path,
                             packageName: String,
                             embedArt: B,
                             bless: B,
                             verbose: B,
                             devicesAsThreads: B,
                             ipc: IpcMechanism.Type,
                             auxCodeDirs: ISZ[String],
                             outputSharedCDir: Option[Os.Path],
                             outputPlatformCDir: Option[Os.Path],
                             excludeImpl: B,
                             platform: ArsitPlatform.Type,
                             bitWidth: Z,
                             maxStringSize: Z,
                             maxArraySize: Z,
                             pathSeparator: C,
                             experimentalOptions: ISZ[String])
