// #Sireum

package org.sireum.hamr.arsit.test.util

import org.sireum._

@enum object ArsitTestMode {
  "ProyekCompile"

  "ProyekTest"

  "ProyekRun"

  "ProyekTipe"

  "LinuxCompile"

  // everything in Base + compiles the generated sbt project
  "SbtCompile"

  // everything in SbtCompile + runs demo
  "SbtRun"

  // everything in SbtCompile + runs unit tests
  "SbtTest"
}
