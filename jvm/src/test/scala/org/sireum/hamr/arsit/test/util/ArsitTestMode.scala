// #Sireum

package org.sireum.hamr.arsit.test.util

import org.sireum._

@enum object ArsitTestMode {
  // structural comparison b/w expected and results map
  'Base

  // everything in Base + compiles the generated sbt project
  'SbtCompile

  // everything in SbtCompile + runs demo
  'SbtRun

  // everything in SbtCompile + runs unit tests
  'SbtTest
}
