package org.sireum.hamr.arsit.test

import org.sireum._
import org.sireum.test.TestSuite

class ExampleTests extends TestSuite {

  val test = Tests {
    * - { println("Hello World - 1") }

    * - { println("Hello World - 2") }
  }
}
