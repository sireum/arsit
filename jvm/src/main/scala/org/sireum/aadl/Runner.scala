package org.sireum.aadl

import java.io.File
import org.sireum.aadl.ast.JSON
import scala.io.Source

object Runner {

  def main (args: Array[String]): Unit = {

    val fileName = System.getProperty("user.home") + "/aadl.json"
    val json = Source.fromFile(fileName).getLines.mkString

    val m = JSON.toAadlXml(org.sireum.String(json))

    val outDir = new File("/Users/belt/devel/sireum/slang-embedded/pca-pump-gen/src/main/scala/pca_pump_gen")

    ArtArchitectureGen.generator(outDir, m)

    ArtStubGenerator.generator(outDir, m)

  }
}
