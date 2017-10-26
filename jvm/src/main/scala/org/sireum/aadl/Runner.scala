package org.sireum.aadl

import org.sireum.ST
import java.io.File

object Runner {

  def main (args: Array[String]): Unit = {
    import scala.io.Source
    import org.sireum.aadl.ast.JSON
    import org.sireum.{String => SireumString}

    val fileName = System.getProperty("user.home") + "/aadl.json"
    val json = Source.fromFile(fileName).getLines.mkString

    val m = JSON.toAadlXml(SireumString(json))

    val outDir = new File("/Users/belt/devel/sireum/slang-embedded/pca-pump-gen/src/main/scala/pca_pump_gen")

    ArtArchitectureGen.generator(outDir, m)

    ArtStubGenerator.generator(outDir, m)

  }
}
