// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.containers
import org.sireum.hamr.codegen.common.containers.Resource

object ToolsTemplate {
  val header: ST =
    st"""::/*#! 2> /dev/null                                   #
      |@ 2>/dev/null # 2>nul & echo off & goto BOF           #
      |if [ -z $${SIREUM_HOME} ]; then                        #
      |  echo "Please set SIREUM_HOME env var"               #
      |  exit -1                                             #
      |fi                                                    #
      |exec $${SIREUM_HOME}/bin/sireum slang run "$$0" "$$@"    #
      |:BOF
      |setlocal
      |if not defined SIREUM_HOME (
      |  echo Please set SIREUM_HOME env var
      |  exit /B -1
      |)
      |%SIREUM_HOME%\\bin\\sireum.bat slang run "%0" %*
      |exit /B %errorlevel%
      |::!#*/
      |// #Sireum
      |
      |import org.sireum._
      |
      |val sireum = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")"""

  def toISString(rootDir: Os.Path, resources: ISZ[Resource]): ST = {
    val relResources: ISZ[String] = for(r <- resources) yield s"\"${rootDir.relativize(Os.path(r.dstPath))}\""
    val r: ST =
      st"""val files: ISZ[String] = ISZ(${(relResources, ",\n")})
          |
          |val toolargs: String = st"$${(files, " ")}".render"""
    return r
  }

  def slangCheck(resources: ISZ[containers.Resource], basePackage: String, outputdir: String, slangBinDir: String): ST = {
    val slangDir = Os.path(slangBinDir)
    val outDir = slangDir.relativize(Os.path(outputdir))

    val ret: ST =
      st"""$header
          |
          |// create SlangCheck generators for the types used in the project
          |
          |val slangCheckJar: Os.Path = {
          |  Os.env("SLANG_CHECK_JAR") match {
          |    case Some(p) =>
          |      val cand = Os.path(p)
          |      if (!cand.exists) {
          |        halt(s"SLANG_CHECK_JAR is not a file: $$p")
          |      } else {
          |        cand
          |      }
          |    case _ => halt(s"SLANG_CHECK_JAR is not defined")
          |  }
          |}
          |
          |${toISString(slangDir, resources)}
          |
          |proc"java -jar $$slangCheckJar tools slangcheck -p $basePackage -o $outDir $$toolargs".at(Os.slashDir).console.runCheck()
          |"""
    return ret

  }

  def genSerGen(basePackage: String, slangBinDir: String, resources: ISZ[Resource]): ST = {
    return (
      st"""$header
          |
          |// create serializers/deserializers for the Slang types used in the project
          |
          |${toISString(Os.path(slangBinDir), resources)}
          |
          |proc"$$sireum tools sergen -p ${basePackage} -m json,msgpack -o $${Os.slashDir.up}/src/main/data/${basePackage} $$toolargs".at(Os.slashDir).console.runCheck()
          |""")
  }
}
