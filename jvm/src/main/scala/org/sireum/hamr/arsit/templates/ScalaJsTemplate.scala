// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._

object ScalaJsTemplate {

  def scalaJsScript(mainMethod: String): ST = {
    val ret: ST =
      st"""::#! 2> /dev/null                                   #
          |@ 2>/dev/null # 2>nul & echo off & goto BOF         #
          |if [ -z $${SIREUM_HOME} ]; then                      #
          |  echo "Please set SIREUM_HOME env var"             #
          |  exit -1                                           #
          |fi                                                  #
          |exec $${SIREUM_HOME}/bin/sireum slang run "$$0" "$$@"  #
          |:BOF
          |setlocal
          |if not defined SIREUM_HOME (
          |  echo Please set SIREUM_HOME env var
          |  exit /B -1
          |)
          |%SIREUM_HOME%\\bin\\sireum.bat slang run "%0" %*
          |exit /B %errorlevel%
          |::!#
          |// #Sireum
          |
          |import org.sireum._
          |import org.sireum.project.{JSON, Project, Target}
          |
          |@enum object OptType {
          |  "fullOpt"
          |  "fastOpt"
          |}
          |
          |val optType = OptType.fastOpt
          |val mainMethod = "${mainMethod}"
          |val runViaNodeJs = F // fails if code uses scalajs-dom, open webpage instead
          |
          |val home = Os.slashDir.up
          |val jsOutputDir = home / "js"
          |
          |
          |val sireum = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if(Os.isWin) "sireum.bat" else "sireum")
          |
          |
          |println("Tipe checking ...")
          |proc"$${sireum.string} proyek tipe --par $${home.string}".at(home).console.runCheck()
          |
          |println("Proyek JS compiling project ...")
          |proc"$${sireum.string} proyek compile --par --js $${home.string}".at(home).console.runCheck()
          |
          |val jsDir = home / "out" / s"$${home.name}-js"
          |val proyek_json = jsDir / "proyek.json"
          |val versions_json: Os.Path = jsDir / "versions.json"
          |
          |assert(proyek_json.exists, s"$${proyek_json} not found")
          |assert(versions_json.exists, s"$${versions_json} not found")
          |
          |val project: Project = JSON.toProject(proyek_json.read) match {
          |  case Either.Left(u) => u
          |  case Either.Right(e) => halt(e.string)
          |}
          |
          |val parser = Json.Parser.create(versions_json.read)
          |val versions: HashSMap[String, String] = parser.parseHashSMap(parser.parseString _, parser.parseString _)
          |
          |val scalaVer = versions.get("org.scala-lang:scala-library:").get
          |
          |def fetch(s: String): CoursierFileInfo = {
          |  Coursier.setScalaVersion(scalaVer)
          |  return org.sireum.Coursier.fetch(ISZ(s))(0)
          |}
          |
          |val runtimeLibraryVer = versions.get("org.sireum.kekinian::library-shared:").get
          |val scalaJsDomVer = versions.get("org.scala-js::scalajs-dom::").get
          |
          |val libSharedJsLib = fetch(s"org.sireum.kekinian::library-shared_sjs1:$${runtimeLibraryVer}").path
          |val macroJsLib = fetch(s"org.sireum.kekinian::macros_sjs1:$${runtimeLibraryVer}").path
          |val scalaJsDomJsLib = fetch(s"org.scala-js::scalajs-dom_sjs1:$${scalaJsDomVer}").path
          |
          |def rel(p: Os.Path): String = {
          |  return home.relativize(p).value
          |}
          |
          |var cps: ISZ[String] = ISZ()
          |for(m <- project.modules.values){
          |  if(ops.ISZOps(m.targets).contains(Target.Js)){
          |    val classes = jsDir / "modules" / m.id / "classes"
          |    if(!classes.exists) {
          |      halt(s"classes directory not found for JS module $${m.id}")
          |    }
          |    cps = cps :+ rel(classes.canon)
          |  }
          |}
          |
          |val libsDir= home / "lib"
          |libsDir.mkdir()
          |
          |val scalaJsStandaloneVer = "1.7.1"
          |val mScalaVer = ops.StringOps(scalaVer).substring(0, scalaVer.size - 2)
          |val sVer = s"$${mScalaVer}-$${scalaJsStandaloneVer}"
          |
          |val scalajsAssembly = libsDir / s"scalajs_$${sVer}" / "lib" / s"scalajs-cli-assembly_$${sVer}.jar"
          |val scalaJsLibrary = libsDir / s"scalajs_$${sVer}" / "lib" / s"scalajs-library_$${sVer}.jar"
          |// scalaJs lib alternatively available via maven, but cli assembly isn't
          |//val scalaJsLibrary = fetch(s"org.scala-js::scalajs-library:$${scalaJsStandaloneVer}").path
          |
          |if(!scalajsAssembly.exists) {
          |  // see https://www.scala-js.org/doc/internals/downloads.html
          |  println("Downloading scalaJS standalone ...")
          |  val zip = libsDir / s"scalajs_$${sVer}.zip"
          |  zip.downloadFrom(s"https://www.scala-js.org/files/$${zip.name}")
          |  zip.unzipTo(libsDir)
          |}
          |
          |assert(scalajsAssembly.exists && scalaJsLibrary.exists, s"Fetching scalaJS standalone failed")
          |
          |val scalaExe = sireum.up / "scala" / "bin" / (if(Os.isWin) "scala.bat" else "scala")
          |
          |val scalaArgs = ISZ("-classpath", rel(scalajsAssembly), "org.scalajs.cli.Scalajsld",
          |  "--stdlib", rel(scalaJsLibrary), s"--$$optType", "--mainMethod", mainMethod, "--outputDir", rel(jsOutputDir),
          |  scalaJsDomJsLib.value, macroJsLib.value, libSharedJsLib.value) ++ cps
          |
          |val sargs = st"$${(scalaArgs, " ")}".render
          |
          |jsOutputDir.mkdir()
          |
          |println(s"Running scalaJS linker using $${optType} ...")
          |proc"$${scalaExe.value} $$sargs".at(home.canon).console.runCheck()
          |
          |val mainJs = jsOutputDir / "main.js"
          |
          |assert(mainJs.exists, s"JS file not generated: $${mainJs.string}")
          |println(s"Generated: $${mainJs.string}. Size is $${mainJs.size} bytes")
          |
          |if(runViaNodeJs) {
          |  println("Running app using nodejs ...")
          |
          |  proc"node $${mainJs.string}".console.runCheck()
          |}
          |"""
    return ret
  }
}
