// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.util.ArsitLibrary

object ProjectTemplate {

  def arsitSlangInstructionsMessage(path: String): ST = {
    val ret: ST =
      st"""Slang Instructions:
          |-------------------
          |  Slang-Embedded Project Directory: ${path}
          |
          |    Refer to the comments in bin/project.cmd for instructions on how to open the
          |    project in Sireum IVE and how to run the system/unit-tests from IVE or the command line.
          |    Alternatively, refer to the comments in build.sbt or build.sc for sbt or mill
          |    based instructions."""
    return ret
  }

  def arsitCInstructionsMessage(cmakeProjDirectory: String,
                                devDir: String,
                                transpileScript: String,
                                compileScript: String,
                                runScript: String,
                                stopScript: String): ST = {
    var lng = st""
    lng = st"${lng}The cmake directory will be created by the transpiler. "
    lng = st"${lng}The transpiler only needs to be rerun when changes are made to Slang code. "

    val ret: ST =
      st"""C Instructions:
          |---------------
          |  Cmake Project Directory:  ${cmakeProjDirectory}
          |  Developer Code Directory: ${devDir}
          |
          |  $lng
          |
          |  Execute the following scripts to build/run the system. Pass '-h' to see a script's options.
          |
          |    ${transpileScript}
          |    ${compileScript}
          |    ${runScript}
          |    ${stopScript}"""

    return ret
  }

  def arsitCAmkESInstructionsMessage(devDir: String,
                                     transpileScript: String): ST = {
    val ret: ST =
      st"""C Instructions for CAmkES:
          |--------------------------
          |  Developer Code Directory: ${devDir}
          |
          |  Execute the following script to transpile the Slang code for CAmkES (transpiler only needs
          |  to be rerun when changes are made to Slang code):
          |
          |    ${transpileScript}"""

    return ret
  }

  def proyekBuild(projectName: String,
                  basePackageName: String,
                  processes: ISZ[ISZ[String]],
                  embedArt: B,
                  demoScalaPath: String,
                  bridgeTestPath: String): ST = {
    val (artDir, artIvy): (Option[ST], Option[ST]) =
      if(embedArt)
        (Some(st""""art", """), None())
      else
        (None(), Some(st""""org.sireum.slang-embedded-art::slang-embedded-art:", """))

    // removed from ivy deps the following from
    //, "com.intellij:forms_rt:"

    val _processes = processes.map((m: ISZ[String]) => {
      val quote = m.map((n: String) => st""""${n}"""")
      st"""ISZ(${(quote, ",")})"""
    })

    val ret = st"""::#! 2> /dev/null                                   #
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
                  |// Example Sireum Proyek build definitions -- the contents of this file will not be overwritten
                  |//
                  |// To install Sireum (Proyek and IVE) see https://github.com/sireum/kekinian#installing
                  |//
                  |// The following commands should be executed in the parent of the 'bin' directory.
                  |//
                  |// Command Line:
                  |//   To run the demo from the command line using the default scheduler:
                  |//     sireum proyek run . ${basePackageName}.Demo
                  |//
                  |//   To see the available CLI options:
                  |//     sireum proyek run . ${basePackageName}.Demo -h
                  |//
                  |//   To run the example unit tests from the command line:
                  |//     sireum proyek test .
                  |//
                  |//   To build an executable jar:
                  |//     sireum proyek assemble --uber --main ${basePackageName}.Demo .
                  |//
                  |// Sireum IVE:
                  |//
                  |//   If you prevented HAMR from running Proyek IVE then first generate the IVE project:
                  |//     sireum proyek ive .
                  |//
                  |//   Then in IVE select 'File > Open ...' and navigate to the parent of the
                  |//   'bin' directory and click 'OK'.
                  |//
                  |//   To run the demo from within Sireum IVE:
                  |//     Right click ${demoScalaPath} and choose "Run 'Demo'"
                  |//
                  |//   To run the unit test cases from within Sireum IVE:
                  |//     Right click the ${bridgeTestPath} and choose "Run ScalaTests in bridge"
                  |//
                  |//   NOTE: A ClassNotFoundException may be raised the first time you try to
                  |//         run the demo or unit tests.  If this occurs simply delete the directory
                  |//         named 'target' and retry
                  |
                  |import org.sireum._
                  |import org.sireum.project.{JSON, Module, Project, ProjectUtil, Target}
                  |
                  |def usage(): Unit = {
                  |  println("Usage: [ json ]")
                  |}
                  |
                  |var isDot = T
                  |
                  |Os.cliArgs match {
                  |  case ISZ(string"json") => isDot = F
                  |  case ISZ(string"-h") =>
                  |    usage()
                  |    Os.exit(0)
                  |  case ISZ() =>
                  |  case _ =>
                  |    usage()
                  |    Os.exit(-1)
                  |}
                  |
                  |val homeDir: Os.Path = Os.slashDir.up.canon
                  |
                  |val jsTarget: ISZ[Target.Type] = ISZ(Target.Js)
                  |
                  |val jvmTarget: ISZ[Target.Type] = ISZ(Target.Jvm)
                  |val jvmLibrary: ISZ[String] = ISZ("org.sireum.kekinian::library:")
                  |
                  |val sharedTarget: ISZ[Target.Type] = Module.allTargets
                  |val sharedLibrary: ISZ[String] = ISZ("org.sireum.kekinian::library-shared:")
                  |
                  |val jsDeps: ISZ[String] = ISZ("org.scala-js::scalajs-dom_sjs1:")
                  |
                  |def module(id: String,
                  |           baseDir: Os.Path,
                  |           subPathOpt: Option[String],
                  |           deps: ISZ[String],
                  |           targets: ISZ[Target.Type],
                  |           ivyDeps: ISZ[String],
                  |           sources: ISZ[String],
                  |           testSources: ISZ[String]): Module = {
                  |  return Module(
                  |    id = id,
                  |    basePath = baseDir.string,
                  |    subPathOpt = subPathOpt,
                  |    deps = deps,
                  |    targets = targets,
                  |    ivyDeps = ivyDeps,
                  |    resources = ISZ(),
                  |    sources = sources,
                  |    testSources = testSources,
                  |    testResources = ISZ(),
                  |    publishInfoOpt = None())
                  |}
                  |
                  |val artShared: Module = module(
                  |  id = "art-shared",
                  |  baseDir = homeDir / "src" / "infrastructure" / "art",
                  |  subPathOpt = Some("shared"),
                  |  deps = ISZ(),
                  |  targets = sharedTarget,
                  |  ivyDeps = sharedLibrary,
                  |  sources = ISZ("src/main/scala"),
                  |  testSources = ISZ())
                  |
                  |val artJs: Module = module(
                  |  id = "art-js",
                  |  baseDir = homeDir / "src" / "infrastructure" / "art",
                  |  subPathOpt = Some("js"),
                  |  deps = ISZ(artShared.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = sharedLibrary ++ jsDeps,
                  |  sources = ISZ("src/main/scala"),
                  |  testSources = ISZ())
                  |
                  |val data: Module = module(
                  |  id = "data",
                  |  baseDir = homeDir / "src" / "common" / "data",
                  |  subPathOpt = None(),
                  |  deps = ISZ(artShared.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("main"),
                  |  testSources = ISZ())
                  |
                  |val library: Module = module(
                  |  id = "library",
                  |  baseDir = homeDir / "src" / "common" / "library",
                  |  subPathOpt = None(),
                  |  deps = ISZ(data.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("main"),
                  |  testSources = ISZ())
                  |
                  |val processes: ISZ[ISZ[String]] = ISZ(
                  |  ${(_processes, ",\n")}
                  |)
                  |
                  |var apis: ISZ[Module] = ISZ()
                  |var bridges: ISZ[Module] = ISZ()
                  |var sharedComponents: ISZ[Module] = ISZ()
                  |var jsComponents: ISZ[Module] = ISZ()
                  |var jvmComponents: ISZ[Module] = ISZ()
                  |
                  |for(p <- processes) {
                  |  val id = ops.ISZOps(p).foldLeft((a: String, b: String) => s"$${a}_$${b}", "")
                  |
                  |  val api = module(
                  |    id = s"apis$${id}",
                  |    baseDir = (homeDir / "src" / "infrastructure" / "apis") /+ p,
                  |    subPathOpt = None(),
                  |    deps = ISZ(data.id),
                  |    targets = sharedTarget,
                  |    ivyDeps = ISZ(),
                  |    sources = ISZ("main"),
                  |    testSources = ISZ())
                  |
                  |  val bridge: Module = module(
                  |    id = s"bridges$${id}",
                  |    baseDir = (homeDir / "src" / "infrastructure" / "bridges") /+ p,
                  |    subPathOpt = None(),
                  |    deps = ISZ(api.id),
                  |    targets = sharedTarget,
                  |    ivyDeps = ISZ(),
                  |    sources = ISZ("main"),
                  |    testSources = ISZ())
                  |
                  |  val componentShared: Module = module(
                  |    id = s"components$${id}_shared",
                  |    baseDir = (homeDir / "src" / "components") /+ p,
                  |    subPathOpt = Some("shared"),
                  |    deps = ISZ(api.id, library.id),
                  |    targets = sharedTarget,
                  |    ivyDeps = ISZ[String](),
                  |    sources = ISZ("main"),
                  |    testSources = ISZ())
                  |
                  |  val componentJs: Module = module(
                  |    id = s"components$${id}_js",
                  |    baseDir = (homeDir / "src" / "components") /+ p,
                  |    subPathOpt = Some("js"),
                  |    deps = ISZ(componentShared.id),
                  |    targets = jsTarget,
                  |    ivyDeps = ISZ[String]() ++ jsDeps,
                  |    sources = ISZ("main"),
                  |    testSources = ISZ())
                  |
                  |  val componentJvm: Module = module(
                  |    id = s"components$${id}_jvm",
                  |    baseDir = (homeDir / "src" / "components") /+ p,
                  |    subPathOpt = Some("jvm"),
                  |    deps = ISZ(componentShared.id),
                  |    targets = jvmTarget,
                  |    ivyDeps = ISZ[String](),
                  |    sources = ISZ("main"),
                  |    testSources = ISZ())
                  |
                  |  apis = apis :+ api
                  |  bridges = bridges :+ bridge
                  |  jsComponents = jsComponents :+ componentJs
                  |  jvmComponents = jvmComponents :+ componentJvm
                  |  sharedComponents = sharedComponents :+ componentShared
                  |}
                  |
                  |val architecture: Module = module(
                  |  id = "architecture",
                  |  baseDir = homeDir / "src" / "infrastructure" / "architecture",
                  |  subPathOpt = None(),
                  |  deps = bridges.map((m: Module) => m.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("main"),
                  |  testSources = ISZ())
                  |
                  |val schedulers: Module = module(
                  |  id = "schedulers",
                  |  baseDir = homeDir / "src" / "infrastructure" / "schedulers",
                  |  subPathOpt = None(),
                  |  deps = ISZ(architecture.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("main"),
                  |  testSources = ISZ())
                  |
                  |val appShared: Module = module(
                  |  id = "app",
                  |  baseDir = homeDir / "src" / "app",
                  |  subPathOpt = Some("shared"),
                  |  deps = sharedComponents.map((m: Module) => m.id),
                  |  targets = sharedTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("src/main/scala"),
                  |  testSources = ISZ())
                  |
                  |val appJvm: Module = module(
                  |  id = "appJvm",
                  |  baseDir = homeDir / "src" / "app",
                  |  subPathOpt = Some("jvm"),
                  |  deps = ISZ(appShared.id, schedulers.id) ++ jvmComponents.map((m: Module) => m.id),
                  |  targets = jvmTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("src/main/scala"),
                  |  testSources = ISZ())
                  |
                  |val appJs: Module = module(
                  |  id = "appJs",
                  |  baseDir = homeDir / "src" / "app",
                  |  subPathOpt = Some("js"),
                  |  deps = ISZ(appShared.id, artJs.id, schedulers.id) ++ jsComponents.map((m: Module) => m.id),
                  |  targets = jsTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ("src/main/scala"),
                  |  testSources = ISZ())
                  |
                  |val test: Module = module(
                  |  id = "test",
                  |  baseDir = homeDir / "src" / "test",
                  |  subPathOpt = None(),
                  |  deps = ISZ(appJvm.id),
                  |  targets = jvmTarget,
                  |  ivyDeps = ISZ(),
                  |  sources = ISZ(),
                  |  testSources = ISZ("bridge", "util"))
                  |
                  |var nixModules: ISZ[Module] = ISZ()
                  |
                  |val nixDir = homeDir / "src" / "infrastructure" / "nix"
                  |if(nixDir.exists) {
                  |  nixModules = nixModules :+ module(id = nixDir.name,
                  |                                    baseDir = nixDir,
                  |                                    subPathOpt = None(),
                  |                                    deps = ISZ(appJvm.id),
                  |                                    targets = jvmTarget,
                  |                                    ivyDeps = ISZ(),
                  |                                    sources = ISZ("main"),
                  |                                    testSources = ISZ())
                  |}
                  |
                  |val seL4NixDir = homeDir / "src" / "infrastructure" / "seL4Nix"
                  |if(seL4NixDir.exists) {
                  |  nixModules = nixModules :+ module(id = seL4NixDir.name,
                  |                                    baseDir = seL4NixDir,
                  |                                    subPathOpt = None(),
                  |                                    deps = (bridges ++ sharedComponents).map((m: Module) => m.id) :+ appShared.id,
                  |                                    targets = jvmTarget,
                  |                                    ivyDeps = ISZ(),
                  |                                    sources = ISZ("main"),
                  |                                    testSources = ISZ())
                  |}
                  |
                  |var mods = ISZ(artShared, artJs, architecture, data, library, schedulers, appShared, appJvm, appJs, test) ++
                  |  apis ++ bridges ++ sharedComponents ++ jsComponents ++ jvmComponents ++ nixModules
                  |
                  |var slangProject: Project = Project.empty
                  |for(m <- mods) {
                  |  slangProject = slangProject + m
                  |}
                  |
                  |val project: Project = slangProject
                  |
                  |if (isDot) {
                  |  val projectDot = homeDir / "project.dot"
                  |  projectDot.writeOver(ProjectUtil.toDot(project))
                  |  println(s"Wrote $$projectDot")
                  |} else {
                  |  println(JSON.fromProject(project, T))
                  |}
                  |"""
    return ret
  }

  def proyekVersionProperties(): ST = {
    val kekinianVersion = ArsitLibrary.getKekinianVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()
    val formsRtVersion = ArsitLibrary.getFormsRtVersion()
    val inspectorVersion = ArsitLibrary.getInspectorVersion()
    val artVersion = ArsitLibrary.getArtVersion()
    val scalaJsCompilerVersion = ArsitLibrary.getScalaJsCompilerVersion()
    val scalaJsDomVersion = ArsitLibrary.getScalaJsDomVersion()

    // remove the following from version.properties
    // |com.intellij%forms_rt%=${formsRtVersion}
    val ret:ST = st"""org.sireum.slang-embedded-art%%slang-embedded-art%=${artVersion}
                     |
                     |org.sireum%inspector-capabilities%=${inspectorVersion}
                     |org.sireum%inspector-gui%=${inspectorVersion}
                     |org.sireum%inspector-services-jvm%=${inspectorVersion}
                     |
                     |# scalajs-compiler need to match the stand-alone version
                     |org.scala-js%%%scalajs-compiler%=${scalaJsCompilerVersion}
                     |org.scala-js%%scalajs-dom%%=${scalaJsDomVersion}
                     |org.scala-js%%scalajs-dom_sjs1%=${scalaJsDomVersion}
                     |
                     |# remove the following entries if you want to use the versions
                     |# that ship with sireum (i.e. $$SIREUM_HOME/bin/sireum --version)
                     |
                     |# Scala compiler plugin for Slang
                     |org.sireum%%scalac-plugin%=${sireumScalacVersion}
                     |
                     |org.sireum.kekinian%%library%=${kekinianVersion}
                     |org.sireum.kekinian%%library-shared%=${kekinianVersion}
                     |org.sireum.kekinian%%macros%=${kekinianVersion}
                     |
                     |org.scala-lang%scala-library%=${scalaVersion}
                     |org.scalatest%%scalatest%%=${scalaTestVersion}
                     |"""
    return ret
  }

  def sbtBuild(projectName: String,
               basePackageName: String,
               embedArt: B,
               demoScalaPath: String,
               bridgeTestPath: String): ST = {
    val artVersion = ArsitLibrary.getArtVersion()

    val (artVer, artJitpack, artSrcDir): (Option[ST], Option[ST], Option[ST]) = if (!embedArt) {
      (Some(
        st"""// https://github.com/sireum/slang-embedded-art/tree/${artVersion}
            |val artVersion = "${artVersion}""""),
        Some(st""""org.sireum.slang-embedded-art" %% "slang-embedded-art" % artVersion withSources() withJavadoc(),"""),
        None())
    } else {
      (None(), None(), Some(st"""Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/art","""))
    }

    val ret: ST =
      st"""// Example sbt build definitions -- the contents of this file will not be overwritten
          |//
          |// sbt can be obtained from https://www.scala-sbt.org/download.html
          |//
          |// To run the demo from the command line using the default scheduler:
          |//   sbt run
          |//
          |// To see the available CLI options:
          |//   sbt "run -h"
          |//
          |// To run the example unit tests from the command line:
          |//   sbt test
          |//
          |// To build a runnable/executable jar:
          |//   sbt assembly
          |//
          |// To skip running the unit tests while building the executable jar:
          |//   sbt 'set test in assembly := {}' assembly
          |// on Linux/Mac, or
          |//   sbt "set test in assembly := {}" assembly
          |// on Windows
          |//
          |// Sireum IVE:
          |//   In IVE select 'File > Open ...' and navigate to the directory containing
          |//   this file then click 'OK'.  To install Sireum IVE see https://github.com/sireum/kekinian#installing
          |//
          |//   To run the demo from within Sireum IVE:
          |//     Right click ${demoScalaPath} and choose "Run 'Demo'"
          |//
          |//   To run the unit test cases from within Sireum IVE:
          |//     Right click the ${bridgeTestPath} directory and choose "Run ScalaTests in bridge"
          |//
          |//   NOTE: A ClassNotFoundException may be raised the first time you try to
          |//         run the demo or unit tests.  If this occurs simply delete the directory
          |//         named 'target' and retry
          |
          |
          |lazy val ${projectName} = slangEmbeddedProject("${projectName}", ".")
          |
          |${libDependencies()}
          |
          |${artVer}
          |
          |val commonSettings = Seq(
          |  organization := "org.sireum",
          |  incOptions := incOptions.value.withLogRecompileOnMacro(false),
          |  scalaVersion := scalaVer,
          |  scalacOptions := Seq("-target:jvm-1.8", "-deprecation",
          |    "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings"),
          |  Test / parallelExecution := true,
          |  resolvers ++= Seq(Resolver.sonatypeRepo("public"), "jitpack" at "https://jitpack.io"),
          |  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion),
          |  libraryDependencies ++= Seq(
          |    ${artJitpack}
          |    "org.sireum.kekinian" %% "library" % kekinianVersion withSources() withJavadoc()
          |  )
          |)
          |
          |import sbtassembly.AssemblyPlugin.defaultUniversalScript
          |val slangEmbeddedSettings = Seq(
          |  ${artSrcDir}
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/bridge",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/component",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/data",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/nix",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/seL4Nix",
          |
          |  Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
          |  Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/util",
          |
          |  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
          |
          |  // Jetbrains UI Designer
          |  libraryDependencies += "com.intellij" % "forms_rt" % formsRtVersion,
          |
          |  mainClass in (Compile, run) := Some("${basePackageName}.Demo"),
          |
          |  mainClass in assembly := Some("${basePackageName}.Demo"),
          |  assemblyJarName in assembly := "${projectName}.jar",
          |  assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false))),
          |
          |  assemblyMergeStrategy in assembly := {
          |    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
          |    case x => MergeStrategy.first
          |  }
          |)
          |
          |val slangEmbeddedInspectorSettings = Seq(
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/inspector",
          |
          |  libraryDependencies += "org.sireum" % "inspector-capabilities" % inspectorVersion withSources(),
          |  libraryDependencies += "org.sireum" % "inspector-gui" % inspectorVersion withSources(),
          |  libraryDependencies += "org.sireum" % "inspector-services-jvm" % inspectorVersion withSources(),
          |
          |  mainClass in (Compile, run) := Some("${basePackageName}.InspectorDemo"),
          |)
          |
          |def slangEmbeddedProject(projId: String, projectDirectory: String) =
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings)
          |
          |def slangEmbeddedInspectorProject(projId: String, projectDirectory: String) = {
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings ++ slangEmbeddedInspectorSettings)
          |}
          |"""
    return ret
  }

  def sbtBuildPropertiesContents(): ST = {
    val sbtVersion: String = ArsitLibrary.getSBTVersion()
    val ret: ST =
      st"""sbt.version=${sbtVersion}
          |"""
    return ret
  }

  def sbtPluginsSbtContents(): ST = {
    val sbtAssemblyVersion = ArsitLibrary.getSbtAssemblyVersion()
    val ret: ST =
      st"""addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "${sbtAssemblyVersion}")
          |"""
    return ret
  }

  def millBuild(basePackageName: String,
                outputDirSimpleName: String,
                embedArt: B): ST = {
    val inspectorVersion = ArsitLibrary.getInspectorVersion()

    val artVersion = ArsitLibrary.getArtVersion()

    val (artVer, artJitpack, artSrcDir): (Option[ST], Option[ST], Option[ST]) = if (!embedArt) {
      (Some(
        st"""// https://github.com/sireum/slang-embedded-art/tree/${artVersion}
            |val artVersion = "${artVersion}""""),
        Some(st"""ivy"org.sireum.slang-embedded-art::slang-embedded-art::$${artVersion}","""),
        None())
    } else {
      (None(), None(), Some(st"""millSourcePath / os.up / "src" / "main" / "art","""))
    }

    val ret: ST =
      st"""import mill._
          |import scalalib._
          |import ammonite.ops._
          |
          |// Example mill build -- the contents of this file will not be overwritten.
          |//
          |// mill can be obtained following instructions at https://github.com/sireum/kekinian#slang-app-example-mill-project
          |//
          |// To run the demo from the command line:
          |//   $$SIREUM_HOME/bin/mill ${basePackageName}.run
          |//
          |// To run the example unit tests:
          |//   $$SIREUM_HOME/bin/mill ${basePackageName}.tests
          |//
          |// Sireum IVE:
          |//   First cd to the directory containing this file and execute the following:
          |//
          |//     $$SIREUM_HOME/bin/sireum tools ivegen -f -m mill -n ${outputDirSimpleName} ../
          |//
          |//   Then in IVE select 'File > Open ...' and navigate to the directory
          |//   containing this file then click 'OK'.  To have the codebase and its
          |//   test suites recompiled upon changes, run:
          |//
          |//     $$SIREUM_HOME/bin/mill -w ${basePackageName}.tests.compile
          |//
          |// Visual Studio Code:
          |//   Follow Sireum Kekinian's instructions for setting up a development
          |//   environment using Scala Metals: https://github.com/sireum/kekinian#scala-metals
          |//   Then open the folder containing this file in VS Code and import the
          |//   mill build when asked.
          |
          |
          |object `${basePackageName}` extends slangEmbeddedProject
          |
          |trait SlangEmbeddedModule extends ScalaModule {
          |
          |  ${libDependencies()}
          |  ${artVer}
          |
          |  def scalaVersion = scalaVer
          |
          |  override def javacOptions = T { Seq("-source", "1.8", "-target", "1.8", "-encoding", "utf8") }
          |
          |  override def scalacOptions = T { Seq(
          |    "-target:jvm-1.8",
          |    "-deprecation",
          |    "-Yrangepos",
          |    "-Ydelambdafy:method",
          |    "-feature",
          |    "-unchecked",
          |    "-Xfatal-warnings",
          |    "-language:postfixOps"
          |  ) }
          |
          |  override def ivyDeps = Agg(
          |    ${artJitpack}
          |    ivy"org.sireum.kekinian::library::$${kekinianVersion}",
          |
          |    // Jetbrains UI Designer
          |    ivy"com.intellij:forms_rt:$${formsRtVersion}"
          |  )
          |
          |  override def scalacPluginIvyDeps = Agg(ivy"org.sireum::scalac-plugin::$${sireumScalacVersion}")
          |
          |  override def repositories = super.repositories :+ coursier.Repositories.jitpack
          |
          |  override def mainClass = T { Some("${basePackageName}.Demo") }
          |
          |  implicit def osPath2PathRef(p: os.Path): PathRef = PathRef(p)
          |}
          |
          |trait slangEmbeddedProject extends SlangEmbeddedModule {
          |
          |  def contributedSources: Seq[PathRef] = Seq(
          |    millSourcePath / os.up / "src" / "main" / "architecture",
          |    ${artSrcDir}
          |    millSourcePath / os.up / "src" / "main" / "bridge",
          |    millSourcePath / os.up / "src" / "main" / "component",
          |    millSourcePath / os.up / "src" / "main" / "data",
          |    millSourcePath / os.up / "src" / "main" / "nix",
          |    millSourcePath / os.up / "src" / "main" / "seL4Nix"
          |  )
          |
          |  override def sources = T.sources(contributedSources)
          |
          |  object tests extends Tests {
          |
          |    final override def millSourcePath = super.millSourcePath / os.up / os.up / "src" / "test"
          |
          |    override def sources = T.sources( millSourcePath / "bridge",
          |                                      millSourcePath / "util" )
          |
          |    override def ivyDeps = Agg(ivy"org.scalatest::scalatest::$${scalaTestVersion}")
          |
          |    override def testFrameworks = T { Seq("org.scalatest.tools.Framework") }
          |  }
          |}
          |
          |trait slangEmbeddedInspectorProject extends slangEmbeddedProject {
          |
          |  override def mainClass = T { Some("${basePackageName}.InspectorDemo") }
          |
          |  override def contributedSources =
          |    super.contributedSources :+ millSourcePath / os.up / "src" / "main" / "inspector"
          |
          |  // FIXME: 2021.01.04 - the following doesn't work due to javafx/mill resolution issue
          |  //        -- refer to https://github.com/lihaoyi/mill/issues/767
          |  // override def ivyDeps = Agg(
          |  //   ivy"org.sireum::inspector-capabilities::$${inspectorVersion}",
          |  //   ivy"org.sireum::inspector-gui::$${inspectorVersion}",
          |  //   ivy"org.sireum::inspector-services-jvm::$${inspectorVersion}"
          |
          |  // workaround to #767 -- refer to https://github.com/lihaoyi/mill/issues/767#issuecomment-652799588
          |  override def unmanagedClasspath = T {
          |    import coursier._
          |
          |    val files = Fetch().addDependencies(
          |      dep"org.sireum:inspector-capabilities:${inspectorVersion}",
          |      dep"org.sireum:inspector-gui:${inspectorVersion}",
          |      dep"org.sireum:inspector-services-jvm:${inspectorVersion}"
          |    ).addRepositories(
          |      Repositories.sonatype("releases"),
          |      Repositories.jitpack
          |    ).run()
          |    val pathRefs = files.map(f => PathRef(Path(f)))
          |    Agg(pathRefs : _*)
          |  }
          |}
          |"""

    return ret
  }

  def libDependencies(): ST = {
    val kekinianVersion = ArsitLibrary.getKekinianVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()
    val formsRtVersion = ArsitLibrary.getFormsRtVersion()
    val inspectorVersion = ArsitLibrary.getInspectorVersion()

    val ret: ST =
      st"""// refer to https://github.com/sireum/kekinian/blob/master/versions.properties
          |// to get the most recent versions of the following dependencies
          |
          |// versions.properties key: org.scala-lang%scala-library%
          |val scalaVer = "${scalaVersion}"
          |
          |// versions.properties key: org.scalatest%%scalatest%%
          |val scalaTestVersion = "${scalaTestVersion}"
          |
          |// versions.properties key: org.sireum%%scalac-plugin%
          |// https://github.com/sireum/scalac-plugin/tree/${sireumScalacVersion}
          |val sireumScalacVersion = "${sireumScalacVersion}"
          |
          |
          |// refer to https://github.com/sireum/kekinian/releases to get the latest
          |// Sireum Kekinian release: https://github.com/sireum/kekinian/tree/${kekinianVersion}
          |val kekinianVersion = "${kekinianVersion}"
          |
          |
          |val inspectorVersion = "${inspectorVersion}"
          |
          |val formsRtVersion = "${formsRtVersion}"
          |"""
    return ret
  }
}
