package org.sireum.hamr.arsit.test

import org.sireum._
import org.sireum.$internal.RC
import org.sireum.hamr.arsit.test.util.ArsitTestMode
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.arsit.{Arsit, ArsitResult}
import org.sireum.hamr.codegen.common.util.test.{ETestResource, ITestResource, TestJSON, TestOs, TestResource, TestResult, TestUtil}
import org.sireum.hamr.ir.{Aadl, JSON}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.hamr.arsit.test.ArsitTest._
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, CodeGenIpcMechanism, CodeGenPlatform, ExperimentalOptions, ModelUtil}
import org.sireum.ops.ISZOps

trait ArsitTest extends TestSuite {

  def generateExpected: B = F

  def testModes: ISZ[ArsitTestMode.Type] = Os.env("ArsitTestModes") match {
    case Some(list) => ops.StringOps(list).split((c: C) => c == C(',')).map((m: String) => ArsitTestMode.byName(m).get)
    case _ =>
      // Run Tipe (parser and type-checker) only on results
      ISZ(ArsitTestMode.ProyekTipe, ArsitTestMode.ProyekTest)
      //ISZ(ArsitTestMode.LinuxCompile)
      // ISZ(ArsitTestMode.ProyekTipe, ArsitTestMode.ProyekCompile, ArsitTestMode.ProyekRun)
    //ISZ(ArsitTestMode.Tipe, ArsitTestMode.ProyekCompile, ArsitTestMode.ProyekTest, ArsitTestMode.ProyekRun, ArsitTestMode.LinuxCompile)
  }

  def timeoutInSeconds: Z = 30

  def ignoreBuildDefChanges: B = F // temporarily ignore build.sbt and build.sc changes due to build.properties updates

  val (expectedJsonDir, baseModelsDir) = getDirectories()

  def filter: B = if(ArsitTest.filterTestsSet().nonEmpty) filterTestsSet().get else F
  def filters: ISZ[String] = ISZ("prefix_of_testname_to_filter")

  def ignores: ISZ[String] = ISZ("prefix_of_testname_to_ignore")

  def test(testName: String, airFile: Os.Path, ops: ArsitOptions)(implicit position: org.scalactic.source.Position) : Unit = {
    test(testName, airFile, ops, None())
  }

  def test(testName: String, airFile: Os.Path, ops: ArsitOptions, resultDir:Option[String])(implicit position: org.scalactic.source.Position) : Unit = {
    var tags: ISZ[org.scalatest.Tag] = ISZ()

    if(ignores.elements.exists(elem => org.sireum.ops.StringOps(testName).startsWith(elem))){
      registerIgnoredTest(s"${testName} L${position.lineNumber}", tags.elements:_*)(
        testAir(testName, airFile, ops, resultDir))
    }
    else if(!filter || filters.elements.exists(elem => org.sireum.ops.StringOps(testName).startsWith(elem))) {
      registerTest(s"${testName} L${position.lineNumber}", tags.elements:_*)(
        testAir(testName, airFile, ops, resultDir))
    }
  }

  def testAir(testName: String, airFile: Os.Path, ops: ArsitOptions, resultDir: Option[String]): Unit = {

    val expectedJson = expectedJsonDir / s"${testName}.json"

    val rootTestOutputDir = if(resultDir.nonEmpty) rootResultsDir / resultDir.get / testName else rootResultsDir / testName
    val expectedDir = rootTestOutputDir / "expected"
    val resultsDir = rootTestOutputDir / "results"
    val slangOutputDir = resultsDir / testName
    val outputSharedCDir = slangOutputDir / "src" / "c"
    val outputPlatformCDir = outputSharedCDir

    var testOps = ops(
      outputDir = if(ops.outputDir.name != string"fake") ops.outputDir else slangOutputDir.canon,
      outputSharedCDir = if(ops.outputSharedCDir.name != string"fake") ops.outputSharedCDir else outputSharedCDir.canon,
      outputPlatformCDir = if(ops.outputPlatformCDir.name != string"fake") ops.outputPlatformCDir else outputPlatformCDir.canon,

      packageName = if(ops.packageName != string"") ops.packageName else "packageName_not_set"
    )

    rootTestOutputDir.removeAll()
    rootTestOutputDir.mkdirAll()

    val reporter = Reporter.create

    val model = getModel(airFile.read)

    println(s"Result Dir: ${rootTestOutputDir.canon.toUri}")

    val p:CodeGenPlatform.Type = ops.platform match {
      case ArsitPlatform.JVM => CodeGenPlatform.JVM
      case ArsitPlatform.Linux => CodeGenPlatform.Linux
      case ArsitPlatform.Cygwin => CodeGenPlatform.Cygwin
      case ArsitPlatform.MacOS => CodeGenPlatform.MacOS
      case ArsitPlatform.SeL4 => CodeGenPlatform.SeL4
      case x => halt(s"Unexpected $x")
    }

    val co = CodeGenConfig(
      writeOutResources = F,
      ipc = CodeGenIpcMechanism.SharedMemory,
      verbose = T,
      platform = p,
      slangOutputDir = None(),
      packageName = Some(ops.packageName),
      noProyekIve = T,
      noEmbedArt = ops.noEmbedArt,
      devicesAsThreads = ops.devicesAsThreads,
      slangAuxCodeDirs = ISZ(),
      slangOutputCDir = None(),
      excludeComponentImpl = ops.excludeImpl,
      bitWidth = ops.bitWidth,
      maxStringSize = ops.maxStringSize,
      maxArraySize = ops.maxArraySize,
      runTranspiler = F,
      camkesOutputDir = None(),
      camkesAuxCodeDirs = ISZ(),
      aadlRootDir = None(),
      experimentalOptions = ops.experimentalOptions
    )
    val (rmodel, aadlTypes, symbolTable) = {
      val me = ModelUtil.resolve(model.get, ops.packageName, co, reporter).get
      (me.model, me.types, me.symbolTable)
    }

    val results: ArsitResult = Arsit.run(rmodel, testOps, aadlTypes, symbolTable, reporter)

    reporter.printMessages()

    if(reporter.hasError) {
      assert(F, "Arsit reported errors")
    }

    val resultMap = TestUtil.convertToTestResult(results.resources, resultsDir)

    writeOutTestResults(resultMap, resultsDir)

    var testPass = T

    if(testModes.nonEmpty) {

      val sireum = Os.cwd / "bin" / (if(Os.isWin) "sireum.bat" else "sireum")

      def check(results: OsProto.Proc.Result, failMsg: String): Unit = {
        if(!results.ok) {
          println(s"${testName}: ${failMsg}")
          println("out:")
          println(results.out)
          println("err:")
          println(results.err)
        }
        testPass = testPass && results.ok
      }

      def isLinux(p: ArsitPlatform.Type): B = {
        val ret: B = p match {
          case ArsitPlatform.Linux => T
          case ArsitPlatform.Cygwin => T
          case ArsitPlatform.MacOS => T
          case _ => F
        }
        return ret
      }

      val dir = writeOutTestResults(resultMap, Os.tempDir()) / testName // don't pollute results directory

      def fetch(filename: String): Os.Path = {
        var loc: ISZ[Os.Path] = ISZ()
        def r(p: Os.Path): Unit = {
          if(p.isDir) {  for(pp <- p.list if loc.isEmpty ) { r(pp)} }
          else { if(p.name == filename) { loc = loc :+ p }
          }
        }
        r(dir)
        assert(loc.size == 1, s"Fetch failed for ${filename}. Found ${loc.size} matches. ${loc}")
        return loc(0).canon
      }

      for (testMode <- testModes if (testPass)) {
        testMode match {
          case ArsitTestMode.ProyekTipe =>
            if (!testOps.noEmbedArt) { // need ART source code for Tipe checking
              println("Type checking via proyek tipe ...")
              val results = proc"${sireum.value} proyek tipe --par ${dir.value}".run()
              check(results, "Type checking failed")
            }

          case ArsitTestMode.ProyekCompile =>
            println("Compiling Slang project via proyek compile ...")
            val results = proc"${sireum.value} proyek compile --par ${dir.value}".run()
            check(results, "Proyek compilation failed")

          case ArsitTestMode.ProyekTest =>
            println("Running generated unit tests via proyek test ...")
            val results = proc"${sireum.value} proyek test --par ${dir.value}".run()
            check(results, "Proyek test failed")

          case ArsitTestMode.ProyekRun =>
            val app = s"${testOps.packageName}.Demo"
            println(s"Running demo via proyek run for ${timeoutInSeconds} seconds ...")
            val results = proc"${sireum.value} proyek run ${dir.value} ${app}".timeout(timeoutInSeconds * z"1000").console.run()

          // TODO: get newline injector working again
          //check(results, "Proyek run failed")

          case ArsitTestMode.LinuxCompile =>
            if (isLinux(testOps.platform)) {
              val transpileScript = fetch("transpile.cmd")
              val compileScript = fetch("compile.cmd")

              println(s"Transpiling via ${transpileScript.value} ...")
              var results = proc"${transpileScript.value}".run()
              check(results, "Transpiling failed")

              if (testPass) {
                println(s"Compiling C code via ${compileScript.value} ...")
                results = proc"${compileScript.value} -b -r -l".run()
                check(results, "C compilation failed")
              }
            }

          case ArsitTestMode.SbtCompile =>
            val args: ISZ[String] = ISZ("sbt", "compile")
            val results = Os.proc(args).at(dir).console.run()
            check(results, "SBT compilation failed")

          case ArsitTestMode.SbtTest =>
            val args: ISZ[String] = ISZ("sbt", "test")
            val results = Os.proc(args).at(dir).console.run()
            check(results, "SBT test failed")

          case ArsitTestMode.SbtRun =>
            val args: ISZ[String] = ISZ("sbt", "run")
            val p = Os.proc(args).timeout(timeoutInSeconds * z"1000").at(dir).console()
            val results = TestOs.proc2(p, Some("[info] Done compiling."), Some("\n"))
            check(results, "SBT run failed")
        }
      }
    }

    val expectedMap: TestResult =
      if(generateExpected) {
        TestUtil.writeExpected(resultMap, expectedJson)
        println(s"Wrote: ${expectedJson}")
        resultMap
      }
      else if(expectedJson.exists) {
        TestUtil.readExpected(expectedJson)
      }
      else {
        testPass = F
        Console.err.println(s"Expected does not exist: ${expectedJson}")
        TestResult(Map.empty)
      }

    writeOutTestResults(expectedMap, expectedDir)

    var allEqual = T
    for(r <- resultMap.map.entries) {
      if(expectedMap.map.contains(r._1)) {
        val e = expectedMap.map.get(r._1).get
        allEqual &= {
          val ignores = ISZOps(ISZ("build.sbt", "build.sc", "versions.properties", "project.cmd"))
          val ignoreFile = ignoreBuildDefChanges && ignores.exists(p => r._1.native.endsWith(p))
          val sameContents = r._2 == e
          if(!sameContents) {
            var reason: ISZ[String] = ISZ()
            (r._2, e) match {
              case ((ri: ITestResource, ei: ITestResource)) =>
                if(ri.content != ei.content) reason = reason :+ "content is not the same"
                if(ri.overwrite != ei.overwrite) reason = reason :+ "overwrite flag is not the same"
                if(ri.makeExecutable != ei.makeExecutable) reason = reason :+ "makeExecutable flag is not the same"
                if(ri.makeCRLF != ei.makeCRLF) reason = reason :+ "makeCRLF flag is not the same"
              case ((re: ETestResource, ee: ETestResource)) =>
                if(re.content != ee.content) reason = reason :+ "content is not the same"
                if(re.symlink != ee.symlink) reason = reason :+ "symlink flag is not the same"
            }
            eprintln(st"${r._1} ${(reason, ", ")}".render)
          }
          ignoreFile || sameContents
        }
      } else if(!generateExpected) {
        allEqual = F
        expectedMap.map.keySet.elements.foreach(p => println(p))
        eprintln(s"Expected missing: ${r._1}")
      }
    }

    testPass = testPass && allEqual

    assert(testPass, s"Test fail in ${rootTestOutputDir.canon.toUri}")
  }
}

object ArsitTest {

  val fakedir = Os.path("fake")
  val baseOptions = ArsitOptions(
    outputDir = fakedir,
    packageName = "",
    noEmbedArt = F,
    bless = F,
    verbose = T,
    devicesAsThreads = T,
    ipc = IpcMechanism.SharedMemory,
    auxCodeDirs = ISZ(),
    outputSharedCDir = fakedir,
    outputPlatformCDir = fakedir,
    excludeImpl = F,
    platform = ArsitPlatform.JVM,
    bitWidth = 64,
    maxStringSize = 256,
    maxArraySize = 1,
    pathSeparator = '/',
    experimentalOptions = ISZ()
  )

  def filterTestsSet(): Option[B] = {
    val FILTER = "FILTER"
    return if (Os.env(FILTER).nonEmpty) return Some(Os.env(FILTER).get.native.toBoolean) else None()
  }

  val rootResultsDir: Os.Path = {
    val rootDir = Os.path("./hamr/codegen/arsit/jvm/src/test")
    if(rootDir.exists) rootDir / "results"
    else Os.tempDir() / "results"
  }

  def getDirectories(): (Os.Path, Os.Path) =  {
    val intellijTestDir = Os.path("./hamr/codegen/arsit/jvm/src/test/scala")
    val rootResultsDir: Os.Path = if(intellijTestDir.exists) {
      // use local/intellij copy
      intellijTestDir
    } else {
      // probably running from jar so copy resources to a temp directory
      val temp: Os.Path = Os.tempDir()
      for (entry <- testResources()) {
        assert(entry._1.head == "expected" || entry._1.head == "models")
        val f = temp / entry._1.mkString("/")
        f.writeOver(entry._2)
      }
      temp
    }
    (rootResultsDir / "expected", rootResultsDir / "models")
  }

  def testResources(): scala.collection.Map[scala.Vector[Predef.String], Predef.String] = {
    // scala/java 'resources' directories don't play nicely with mill so instead add the contents
    // of 'expected' and 'models' into the binary via RC.text.  These can then
    // be retrieved as a map from 'exploded path' to 'contents' via a call to 'testResources()'
    RC.text(Vector("../../../../../")) { (p, f) =>
      val allowedDirs: ISZ[String] = ISZ("expected", "models")

      // exclude unneeded/binary files
      val excludedResources: ISZ[String] = ISZ("png", "pdf", "md", "dot", "aadl", "aadl_diagram")

      val filename = Os.path(p.last)

      val allow = ops.ISZOps(allowedDirs).contains(p.head) &&
        !ops.ISZOps(excludedResources).contains(filename.ext.native)

      if(allow) {
        //println(p)
      }

      allow
    }
  }

  def getModel(s: String): Option[Aadl] = {
    return JSON.toAadl(s) match {
      case Either.Left(m) => Some(m)
      case Either.Right(m) =>
        eprintln(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
        None()
    }
  }

  def writeOutTestResults(testResults: TestResult, dir: Os.Path, verbose: B = F): Os.Path = {
    // arsit run outside of codegen so need to write out the results ala codegen's writeOutResources
    for(result <- testResults.map.entries) {
      val dstPath = (dir / result._1).canon
      result._2 match {
        case i: ITestResource =>
          if(i.overwrite || !dstPath.exists) {
            dstPath.writeOver(i.content)
            if(verbose) {
              println(s"Wrote: ${dstPath}")
            }
            if(i.makeExecutable) {
              dstPath.chmodAll("700")
            }
          }
        case e: ETestResource =>
          if(e.symlink) {
            halt("sym linking not yet supported")
          } else {
            dstPath.writeOver(e.content)
            if(verbose) {
              println(s"Copied to ${dstPath}")
            }
          }
      }
    }
    return dir
  }
}