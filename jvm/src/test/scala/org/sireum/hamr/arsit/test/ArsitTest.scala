package org.sireum.hamr.arsit.test

import org.sireum._
import org.sireum.$internal.RC
import org.sireum.hamr.arsit.test.util.ArsitTestMode
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.arsit.{Arsit, ArsitResult}
import org.sireum.hamr.codegen.common.util.test.{TestJSON, TestOs, TestResource, TestResult, TestUtil}
import org.sireum.hamr.ir.{Aadl, JSON}
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.hamr.arsit.test.ArsitTest._

trait ArsitTest extends TestSuite {

  def generateExpected: B = F

  def testMode: ArsitTestMode.Type = ArsitTestMode.Base
  def timeoutInSeconds: Z = 7

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

    var testOps = ops(
      outputDir = if(ops.outputDir != string"") ops.outputDir else slangOutputDir.canon.value,
      packageName = if(ops.packageName != string"") ops.packageName else "packageName_not_set"
    )

    rootTestOutputDir.removeAll()
    rootTestOutputDir.mkdirAll()

    val reporter = Reporter.create

    val model = getModel(airFile.read)

    println(s"Result Dir: ${rootTestOutputDir.canon.toUri}")

    val results: ArsitResult = Arsit.run(model.get, testOps, reporter)

    reporter.printMessages()

    if(reporter.hasError) {
      assert(F, "Arsit reported errors")
    }

    val resultMap = TestResult(Map.empty ++ (results.resources.map(m => {
      val key = resultsDir.relativize(Os.path(m.path)).value
      (key, TestResource(m.content.render, m.overwrite, m.makeExecutable))
    })))

    writeOutTestResults(resultMap, resultsDir)

    var testPass = T

    envTest(testMode) match {
      case ArsitTestMode.SbtCompile =>
        val sbtDir = writeOutTestResults(resultMap, Os.tempDir()) / testName // don't pollute results directory
        val args: ISZ[String] = ISZ("sbt", "compile")
        val results = Os.proc(args).at(sbtDir).console.run()
        testPass = testPass && results.ok

      case ArsitTestMode.SbtTest =>
        val sbtDir = writeOutTestResults(resultMap, Os.tempDir()) / testName // don't pollute results directory
        val args: ISZ[String] = ISZ("sbt", "test")
        val results = Os.proc(args).at(sbtDir).console.run()
        testPass = testPass && results.ok

      case ArsitTestMode.SbtRun =>
        val sbtDir = writeOutTestResults(resultMap, Os.tempDir()) / testName // don't pollute results directory
        val args: ISZ[String] = ISZ("sbt", "run")
        val p = Os.proc(args).timeout(timeoutInSeconds * z"1000").at(sbtDir).console()
        val results = TestOs.proc2(p, Some("[info] Done compiling."), Some("\n"))
        testPass = testPass && results.ok

      case ArsitTestMode.Base =>
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

    testPass = testPass && resultMap.map == expectedMap.map

    assert(testPass, s"Test fail in ${rootTestOutputDir.canon.toUri}")
  }
}

object ArsitTest {

  val baseOptions = ArsitOptions(
    outputDir = "",
    packageName = "",
    embedArt = T,
    bless = F,
    verbose = T,
    devicesAsThreads = T,
    ipc = IpcMechanism.SharedMemory,
    auxCodeDir = ISZ(),
    outputCDir = None(),
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
    for(result <- testResults.map.entries) {
      val r = (dir / result._1).canon
      if(result._2.overwrite || !r.exists) {
        r.writeOver(result._2.content)
        if(verbose) {
          println(s"Wrote: ${r}")
        }
        if(result._2.makeExecutable) {
          r.chmodAll("700")
        }
      }
    }
    return dir
  }

  def envTest(testMode: ArsitTestMode.Type): ArsitTestMode.Type = {
    val ret: ArsitTestMode.Type =
      Os.env("ARSIT_TEST_MODE") match {
        case Some(x) => ArsitTestMode.byName(x).get
        case _ => testMode
      }
    return ret
  }
}