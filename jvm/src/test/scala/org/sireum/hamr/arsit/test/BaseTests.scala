package org.sireum.hamr.arsit.test

import org.sireum._
import org.sireum.hamr.arsit.Cli.{ArsitOption, ArsitPlatform}
import org.sireum.hamr.arsit.test.ArsitTest.baseOptions

class BaseTests extends ArsitTest {

  override def generateExpected: B = if(super.generateExpected) T else F

  val name = "building_control_gen_mixed"

  val resultDir: Option[String] = Some(getClass.getSimpleName)
  val modelsDir = baseModelsDir / getClass.getSimpleName

  val modelDir = modelsDir / name
  val model = modelDir / ".slang" / "BuildingControl_BuildingControlDemo_i_Instance.json"


  "Can run these individually" - {
    "JVM" in {
      val platform: ArsitPlatform.Type = ArsitPlatform.JVM
      testAir(s"$name--${platform}", model,
        baseOptions(platform = platform),
        resultDir)
    }
    "Linux" in {
      val platform: ArsitPlatform.Type = ArsitPlatform.Linux
      testAir(s"$name--${platform}", model,
        baseOptions(platform = platform),
        resultDir)
    }
  }


  val testsV1 = Tests {

    var platform: ArsitPlatform.Type = ArsitPlatform.JVM
    test(s"$name--${platform}", baseOptions(platform = platform))

    platform = ArsitPlatform.JVM
    test(s"$name--${platform}-Do-not-embed-art",
      baseOptions(
        platform = platform,
        embedArt = F))

    platform = ArsitPlatform.Linux
    test(s"$name--${platform}", baseOptions(platform = platform))

    platform = ArsitPlatform.Linux
    test(s"$name--${platform}-Excludes-Impl",
      baseOptions(
        platform = platform,
        excludeImpl = T))

    platform = ArsitPlatform.SeL4
    test(s"$name--${platform}",
      baseOptions(
        platform = platform,
        excludeImpl = T,
        devicesAsThreads = F,
        maxStringSize = 300,
        packageName = "building_control_gen_mixed"
      ))
  }

  val testsV2 = Tests {
    ISZ(ArsitPlatform.JVM, ArsitPlatform.Linux).foreach(platform =>
      test(s"$name--$platform--v2", baseOptions(platform = platform)))
  }

  def test(name: String, options: ArsitOption): Unit = {
    test(name, model, options, resultDir)
  }
}
