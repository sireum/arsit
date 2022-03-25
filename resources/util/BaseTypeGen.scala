package __BASE_PACKAGE_NAME__

import org.scalacheck.Gen
import org.sireum.Z

object Base_Types_Gen {

  //===============================================
  //   Z   Generators
  //===============================================
  object Z {

    // this should be auto-generated, BUT there must be some mechanism developed to set the bounds
    // TODO: Develop mechanism to set the bounds on simple generators for base types.
    val simple: Gen[Z] = for {
      // Use ScalaCheck's long generator to generate values for Z
      n <- Gen.choose(-1000: Long,1000: Long)
    } yield org.sireum.Z.MP.Long(n)

    // auto-generated
    def bounded(low_Z:Z, high_Z:Z): Gen[Z] = for {
      // TODO: add assertions on bounds
      // Use ScalaCheck's long generator to generate values for Z
      n <- Gen.choose(low_Z.toLong: Long, high_Z.toLong: Long)
    } yield org.sireum.Z.MP.Long(n)

    val default: Gen[Z] = simple
  }

  //===============================================
  //   F32   Generators
  //===============================================
  object F32 {
    // auto-generated
    val simple: Gen[org.sireum.F32] = for {
      // use ScalaCheck supplied generator for Scala float and convert between
      //  Slang F32
      // bounds on values chosen are determined by GUMBOCheck configuration
      n <- Gen.choose(GUMBOCheck.F32_lower.native, GUMBOCheck.F32_upper.native)
    } yield org.sireum.F32(n)

    // auto-generated
    // Generator with developer supplied bounds
    def bounded(low_F32:org.sireum.F32, high_F32:org.sireum.F32): Gen[org.sireum.F32] = for {
      n <- Gen.choose(low_F32.native, high_F32.native)
    } yield org.sireum.F32(n)

    // "reference" to default generator (which can be configured in GUMBOCheck profiles)
    var default: Gen[org.sireum.F32] = simple
  }
}
