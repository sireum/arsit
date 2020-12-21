// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.templates.StringTemplate.doNotEditComment
import org.sireum.hamr.codegen.common.types.DataTypeNames

object TypeTemplate {
  def Base_Types(basePackage: String): ST = {
    val ret =
      st"""// #Sireum
          |
          |package ${basePackage}
          |
          |import org.sireum._
          |import org.sireum.S8._
          |import org.sireum.S16._
          |import org.sireum.S32._
          |import org.sireum.S64._
          |import org.sireum.U8._
          |import org.sireum.U16._
          |import org.sireum.U32._
          |import org.sireum.U64._
          |
          |${doNotEditComment(None())}
          |
          |object Base_Types {
          |
          |  type Boolean = B
          |
          |  type Integer = Z
          |
          |  type Integer_8 = S8
          |  type Integer_16 = S16
          |  type Integer_32 = S32
          |  type Integer_64 = S64
          |
          |  type Unsigned_8 = U8
          |  type Unsigned_16 = U16
          |  type Unsigned_32 = U32
          |  type Unsigned_64 = U64
          |
          |  // TODO: Base_Types::Natural
          |
          |  type Float = R
          |  type Float_32 = F32
          |  type Float_64 = F64
          |
          |  type Character = C
          |  type String = org.sireum.String
          |
          |  type Bits = org.sireum.ISZ[B]
          |
          |  @datatype class Boolean_Payload(value: B) extends art.DataContent
          |
          |  @datatype class Integer_Payload(value: Z) extends art.DataContent
          |
          |  @datatype class Integer_8_Payload(value: S8) extends art.DataContent
          |  @datatype class Integer_16_Payload(value: S16) extends art.DataContent
          |  @datatype class Integer_32_Payload(value: S32) extends art.DataContent
          |  @datatype class Integer_64_Payload(value: S64) extends art.DataContent
          |
          |  @datatype class Unsigned_8_Payload(value: U8) extends art.DataContent
          |  @datatype class Unsigned_16_Payload(value: U16) extends art.DataContent
          |  @datatype class Unsigned_32_Payload(value: U32) extends art.DataContent
          |  @datatype class Unsigned_64_Payload(value: U64) extends art.DataContent
          |
          |  @datatype class Float_Payload(value: R) extends art.DataContent
          |  @datatype class Float_32_Payload(value: S32) extends art.DataContent
          |  @datatype class Float_64_Payload(value: S64) extends art.DataContent
          |
          |  @datatype class Character_Payload(value: C) extends art.DataContent
          |  @datatype class String_Payload(value: String) extends art.DataContent
          |
          |  @datatype class Bits_Payload(value: ISZ[B]) extends art.DataContent
          |
          |  def Boolean_empty(): Boolean = { return F }
          |
          |  def Integer_empty(): Integer = { return z"0" }
          |
          |  def Integer_8_empty(): Integer_8 = { return s8"0" }
          |  def Integer_16_empty(): Integer_16 = { return s16"0" }
          |  def Integer_32_empty(): Integer_32 = { return s32"0" }
          |  def Integer_64_empty(): Integer_64 = { return s64"0" }
          |
          |  def Unsigned_8_empty(): Unsigned_8 = { return u8"0" }
          |  def Unsigned_16_empty(): Unsigned_16 = { return u16"0" }
          |  def Unsigned_32_empty(): Unsigned_32 = { return u32"0" }
          |  def Unsigned_64_empty(): Unsigned_64 = { return u64"0" }
          |
          |  def Float_empty(): Float = { return r"0" }
          |  def Float_32_empty(): Float_32 = { return f32"0" }
          |  def Float_64_empty(): Float_64 = { return f64"0" }
          |
          |  def Character_empty(): Character = { return ' ' }
          |  def String_empty(): String = { return "" }
          |
          |  def Bits_empty(): Bits = { return ISZ() }
          |}"""
    return ret
  }
  @pure def enumType(typeNames: DataTypeNames,
                     values: ISZ[String]): ST = {
    val vals = values.map((m: String) => st""""$m"""")
    val ret: ST =
      st"""@enum object ${typeNames.typeName} {
          |  ${(vals, "\n")}
          |}
          |"""
    return ret
  }

  @pure def dataType(typeNames: DataTypeNames,
                     fields: ISZ[ST],
                     paramInits: ISZ[String],
                     optChecks: Option[ST]): ST = {
    val ret: ST =
      st"""object ${typeNames.typeName} {
          |  def empty(): ${typeNames.qualifiedTypeName} = {
          |    return ${typeNames.qualifiedTypeName}(${(paramInits, ", ")})
          |  }
          |}
          |
          |@datatype class ${typeNames.typeName}(
          |  ${(fields, ",\n")}) {
          |  $optChecks
          |}
          |"""
    return ret
  }

  @pure def typeSkeleton(typeNames: DataTypeNames): ST = {
    val ret: ST =
      st"""object ${typeNames.typeName} {
          |  def empty(): ${typeNames.qualifiedTypeName} = {
          |    return ${typeNames.qualifiedTypeName}()
          |  }
          |}
          |
          |@datatype class ${typeNames.typeName}() // type skeleton
          |"""
    return ret
  }

  @pure def payloadType(typeNames: DataTypeNames): ST = {
    val typeName = typeNames.qualifiedReferencedTypeName
    val payloadTypeName = typeNames.payloadName
    val emptyPayload = typeNames.empty()

    val ret: ST =
      st"""object $payloadTypeName {
          |  def empty(): $payloadTypeName = {
          |    return $payloadTypeName($emptyPayload)
          |  }
          |}
          |
          |@datatype class $payloadTypeName(value: $typeName) extends art.DataContent"""
    return ret
  }

  @pure def typeS(topLevelPackageName: String,
                  packageName: String,
                  body: ST,
                  payload: ST,
                  canOverwrite: B): ST = {
    val overwrite: ST = if (canOverwrite) {
      st"""
          |${StringTemplate.doNotEditComment(None())}
          |"""
    } else {
      st""
    }

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import $topLevelPackageName._
          |$overwrite
          |$body
          |$payload
          |"""
    return ret
  }
}
