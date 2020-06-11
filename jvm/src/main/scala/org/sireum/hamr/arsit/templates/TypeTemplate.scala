// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.types.DataTypeNames

object TypeTemplate {
  @pure def enumType(typeNames: DataTypeNames,
                     values: ISZ[String]): ST = {
    val vals = values.map((m: String) => st"'$m")
    return st"""@enum object ${typeNames.typeName} {
               |  ${(vals, "\n")}
               |}
               |"""
  }

  @pure def dataType(typeNames: DataTypeNames,
                     fields: ISZ[ST],
                     paramInits: ISZ[String],
                     optChecks: Option[ST]): ST = {
    return st"""object ${typeNames.typeName} {
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
  }

  @pure def typeSkeleton(typeNames: DataTypeNames): ST = {
    val typeName = typeNames.qualifiedReferencedTypeName
    val payloadTypeName = typeNames.payloadName
    val emptyPayload = typeNames.empty()

    return st"""object ${typeNames.typeName} {
               |  def empty(): ${typeNames.qualifiedTypeName} = {
               |    return ${typeNames.qualifiedTypeName}()
               |  }
               |}
               |
               |@datatype class ${typeNames.typeName}() // type skeleton
               |"""
  }

  @pure def payloadType(typeNames: DataTypeNames) : ST = {
    val typeName = typeNames.qualifiedReferencedTypeName
    val payloadTypeName = typeNames.payloadName
    val emptyPayload = typeNames.empty()

    return st"""object $payloadTypeName {
               |  def empty(): $payloadTypeName = {
               |    return $payloadTypeName($emptyPayload)
               |  }
               |}
               |
               |@datatype class $payloadTypeName(value: $typeName) extends art.DataContent"""
  }

  @pure def typeS(topLevelPackageName: String,
                  packageName: String,
                  body: ST,
                  payload: ST,
                  canOverwrite: B): ST = {
    val overwrite: ST = if(canOverwrite) {
      st"""
          |${StringTemplate.doNotEditComment(None())}
          |"""
    } else { st"" }

    return st"""// #Sireum
               |
               |package $packageName
               |
               |import org.sireum._
               |import $topLevelPackageName._
               |$overwrite
               |$body
               |$payload
               |"""
  }
}
