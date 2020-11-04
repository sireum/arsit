// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir.FeatureCategory

object TestTemplate {

  @pure def addId(s: String): String = {
    return s"${s}_Id"
  }

  @pure def bridgeTestSuite(basePackage: String,
                            names: Names,
                            ports: ISZ[Port]): ST = {
    val ret: ST =
      st"""package ${names.packageName}
          |
          |import org.sireum._
          |import ${basePackage}._
          |
          |${StringTemplate.safeToEditComment()}
          |class ${names.testName} extends ${names.testApisName} {
          |
          |  test("Example Unit Test"){
          |    executeTest()
          |  }
          |}
          |"""
    return ret
  }

  @pure def bridgeTestApis(basePackage: String,
                           names: Names,
                           ports: ISZ[Port]): ST = {

    var concretePutParams: ISZ[ST] = ISZ()
    var concretePutBlocks: ISZ[ST] = ISZ()
    var concretePutScalaDoc: ISZ[ST] = ISZ()

    val setters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isInFeature(p.feature)).map((p: Port) => {
      val putMethodName = s"put_${p.name}"

      val (putParamName, putArgName, concreteParamName, concreteParamType): (String, String, String, String) =
        p.feature.category match {
          case FeatureCategory.EventPort => {
            concretePutScalaDoc = concretePutScalaDoc :+
              st"""* @param ${p.name} the number of events to place in the ${p.name} event port queue.
                  |*   ART currently supports single element event queues so at most
                  |*   one event will be placed in the queue."""

            concretePutBlocks = concretePutBlocks :+ st"""for(i <- 0 until ${p.name}) {
                                                         |  ${putMethodName}()
                                                         |}"""
            (s"", s"Empty()", p.name, "Z")
          }
          case _ => {
            val ptype = p.getPortTypeNames.qualifiedReferencedTypeName
            val concreteParamType: String =
              if (CommonUtil.isAadlEventDataPort(p.feature)) s"ISZ[${ptype}]"
              else ptype

            if(CommonUtil.isAadlEventDataPort(p.feature)) {
              concretePutScalaDoc = concretePutScalaDoc :+
                st"""* @param ${p.name} payloads for event data port ${p.name}.
                    |*   ART currently supports single element event data queues so
                    |*   only the last element of ${p.name} will be used"""

              concretePutBlocks = concretePutBlocks :+
                st"""for(v <- ${p.name}){
                    |  ${putMethodName}(v)
                    |}"""

            } else {
              concretePutScalaDoc = concretePutScalaDoc :+
                st"* @param ${p.name} payload for data port ${p.name}"

              concretePutBlocks = concretePutBlocks :+
                st"""${putMethodName}(${p.name})"""
            }

            (s"value : ${p.getPortTypeNames.qualifiedReferencedTypeName}",
              s"${p.getPortTypeNames.qualifiedPayloadName}(value)",
              p.name,
              concreteParamType)
          }
        }

      concretePutParams = concretePutParams :+ st"${concreteParamName} : ${concreteParamType}"

      st"""// setter for in ${p.feature.category}
          |def ${putMethodName}(${putParamName}): Unit = {
          |  ArtNative_Ext.insertInPortValue(bridge.operational_api.${p.name}_Id, ${putArgName})
          |}
          |"""
    })

    val concretePutter: Option[ST] =
      if(concretePutBlocks.isEmpty) { None() }
      else {
        val scalaDoc = concretePutScalaDoc.map(m => st"${m}")
        Some(st"""/** helper function to set the values of all input ports.
                 | ${(scalaDoc, "\n")}
                 | */
                 |def put_concrete_inputs(${(concretePutParams, ",\n")}) = {
                 |  ${(concretePutBlocks, "\n")}
                 |}
                 |
                 |""")
      }

    val getters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isOutFeature(p.feature)).map((p: Port) => {
      val isEvent = p.feature.category == FeatureCategory.EventPort
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
      val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
      val value: String = if (isEvent) "Empty()" else "v"
      val payloadMethodName: String = s"get_${p.name}_payload()"

      st"""// getter for out ${p.feature.category}
          |def get_${p.name}(): Option[${typeName}] = {
          |  val value: Option[${typeName}] = ${payloadMethodName} match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) => fail(s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}
          |
          |// payload getter for out ${p.feature.category}
          |def ${payloadMethodName}: Option[${payloadType}] = {
          |  return ArtNative_Ext.observeOutPortValue(bridge.initialization_api.${addId(p.name)}).asInstanceOf[Option[${payloadType}]]
          |}
          |"""
    })

    val ret: ST =
      st"""package ${names.packageName}
          |
          |import org.sireum._
          |import art.{ArtNative_Ext, Empty}
          |import ${basePackage}._
          |
          |${StringTemplate.doNotEditComment(None())}
          |abstract class ${names.testApisName} extends BridgeTestSuite[${names.bridge}](Arch.${names.instanceName}) {
          |
          |  ${concretePutter}
          |  ${(setters ++ getters, "\n")}
          |}
          |"""
    return ret
  }
}
