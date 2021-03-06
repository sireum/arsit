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

    var concreteCheckParams: ISZ[ST] = ISZ()
    var concreteCheckBlocks: ISZ[ST] = ISZ()
    var concreteCheckScalaDoc: ISZ[ST] = ISZ()

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
        val scalaDoc = concretePutScalaDoc.map((m: ST) => st"${m}")
        Some(st"""/** helper function to set the values of all input ports.
                 | ${(scalaDoc, "\n")}
                 | */
                 |def put_concrete_inputs(${(concretePutParams, ",\n")}): Unit = {
                 |  ${(concretePutBlocks, "\n")}
                 |}
                 |
                 |""")
      }

    val testFailures = "testFailures"

    val getters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isOutFeature(p.feature)).map((p: Port) => {
      val portName = p.name
      val portNameValue = s"${portName}Value"
      val getterName = s"get_${portName}"
      val payloadGetterName: String = s"get_${portName}_payload()"

      val isEvent = p.feature.category == FeatureCategory.EventPort
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
      val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
      val value: String = if (isEvent) "Empty()" else "v"

      val (checkParamType, concretePreamble, checkExplanation, checkScalaDoc): (String, ST, String, ST) =
        p.feature.category match {
        case FeatureCategory.EventPort =>
          val preamble =
            st"""// TODO: event port getter should return the number of events in
                |//       the output queue when queue sizes > 1 support is added to ART
                |val ${portNameValue}: Z = if(${getterName}().nonEmpty) z"1" else z"0""""
          val scalaDoc = st"""* @param ${portName} method that will be called with the number of events to be sent
                             |*        on the outgoing event port '${portName}'."""
          ("Z", preamble, s"$${${portNameValue}} events were in the outgoing event queue", scalaDoc)
        case FeatureCategory.EventDataPort =>
          val preamble =
            st"""var ${portNameValue}: ISZ[${typeName}] = ISZ()
                |// TODO: event data port getter should return all of the events/payloads
                |//       received on event data ports when queue sizes > 1 support is added
                |//       to ART
                |if(${getterName}().nonEmpty) ${portNameValue} = ${portNameValue} :+ ${getterName}().get"""
          val scalaDoc = st"""* @param ${portName} method that will be called with the payloads to be sent
                             |*        on the outgoing event data port '${portName}'."""
          (s"ISZ[$typeName]", preamble, s"received $${${portNameValue}.size} events with the following payloads $${${portNameValue}}", scalaDoc)
        case FeatureCategory.DataPort =>
          val preamble = st"val ${portNameValue}: ${typeName} = ${getterName}().get"
          val scalaDoc = st"""* @param ${portName} method that will be called with the value of the outgoing data
                             |*        port '${portName}'."""
          (typeName, preamble, s"value of the outgoing data port is $${${portNameValue}}", scalaDoc)
        case _ => halt("Unexpected")
      }

      concreteCheckScalaDoc = concreteCheckScalaDoc :+ checkScalaDoc

      concreteCheckParams = concreteCheckParams :+
        st"${portName}: ${checkParamType} => B = ${portName}Param => {T}"

      concreteCheckBlocks = concreteCheckBlocks :+
        st"""${concretePreamble}
            |if(!${portName}(${portNameValue})) {
            |  testFailures = testFailures :+ st"'${portName}' did not match expected: ${checkExplanation}"
            |}"""

      st"""// getter for out ${p.feature.category}
          |def ${getterName}(): Option[${typeName}] = {
          |  val value: Option[${typeName}] = ${payloadGetterName} match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) => fail(s"Unexpected payload on port ${portName}.  Expecting '${payloadType}' but received $${v}")
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}
          |
          |// payload getter for out ${p.feature.category}
          |def ${payloadGetterName}: Option[${payloadType}] = {
          |  return ArtNative_Ext.observeOutPortValue(bridge.initialization_api.${addId(portName)}).asInstanceOf[Option[${payloadType}]]
          |}
          |"""
    })

    val concreteChecker: Option[ST] =
    if(concreteCheckBlocks.isEmpty) { None() }
    else {
      val scalaDoc = concreteCheckScalaDoc.map((m: ST) => st"${m}")
      Some(st"""/** helper function to check ${names.componentSingletonType}'s
               | * output ports.  Use named arguments to check subsets of the output ports.
               | ${(scalaDoc, "\n")}
               | */
               |def check_concrete_output(${(concreteCheckParams, ",\n")}): Unit = {
               |  var ${testFailures}: ISZ[ST] = ISZ()
               |
               |  ${(concreteCheckBlocks, "\n")}
               |
               |  assert(testFailures.isEmpty, st"$${(testFailures, "\n")}".render)
               |}
               |
               |""")
    }

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
          |  ${concreteChecker}
          |  ${(setters ++ getters, "\n")}
          |}
          |"""
    return ret
  }
}
