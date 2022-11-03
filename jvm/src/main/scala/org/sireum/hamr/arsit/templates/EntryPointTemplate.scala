// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.FeatureCategory

@datatype class EntryPointTemplate(parameters: ISZ[ST],
                                   localVars: ISZ[ST],

                                   defaultActivateBody: ST,

                                   defaultInitialiseBody: ST,
                                   defaultTestInitialiseBody: ST,

                                   defaultComputeBody: ST,
                                   defaultTestComputeBody: ST,

                                   defaultDeactivateBody: ST,
                                   defaultFinaliseBody: ST,
                                   defaultRecoverBody: ST) {
  def generateDefault(): ST = {
    halt("stub")
  }
}

object EntryPointTemplate {
  @pure def assignEntryPointImpl(names: NameProvider): ST = {
    val ret: ST = st"${names.componentEntryPointSingletonQualifiedName}.impl = ${names.componentEntryPointImplName}()"
    return ret
  }

  @pure def genEntryPointImpl(names: NameProvider,
                              ports: ISZ[Port],
                              dispatchProtocol: Dispatch_Protocol.Type): ST = {

    def genMethod(sig: ST, body: ST): ST = {
      val ret: ST =
        st"""def ${sig} = {
            |  ${body}
            |}"""
      return ret
    }

    var entryPointImplMethods: ISZ[ST] = ISZ()

    if (dispatchProtocol == Dispatch_Protocol.Periodic) {
      val ttsig = st"timeTriggered(api: ${names.apiOperationalQualifiedName}): Unit"
      entryPointImplMethods = entryPointImplMethods :+ genMethod(ttsig, st"${names.componentSingletonTypeQualifiedName}.timeTriggered(api)")
    } else {
      val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
      var first = T
      for(p <- inEventPorts) {
        val handlerName: String = s"handle_${p.name}"

        val (eventSig, args) : (ST, String) = p.feature.category match {
          case FeatureCategory.EventDataPort =>
            val edpSig = st"$handlerName(api: ${names.apiOperationalQualifiedName}, value : ${p.getPortTypeNames.qualifiedReferencedTypeName}): Unit"
            (edpSig, "api, value")
          case FeatureCategory.EventPort =>
            val epSig = st"$handlerName(api: ${names.apiOperationalQualifiedName}): Unit"
            (epSig, "api")
          case _ => halt(s"Unexpected ${p.feature.category}")
        }

        entryPointImplMethods = entryPointImplMethods :+ genMethod(eventSig, st"${names.componentSingletonTypeQualifiedName}.${handlerName}(${args})")
      }
    }

    for(m <- EntryPoints.elements.filter((f: EntryPoints.Type) => f != EntryPoints.compute &&
      f != EntryPoints.testCompute && f != EntryPoints.testInitialise)){
      val apiType: String =
        if(m == EntryPoints.initialise) names.apiInitializationQualifiedName
        else names.apiOperationalQualifiedName
      val esig = st"${m.string}(api: ${apiType}): Unit"

      entryPointImplMethods = entryPointImplMethods :+ genMethod(esig, st"${names.componentSingletonTypeQualifiedName}.${m.string}(api)")
    }

    val entryPointImpl: ST =
      st"""@record class ${names.componentEntryPointImplName} extends ${names.componentEntryPointTraitQualifiedName} {
          |  ${(entryPointImplMethods, "\n\n")}
          |}
          |"""

    return entryPointImpl
  }

  @pure def genEntryPointObject(topLevelPackageName: String,
                                packageName: String,
                                names: NameProvider,
                                ports: ISZ[Port],
                                dispatchProtocol: Dispatch_Protocol.Type): ST = {

    def genMethod(sig: ST, body: ST): ST = {
      val ret: ST =
        st"""def ${sig} = {
            |  ${body}
            |}"""
      return ret
    }

    var entryPointTraitMethods: ISZ[ST] = ISZ()
    var entryPointStubMethods: ISZ[ST] = ISZ()
    var entryPointSingletonMethods: ISZ[ST] = ISZ()

    if (dispatchProtocol == Dispatch_Protocol.Periodic) {
      val ttsig = st"timeTriggered(api: ${names.apiOperationalQualifiedName}): Unit"

      entryPointSingletonMethods = entryPointSingletonMethods :+ genMethod(ttsig, st"impl.timeTriggered(api)")
      entryPointTraitMethods = entryPointTraitMethods :+ st"def ${ttsig}"
      entryPointStubMethods = entryPointStubMethods :+ genMethod(ttsig, st"stubHalt()")
    } else {
      val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
      var first = T
      for(p <- inEventPorts) {
        val handlerName: String = s"handle_${p.name}"

        val (eventSig, args) : (ST, String) = p.feature.category match {
          case FeatureCategory.EventDataPort =>
            val edpSig = st"$handlerName(api: ${names.apiOperationalQualifiedName}, value : ${p.getPortTypeNames.qualifiedReferencedTypeName}): Unit"
            (edpSig, "api, value")
          case FeatureCategory.EventPort =>
            val epSig = st"$handlerName(api: ${names.apiOperationalQualifiedName}): Unit"
            (epSig, "api")
          case _ => halt(s"Unexpected ${p.feature.category}")
        }

        entryPointSingletonMethods = entryPointSingletonMethods :+ genMethod(eventSig, st"impl.${handlerName}(${args})")
        entryPointTraitMethods = entryPointTraitMethods :+ st"def ${eventSig}"
        entryPointStubMethods = entryPointStubMethods :+ genMethod(eventSig, st"stubHalt()")
      }
    }

    for(m <- EntryPoints.elements.filter((f: EntryPoints.Type) => f != EntryPoints.compute &&
      f != EntryPoints.testInitialise && f != EntryPoints.testCompute)){
      val apiType: String =
        if(m == EntryPoints.initialise) names.apiInitializationQualifiedName
        else names.apiOperationalQualifiedName
      val esig = st"${m.string}(api: ${apiType}): Unit"

      entryPointSingletonMethods = entryPointSingletonMethods :+ genMethod(esig, st"impl.${m.string}(api)")
      entryPointTraitMethods = entryPointTraitMethods :+ st"def ${esig}"
      entryPointStubMethods = entryPointStubMethods :+ genMethod(esig, st"stubHalt()")
    }

    val entryPointObject: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |object ${names.componentEntryPointSingletonName} {
          |  var impl: ${names.componentEntryPointTraitName} = ${names.componentEntryPointStub}()
          |
          |  ${(entryPointSingletonMethods, "\n\n")}
          |}
          |
          |@msig trait ${names.componentEntryPointTraitName} {
          |  ${(entryPointTraitMethods, "\n\n")}
          |}
          |
          |@record class ${names.componentEntryPointStub} extends ${names.componentEntryPointTraitName} {
          |  def stubHalt(): Unit = { halt("Stub implementation of ${names.componentEntryPointTraitName}") }
          |
          |  ${(entryPointStubMethods, "\n\n")}
          |}
          |"""

    return entryPointObject
  }

}
