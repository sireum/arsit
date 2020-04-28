// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit.{DataTypeNames, Names}


object Sel4NamesUtil {
  def apiHelperMethodName(methodName: String, names: Names): String = {
    return s"api_${methodName}__${names.cComponentImplQualifiedName}"
  }

  @pure def getTypeFingerprints(slangTypeName: String) : (String, String, String) = {

    @pure def stableTypeSig(t: String, width: Z): String = {
      val max: Z = if (0 < width && width <= 64) width else 64
      val bytes = ops.ISZOps(crypto.SHA3.sum512(conversions.String.toU8is(t))).take(max)
      var cs = ISZ[C]()
      for (b <- bytes) {
        val c = conversions.U32.toC(conversions.U8.toU32(b))
        cs = cs :+ ops.COps.hex2c((c >>> '\u0004') & '\u000F')
        cs = cs :+ ops.COps.hex2c(c & '\u000F')
      }
      return st"$cs".render
    }
    
    val optionType = s"Option[${slangTypeName}]"
    val someType = s"Some[${slangTypeName}]"
    val noneType = s"None[${slangTypeName}]"

    val optionSig = s"Option_${stableTypeSig(optionType, 3)}"
    val someSig = s"Some_${stableTypeSig(someType, 3)}"
    val noneSig = s"None_${stableTypeSig(noneType, 3)}"

    return (optionSig, someSig, noneSig)
  }
}

object Sel4NixTemplate {

  def sendOutput(entries: ST): ST = {
    val ret: ST = st"""def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                      |  // ignore params
                      |    
                      |  ${entries}
                      |}"""
    return ret
  }

  def getValue(entries: ST): ST = {
    val ret: ST = st"""def getValue(portId: Art.PortId): Option[DataContent] = {
                      |  ${entries}
                      |}"""
    return ret
    
  }

  def putValue(entries: ST): ST = {
    val ret: ST = st"""def putValue(portId: Art.PortId, data: DataContent): Unit = {
                      |  ${entries}
                      |}"""
    return ret
  }

  def receiveInput(entries: ST): ST = {
    val ret: ST = st"""def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                      |  // ignore params
                      |  
                      |  ${entries}  
                      |}"""
    return ret
  }

  def portComment(portName: String,
                  dir: String,
                  portCategory: String,
                  portType: String): ST = {
    return st"${portName}: ${dir} ${portCategory} ${portType}"
  }
  
  def portVariable(bridgeIdentifier: String,
                   portVariableName: String,
                   archPortName: String,
                   portId: String,
                   portComment: ST): ST = {
    val ret: ST = st"""// ${portComment}
                      |val ${portId}: Art.PortId = ${bridgeIdentifier}.${archPortName}.id
                      |var ${portVariableName}: Option[DataContent] = noData"""
    return ret
  }
  
  def extensionObjectStub(packageName: String,
                          sel4ExtensionStubName: String,
                          entries: ST): ST = {
    val ret: ST = st"""package ${packageName}
                      |
                      |import org.sireum._
                      |import art._
                      |
                      |object ${sel4ExtensionStubName} {
                      |  ${entries}
                      |}
                      |"""
    return ret
  }
  
  def extensionObject(packageName: String,
                      sel4ExtensionName: String,
                      entries: ST): ST = {
    val ret: ST = st"""// #Sireum
                      |
                      |package ${packageName}
                      |
                      |import org.sireum._
                      |import art._
                      |
                      |@ext object ${sel4ExtensionName} {
                      |  ${entries}
                      |}
                      |"""
    return ret
  }
  
  def dispatchStatus(body: ST): ST = {
    val ret: ST = st"""def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
                      |  ${body}
                      |}"""
    return ret
  }

  def touchType(payloadName: String, typeName: Option[String]): ST = {
    return st"printDataContent(${payloadName}(${typeName}))"
  }
  
  def touchTypes(touches: ISZ[ST]): ST = {
    return st"""// touch each payload/type in case some are only used as a field in a record
               |def printDataContent(a: art.DataContent): Unit = { println(s"$${a}") }
               |
               |${(touches, "\n")}"""
  }
  
  def typeApp(packageName: String,
           instanceName: String,
           identifier: String,
           typeTouches: ISZ[ST]): ST = {
    val ret: ST = st"""// #Sireum
                      |
                      |package ${packageName}.${instanceName}
                      |
                      |import org.sireum._
                      |import art._
                      |import ${packageName}._
                      |
                      |object ${identifier} extends App {
                      |  def main(args: ISZ[String]): Z = {
                      |  
                      |    ${touchTypes(typeTouches)}
                      |    
                      |    return 0
                      |  }
                      |}
                      |"""
    return ret
  }
  
  def app(packageName: String,
          instanceName: String,
          imports: ISZ[String],
          identifier: String,
          bridge: ST, 
          bridgeIdentifier: String,
          dispatchStatus: ST,
          globals: ST,
          receiveInput: ST,
          getValue: ST,
          putValue: ST,
          sendOutput: ST,
          typeTouches: ISZ[ST]): ST = {
    val ret: ST = st"""// #Sireum
                   |
                   |package ${packageName}.${instanceName}
                   |
                   |import org.sireum._
                   |import art._
                   |import art.DispatchPropertyProtocol._
                   |import art.PortMode._
                   |${(imports, "\n")}
                   |
                   |object ${identifier} extends App {
                   |  
                   |  ${bridge}
                   |  
                   |  val entryPoints: Bridge.EntryPoints = ${bridgeIdentifier}.entryPoints
                   |  val noData: Option[DataContent] = None()
                   |  
                   |  ${globals}
                   |
                   |  ${dispatchStatus}
                   |
                   |  ${getValue}
                   |  
                   |  ${receiveInput}
                   |
                   |  ${putValue}
                   |  
                   |  ${sendOutput}
                   |
                   |  def initialiseArchitecture(): Unit = { 
                   |    val ad = ArchitectureDescription(
                   |      components = MSZ (${bridgeIdentifier}),
                   |      connections = ISZ ()
                   |    )   
                   |    Art.run(ad)
                   |  }
                   |
                   |  def initialiseEntryPoint(): Unit = { entryPoints.initialise() }
                   |  
                   |  def computeEntryPoint(): Unit = { entryPoints.compute() }
                   |  
                   |  def finaliseEntryPoint(): Unit = { entryPoints.finalise() }
                   |  
                   |  def main(args: ISZ[String]): Z = {
                   |
                   |    // need to touch the following for transpiler
                   |    initialiseArchitecture()
                   |    initialiseEntryPoint()
                   |    computeEntryPoint()
                   |    finaliseEntryPoint()
                   |
                   |    ${touchTypes(typeTouches)}
                   |    
                   |    return 0
                   |  }
                   |
                   |  def logInfo(title: String, msg: String): Unit = {
                   |    print(title)
                   |    print(": ")
                   |    println(msg)
                   |  }
                   |
                   |  def logError(title: String, msg: String): Unit = {
                   |    eprint(title)
                   |    eprint(": ")
                   |    eprintln(msg)
                   |  }
                   |
                   |  def logDebug(title: String, msg: String): Unit = {
                   |    print(title)
                   |    print(": ")
                   |    println(msg)
                   |  }
                   |  
                   |  def run(): Unit = {}
                   |
                   |}
                   |"""
    return ret
  }

  def methodSignature(methodName: String, preParams: Option[ST], params: ISZ[ST], returnType: String): ST = {
    val ret: ST = if(preParams.isEmpty && params.isEmpty) {
      st"${returnType} ${methodName}()"
    }
    else {
      st"""${returnType} ${methodName}(
          |  ${preParams}
          |  ${(params, ",\n")})"""
    }
    return ret
  }

  def apiGet(signature: ST,
             apiGetMethodName: String,
             c_this: String,
             typ: DataTypeNames): ST = {
    
    val qualifiedName: String = s"${if(typ.isAadlType()) s"${typ.basePackage}." else ""}${typ.qualifiedReferencedTypeName}" 
      
    val (optionSig, someSig, noneSig) = Sel4NamesUtil.getTypeFingerprints(qualifiedName)
        
    val typeAssign : Option[String] =  
      if(typ.isEmptyType()) { None[String]() }  
      else if(typ.isBaseType()) { Some(s"*value = t_0.${someSig}.value;") } 
      else {
        val struct: String = if(!typ.isEnum() && !typ.isBaseType()) s"struct " else ""
        Some(s"Type_assign(value, &t_0.${someSig}.value, sizeof(${struct}${typ.qualifiedCTypeName}));") 
      }
    
    val ret: ST = st"""${signature}{
                      |  // ${optionSig} = Option[${qualifiedName}]
                      |  // ${someSig} = Some[${qualifiedName}]
                      |  DeclNew${optionSig}(t_0);
                      |  ${apiGetMethodName}(
                      |    SF
                      |    (${optionSig}) &t_0,
                      |    ${c_this}(this));
                      |    
                      |  if(t_0.type == T${someSig}){
                      |    ${typeAssign}
                      |    return T;
                      |  } else {
                      |    return F;
                      |  }
                      |}"""
    return ret
  }

  def apiSet(signature: ST, apiGetMethodName: String, c_this: String, isEventPort: B): ST = {
    var args: ISZ[ST] = ISZ(st"${c_this}(this)")
    if(!isEventPort) { args = args :+ st"value" }
    
    val ret: ST =st"""${signature} {
                     |
                     |  ${apiGetMethodName}(
                     |    ${(args, ",\n")});
                     |}"""
    return ret
  }
  
  def apiLog(signature: ST, apiLogMethodName: String, c_this: String): ST = {
    var args: ISZ[ST] = ISZ(st"${c_this}(this)", st"str")
    
    val ret: ST = st"""${signature} {
                      |  ${apiLogMethodName}(
                      |    SF
                      |    ${(args, ",\n")});
                      |}"""
    return ret
  }

  def cHeaderFile(macroName: String,
                  headerMethods: ISZ[ST]): ST = {
    val ret: ST = st"""#ifndef ${macroName}
                      |#define ${macroName}
                      |
                      |#include <all.h>
                      |
                      |${(headerMethods, "\n\n")}
                      |
                      |#endif
                      |"""
    return ret
  }
  
  def cImplFile(fileName: String,
               implMethods: ISZ[ST]): ST = {
    val ret: ST = st"""#include <${fileName}.h>
                      |
                      |${(implMethods, "\n\n")}
                      |"""
    return ret
  }
  
  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if(options.size > 0) { Some(options(0)) } else { None() }
    val rest: ISZ[(ST, ST)] = if(options.size > 1) { org.sireum.ops.ISZOps(options).drop(1) } else { ISZ() }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {

    var body = st""

    if(ifbranch.nonEmpty) {
      body = st"""if(${ifbranch.get._1}) {
                 |  ${ifbranch.get._2}
                 |} """
    }

    if(elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) => st"""else if(${x._1}) {
                                               |  ${x._2}
                                               |} """)
      body = st"""${body}${ei}"""
    }

    if(els.nonEmpty) {
      if(ifbranch.nonEmpty) {
        body = st"""${body}else {
                   |  ${els.get}
                   |}"""
      } else {
        body = els.get
      }
    }

    return body
  }
}
