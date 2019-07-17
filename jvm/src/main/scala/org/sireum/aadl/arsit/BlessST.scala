// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._

object BlessST {

  @pure def main(packageName: String,
                 imports: ISZ[ST],
                 completeStateEnumName: String,
                 states: ISZ[ST],
                 componentName: String,
                 bridgeName: String,
                 initialState: String,
                 globalVars: ISZ[ST],
                 methods: ISZ[ST],
                 extensions: ISZ[ST]
                ): ST = {
    return st"""// #Sireum
               |
               |package $packageName
               |
               |import org.sireum._
               |${(imports, "\n")}
               |
               |@enum object ${completeStateEnumName} {
               |  ${(states, "\n")}
               |}
               |
               |@record class ${componentName}(api: ${bridgeName}.Api) {
               |
               |  var currentState : ${completeStateEnumName}.Type = ${completeStateEnumName}.${initialState}
               |
               |  ${if(globalVars.isEmpty) "// no local vars" else st"${(globalVars, "\n")}" }
               |
               |  ${(methods, "\n\n")}
               |}
               |
               |${(extensions, "\n")}
               |"""
  }

  @pure def method(name: ST,
                   params: ISZ[ST],
                   body: ST,
                   retType: ST): ST = {
    return st"""def ${name}(${params}): ${retType} = {
               |  ${body}
               |}"""
  }


  @pure def externalObject(name: ST,
                           methods: ISZ[ST]) : ST = {
    return st"""@ext object ${name} {
               |  ${(methods, "\n\n")}
               |}"""
  }

  @pure def extMethod(name: ST,
                      params: ISZ[ST],
                      retType: ST): ST = {
    return st"def ${name}(${params}): ${retType} = $$"
  }

  @pure def ifST(ifbranch: (ST, ST), elsifs: ISZ[(ST, ST)]): ST = {
    val e = elsifs.map(x => st"""else if(${x._1}) {
                                |  ${x._2}
                                |}
                                |""")
    return st"""if(${ifbranch._1}) {
               |  ${ifbranch._2}
               |}
               |${e}"""
  }

  @pure def variableDec(varName: String,
                        varType: ST,
                        defaultValue: ST): ST = {
    return st"var ${varName}: ${varType} = ${defaultValue}"
  }

  @pure def portSend(portName: String,
                     arg: ST): ST = {
    return st"api.send${portName}(${arg})"
  }

  @pure def portFetch(portName: String): ST = {
    return st"api.get${portName}()"
  }

  @pure def portGet(portName: String): ST = {
    return st"${portFetch(portName)}.get"
  }

  @pure def portQuery(portName: String): ST = {
    return st"${portFetch(portName)}.nonEmpty"
  }
}