// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._

object BlessST {

  @pure def main(packageName: String,
                 imports: ISZ[ST],
                 states: ISZ[ST],
                 componentName: String,
                 bridgeName: String,
                 initialState: String,
                 localVars: ISZ[ST],
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
               |@enum object CompleteState {
               |  ${(states, "\n")}
               |}
               |
               |@record class ${componentName}(api: ${bridgeName}.Api) {
               |
               |  var currentState : CompleteState.Type = CompleteState.${initialState}
               |
               |  ${if(localVars.isEmpty) "// no local vars" else (localVars, "\n")}
               |
               |  ${(methods, "\n\n")}
               |}
               |
               |${(extensions, "\n")}
               |"""
  }

  @pure def method(name: String,
                   body: ST): ST = {
    return st"""def ${name}_Entrypoint(): Unit = {
               |  ${body}
               |}"""
  }

  @pure def ifST(ifbranch: (ST, ST), elsifs: ISZ[(ST, ST)]): ST = {
    val e = elsifs.map(x => st"""else if(${x._1}) {
                                |  ${x._2}
                                |}""")
    return st"""if(${ifbranch._1}) {
               |  ${ifbranch._2}
               |} ${e}"""
  }
  @pure def typeDec(varName: String,
                    varType: String): ST = {
    return st"var ${varName}: ${varType}"
  }
}