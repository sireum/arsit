// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{CTranspilerOption, SlangUtil}

object TranspilerTemplate {

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  @pure def compileLibPreamble(entries: ISZ[ST]): ST = {
    return st"""#!/usr/bin/env bash
               |#
               |# This file is autogenerated.  Do not edit
               |#
               |set -e
               |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
               |
               |${(entries, "\n\n")}
               |"""
  }
  
  @pure def compileLib(childDir: String): ST = {
    val script_home = "${SCRIPT_HOME}"
    return st"""cd "${script_home}/${childDir}"
               |mkdir -p sel4-build
               |cd sel4-build
               |cmake ..
               |make $$MAKE_ARGS
               |"""
  }

  @pure def transpiler(sourcepaths: ISZ[String],
                       outputDir: Os.Path,
                       binDir: String,
                       apps: ISZ[String],
                       forwards: ISZ[String],
                       numBits: Z,
                       maxSequenceSize: Z,
                       maxStringSize: Z,
                       customArraySizes: ISZ[String],
                       customConstants: ISZ[String],
                       stackSizeInBytes: Z,
                       extensions: ISZ[String],
                       excludes: ISZ[String],
                       buildApps: B,
                       additionalInstructions: Option[ST]): (ST, CTranspilerOption) = {

    val _stackSizeInBytes: String = if(stackSizeInBytes < 0) {
      "16*1024*1024" // default set in org.sireum.transpilers.cli.cTranspiler
    } else {
      stackSizeInBytes.string
    }

    val transpilerOptions =
      CTranspilerOption(
        sourcepath = sourcepaths,
        output = Some(outputDir.value),
        verbose = T,
        projectName = Some("main"),  // default set in org.sireum.transpilers.cli.cTranspiler
        apps = apps,
        unroll = F, // default set in org.sireum.transpilers.cli.cTranspiler
        fingerprint = 3, // default set in org.sireum.transpilers.cli.cTranspiler
        bitWidth = numBits,
        maxStringSize = maxStringSize,
        maxArraySize = maxSequenceSize,
        customArraySizes = customArraySizes,
        customConstants = customConstants,
        plugins = ISZ(),
        exts = extensions,
        forwarding = forwards,
        stackSize = Some(_stackSizeInBytes),
        excludeBuild = excludes,
        libOnly = !buildApps,
        stableTypeId = T,
        save = None(),
        load = None()
      )
    val st = transpilerX(transpilerOptions, binDir, additionalInstructions)

    return (st, transpilerOptions)
  }

  @pure def transpilerX(opts: CTranspilerOption,
                        binDir: String,
                        additionalInstructions: Option[ST]): ST = {

    val script_home = s"$${${SCRIPT_HOME}}"

    val projHomesRel = opts.sourcepath.map(s => SlangUtil.relativizePaths(binDir, s, script_home))
    val cOutputDirRel = SlangUtil.relativizePaths(binDir, opts.output.get, script_home)

    val path_sep = s"$${PATH_SEP}"

    def expand(optionName: String, elems: ISZ[String]): Option[ST] = {
      return if(elems.nonEmpty) Some(st"""--${optionName} "${(elems, ";")}" \""") 
      else None()
    }

    val ret = st"""OUTPUT_DIR="${cOutputDirRel}"
                  |
                  |$${SIREUM_HOME}/bin/sireum slang transpilers c \
                  |  --sourcepath "${(projHomesRel, path_sep)}" \
                  |  --output-dir "$${OUTPUT_DIR}" \
                  |  --name "${opts.projectName.get}" \
                  |  --apps "${(opts.apps, ",")}" \
                  |  --fingerprint ${opts.fingerprint} \
                  |  --bits ${opts.bitWidth} \
                  |  --string-size ${opts.maxStringSize} \
                  |  --sequence-size ${opts.maxArraySize} \
                  |  ${expand("sequence", opts.customArraySizes)}
                  |  ${expand("constants", opts.customConstants)}
                  |  --forward "${(opts.forwarding, ",")}" \
                  |  --stack-size "${opts.stackSize.get}" \
                  |  --stable-type-id"""

    var extras: ISZ[ST] = ISZ()

    if(opts.exts.nonEmpty) {
      val extsRel = opts.exts.map(s => SlangUtil.relativizePaths(binDir, s, script_home))
      extras = extras :+ st""" \
                             |  --exts "${(extsRel, path_sep)}""""
    }

    if(opts.excludeBuild.nonEmpty) {
      extras = extras :+ st""" \
                             |  --exclude-build "${(opts.excludeBuild, ",")}""""
    }

    if(opts.libOnly) {
      extras = extras :+ st""" \
                             |  --lib-only"""
    }

    if(opts.verbose) {
      extras = extras :+ st""" \
                             |  --verbose"""
    }

    if(additionalInstructions.nonEmpty) {
      extras = extras :+ st"""
                             |
                             |${additionalInstructions}"""
    }

    return st"""${ret}${(extras, "")}"""
  }
  
  def transpilerScriptPreamble(entries: ISZ[ST]): ST = {
    val ret: ST = st"""#!/usr/bin/env bash
                      |#
                      |# This file is autogenerated.  Do not edit
                      |#
                      |set -e
                      |
                      |if [ -z "$${SIREUM_HOME}" ]; then
                      |  echo "SIREUM_HOME not set. Refer to https://github.com/sireum/kekinian/#installing"
                      |  exit 1
                      |fi
                      |
                      |${SCRIPT_HOME}=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
                      |
                      |PATH_SEP=":"
                      |if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then
                      |  PATH_SEP=";"
                      |fi
                      |
                      |${(entries,"\n\n")}"""
    return ret
  }
}
