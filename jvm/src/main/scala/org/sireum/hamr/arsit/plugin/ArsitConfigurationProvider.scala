// #Sireum

package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.N._
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.codegen.common.CommonUtil.toolName
import org.sireum.hamr.codegen.common.plugin.Plugin

object ArsitConfigurationProvider {
  @strictpure def getArsitConfigurationProviderPlugins(plugins: MSZ[Plugin]): MSZ[ArsitConfigurationProvider] =
    plugins.filter(f => f.isInstanceOf[ArsitConfigurationProvider]).map(m => m.asInstanceOf[ArsitConfigurationProvider])

  @pure def getAdditionalPortIds(cliOpt: N, plugins: MSZ[Plugin]): N = {
    var maxId: N = cliOpt
    var contributor: String = ""
    for (p <- getArsitConfigurationProviderPlugins(plugins) if p.addPortIds > maxId) {
      maxId = p.addPortIds
      contributor = p.name
    }
    if (maxId > n"0") {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max port id increment value of $maxId")
    }
    return maxId
  }

  @pure def getAdditionalComponentIds(cliOpt: N, plugins: MSZ[Plugin]): N = {
    var maxId: N = cliOpt
    var contributor: String = ""
    for (p <- getArsitConfigurationProviderPlugins(plugins) if p.addComponentIds > maxId) {
      maxId = p.addComponentIds
      contributor = p.name
    }
    if (maxId > n"0") {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max component id increment value of $maxId")
    }
    return maxId
  }

  @pure def getAdditionalConnectionIds(cliOpt: N, plugins: MSZ[Plugin]): N = {
    var maxId: N = cliOpt
    var contributor: String = ""
    for (p <- getArsitConfigurationProviderPlugins(plugins) if p.addConnectionIds > maxId) {
      maxId = p.addConnectionIds
      contributor = p.name
    }
    if (maxId > n"0") {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max connection id increment value of $maxId")
    }
    return maxId
  }
}

@msig trait ArsitConfigurationProvider extends ArsitPlugin {

  @strictpure def addPortIds: N = n"0"

  @strictpure def addComponentIds: N = n"0"

  @strictpure def addConnectionIds: N = n"0"
}
