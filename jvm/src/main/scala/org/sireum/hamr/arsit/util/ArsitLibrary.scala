// #Sireum
package org.sireum.hamr.arsit.util

import org.sireum._

@ext object ArsitLibrary {
  def getFiles: ISZ[(String, String)] = $

  def getCompileCli: String = $

  def getArtVersion(): String = $

  def getKekinianVersion(): String = $

  def getSireumScalacVersionVersion(): String = $

  def getScalaVersion(): String = $

  def getScalaTestVersion(): String = $

  def getSBTVersion(): String = $

  def getSbtAssemblyVersion(): String = $

  def getInspectorVersion(): String = $

  def getFormsRtVersion(): String = $
}
