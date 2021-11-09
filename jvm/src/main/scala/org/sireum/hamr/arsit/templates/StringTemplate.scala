// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._

object StringTemplate {

  def doNotEditComment(from: Option[String]): String = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return s"// This file was auto-generated${_from}.  Do not edit"
  }

  def safeToEditComment(): String = {
    return "// This file will not be overwritten so is safe to edit"
  }
}
