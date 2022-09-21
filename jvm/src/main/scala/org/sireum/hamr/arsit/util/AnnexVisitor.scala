// #Sireum
package org.sireum.hamr.arsit.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, AnnexClauseInfo}
import org.sireum.message.Reporter

@sig trait AnnexVisitor {

  // called while generating code for threads allowing visitors to supply their
  // own behavior code.  If the return is non-empty for a given thread
  // then codegen will not generate behavior code
  def offerThread(component: AadlThreadOrDevice,
                  resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                  suggestedFilename: String,
                  componentDirectory: ISZ[String],
                  reporter: Reporter): ISZ[Resource]
}
