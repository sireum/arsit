// #Sireum
package org.sireum.hamr.arsit.bts

import org.sireum._
import org.sireum.hamr.arsit.util.AnnexVisitor
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, AnnexClauseInfo}
import org.sireum.hamr.ir._
import org.sireum.message.Reporter


@datatype class BlessAnnexVisitor extends AnnexVisitor {

  override def offerThread(component: AadlThreadOrDevice,
                           resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                           filename: String,
                           componentDirectory: ISZ[String],
                           reporter: Reporter): ISZ[Resource] = {

    val behaviorProviders: ISZ[BTSSubclauseBehaviorProvider] = component.annexes().filter((p: Annex) =>
      p.clause.isInstanceOf[BTSSubclauseBehaviorProvider]).map((m: Annex) =>
      m.clause.asInstanceOf[BTSSubclauseBehaviorProvider])

    val ret: ISZ[Resource] = behaviorProviders.flatMap((m: BTSSubclauseBehaviorProvider) =>
      m.values.map((r: BTSResource) =>
        r match {
          case t: BTSText =>
            IResource(
              dstPath = filename,
              content = st"${t.source}",
              markers = ISZ(),
              overwrite = t.overwrite,
              makeExecutable = F,
              makeCRLF = F)

          case p: BTSPath =>
            EResource(
              srcPath = p.path,
              dstPath = filename,
              symlink = T)
        }
      ))

    return ret
  }
}
