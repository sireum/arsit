// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.hamr.codegen.common.util.GclUtil

object GumboUtil {

  @record class LitImportResolver() extends org.sireum.hamr.ir.MTransformer {
    var imports: ISZ[ST] = ISZ()

    override def post_langastExpStringInterpolate(o: AST.Exp.StringInterpolate): MOption[AST.Exp] = {
      GclUtil.interpolatorLookup.get(o.prefix) match {
        case Some(e) => imports = imports :+ st"$e"
        case _ =>
      }
      return MNone()
    }
  }

  def resolveLitInterpolateImports(exp: AST.Exp): ISZ[ST] = {
    val r = LitImportResolver()
    r.transform_langastExp(exp)
    return r.imports
  }
}
