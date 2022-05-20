// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.lang.{ast => AST}

object GumboUtil {

  @record class LitImportResolver() extends org.sireum.hamr.ir.MTransformer {
    var imports: ISZ[ST] = ISZ()

    override def post_langastExpStringInterpolate(o: AST.Exp.StringInterpolate): MOption[AST.Exp] = {
      o.prefix match {
        case "s8" => imports = imports :+ st"org.sireum.S8._"
        case "s16" => imports = imports :+ st"org.sireum.S16._"
        case "s32" => imports = imports :+ st"org.sireum.S32._"
        case "s64" => imports = imports :+ st"org.sireum.S64._"
        case "u8" => imports = imports :+ st"org.sireum.U8._"
        case "u16" => imports = imports :+ st"org.sireum.U16._"
        case "u32" => imports = imports :+ st"org.sireum.U32._"
        case "u64" => imports = imports :+ st"org.sireum.U64._"
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
