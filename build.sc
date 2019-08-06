/*
 Copyright (c) 2019, Jason Belt, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import mill._
import ammonite.ops._
import $file.runtime.Runtime
import $file.air.Air
import $file.Arsit
import $file.cli.Cli
import mill.scalalib.ScalaModule
import org.sireum.mill.SireumModule

object runtime extends mill.Module {

  object macros extends Runtime.Module.Macros

  object test extends Runtime.Module.Test {
    override def macrosObject = macros
  }

  trait testProvider extends Runtime.Module.TestProvider {
    override def testObject = test
  }

  object library extends Runtime.Module.Library with testProvider {
    override def macrosObject = macros
  }

  object bin extends ScalaModule {
    final override def scalaVersion = SireumModule.scalaVersion
    final override def moduleDeps = Seq(runtime.library.jvm)
  }
}

object air extends Air.Module with runtime.testProvider {
  final override def libraryObject = runtime.library
  final override def testObject = runtime.test
}


object arsit extends Arsit.Module {
  final override def airObject = air

  final override def millSourcePath = super.millSourcePath / up

  object bin extends ScalaModule {
    final override def scalaVersion = SireumModule.scalaVersion
    final override def moduleDeps = Seq(runtime.library.jvm)
  }
}


object cli extends Cli.Module {
  final override def arsitObject = arsit
}


def regenCli() = T.command {
  val out = pwd / 'bin / "sireum"
  val sireumPackagePath = pwd / 'cli / 'jvm / 'src / 'main / 'scala / 'org / 'sireum
  log(%%(out, 'tools, 'cligen, "-p", "org.sireum", "-l", pwd / "license.txt",
    sireumPackagePath / "cli.sc")(sireumPackagePath))
}

def tipe() = T.command {
  val out = pwd/ 'bin / "sireum"
  val paths = s"${pwd / 'jvm}:${pwd / 'cli}:${pwd / 'air}"
  log(%%(out, 'slang, 'tipe, "-s", paths)(pwd))
}

def refreshResources() = T.command {
  val libraryFile = Path(new java.io.File(pwd.toString, "jvm/src/main/scala/org/sireum/aadl/arsit/Library_Ext.scala").getCanonicalFile)

  def touche(p: Path): Unit = {
    val text = read ! p
    if (text.charAt(text.length - 1) == '\n') {
      write.over(p, text.trim)
    } else {
      write.over(p, text + '\n')
    }
  }

  touche(libraryFile)
}

private def log(r: CommandResult)(implicit ctx: mill.util.Ctx.Log): Unit = {
  val logger = ctx.log
  val out = r.out.string
  val err = r.err.string
  if (out.trim.nonEmpty) logger.info(out)
  if (err.trim.nonEmpty) logger.error(err)
  if (r.exitCode != 0) System.exit(r.exitCode)
}
