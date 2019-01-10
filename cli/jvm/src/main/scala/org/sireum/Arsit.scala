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

package org.sireum

import java.io.File
import org.sireum.aadl.arsit.Cli.{ArsitOption => xArsitOption}
object Arsit extends scala.App {

  Cli(File.pathSeparatorChar).parseArsit(ISZ(args.toSeq.map(s => s: String):_ *), 0) match {
    case Some(o: Cli.ArsitOption) => arsit(o)
    case Some(_: Cli.HelpOption) => 1
    case _ => -1
  }

  def arsit(o : Cli.ArsitOption): Int = {
    o.args.size match {
      case z"1" =>
      case _ => println(o.help); return 0
    }

    val ipc = o.ipc match {
      case Cli.Ipcmech.MessageQueue => org.sireum.aadl.arsit.Cli.Ipcmech.MessageQueue
      case Cli.Ipcmech.SharedMemory => org.sireum.aadl.arsit.Cli.Ipcmech.SharedMemory
    }

    org.sireum.aadl.arsit.Arsit.run(xArsitOption(o.help, o.args, o.json, o.outputDir, o.packageName, o.noart, o.bless, o.genTrans, ipc))
  }
}