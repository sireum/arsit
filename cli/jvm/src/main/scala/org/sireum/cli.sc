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

import org.sireum._
  import org.sireum.cli.CliOpt._

val arsitTool: Tool = Tool(
  name = "arsit",
  command = "arsit",
  description = "Generate Slang project from Aadl IR",
  header = "Slang Generator",
  usage = "<option>* air-file",
  opts = ISZ(
    Opt(name = "json", longKey = "json", shortKey = Some('j'),
      tpe = Type.Flag(F), description = "Input serialized using Json (otherwise MsgPack assumed)"),

    Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
      tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated project files"),

    Opt(name = "packageName", longKey = "package-name", shortKey = None(),
      tpe = Type.Str(sep = None(), default = None()), description = "Base package name for Slang project (output-dir's simple name used if not provided)"),

    Opt(name = "noart", longKey = "noart", shortKey = None(),
      tpe = Type.Flag(F), description = "Do not embed ART project files"),

    Opt(name = "bless", longKey = "bless", shortKey = None(),
      tpe = Type.Flag(F), description = "Generate Bless entrypoints"),

    Opt(name = "verbose", longKey = "verbose", shortKey = None(),
      tpe = Type.Flag(F), description = "Enable verbose mode")

  ),
  groups = ISZ(
    OptGroup(name = "Transpiler", opts = ISZ(
      Opt(name = "genTrans", longKey = "trans", shortKey = None(),
        tpe = Type.Flag(F), description = "Generate Slang/C code required for transpiler"),
      Opt(name = "ipc", longKey = "ipc", shortKey = None(),
        tpe = Type.Choice(name = "ipcmech", sep = None(), elements = ISZ("MessageQueue", "SharedMemory")),
        description = "IPC communication mechanism (requires 'trans' option)"),
      Opt(name = "excludeImpl", longKey = "exclude-impl", shortKey = None(),
        tpe = Type.Flag(F), description = "Exclude Slang component implementations"),
      Opt(name = "hamrTime", longKey = "hamr-time", shortKey = None(),
        tpe = Type.Flag(F), description = "HAMR build")
    ))
  )
)

println(org.sireum.cli.JSON.fromCliOpt(arsitTool, T))
