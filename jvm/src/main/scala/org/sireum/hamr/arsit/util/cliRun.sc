// #Sireum

import org.sireum._
import org.sireum.cli.CliOpt._

val runTool: Tool = Tool(
  name = "run",
  command = "run",
  description = "Run Slang Embedded Program",
  header = "Run Slang Embedded Program",
  usage = "<option>*",
  usageDescOpt = None(),
  opts = ISZ(
    Opt(name = "scheduler", longKey = "scheduler", shortKey = Some('s'),
      tpe = Type.Choice(name = "choice", sep = None(), elements = ISZ("default", "roundRobin", "static", "legacy")),
      description = "The scheduler to use.  See Demo.scala for information on 'default'"
    )
  ),
  groups = ISZ()
)

println(org.sireum.cli.JSON.fromCliOpt(runTool, T))
