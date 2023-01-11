// #Sireum

import org.sireum._
import org.sireum.cli.CliOpt._

val compileTool: Tool = Tool(
  name = "compile",
  command = "compile",
  description = "Compile Slang Embedded Programs",
  header = "Compile Slang Embedded Programs",
  usage = "<option>*",
  usageDescOpt = None(),
  opts = ISZ(
    Opt(name = "boundCheck", longKey = "bound-check", shortKey = Some('b'),
      tpe = Type.Flag(F), description = "Build the program with sequence bound checking"),
    Opt(name = "noPrint", longKey = "no-print", shortKey = Some('p'),
      tpe = Type.Flag(F), description = "Build the program without console output"),
    Opt(name = "rangeCheck", longKey = "range-check", shortKey = Some('r'),
      tpe = Type.Flag(F), description = "Build the program with range checking"),
    Opt(name = "withLoc", longKey = "with-loc", shortKey = Some('l'),
      tpe = Type.Flag(F), description = "Build the program with Slang location info"),
    Opt(name = "jobs", longKey = "jobs", shortKey = Some('j'),
      tpe = Type.Num(None(), 4, Some(1), None()), description = "Number of make jobs to run in parallel"),
    Opt(name = "build", longKey = "build-type", shortKey = Some('t'),
      tpe = Type.Choice(name = "choice", sep = None(), elements = ISZ("release", "debug")),
      description = "Build type"),
    Opt(name = "verbose", longKey = "verbose", shortKey = Some('v'),
      tpe = Type.Flag(F), description = "Echo cmake command")
  ),
  groups = ISZ()
)

println(org.sireum.cli.JSON.fromCliOpt(compileTool, T))
