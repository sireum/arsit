// #Sireum
import org.sireum._
import org.sireum.cli.CliOpt._

val transpileAltTool: Tool = Tool(
  name = "transpile",
  command = "transpile",
  description = "Transpile Slang Embedded Program",
  header = "Transpile Slang Embedded Program",
  usage = "<option>*",
  usageDescOpt = None(),
  opts = ISZ(
    Opt(name = "legacy", longKey = "legacy", shortKey = Some('l'),
      tpe = Type.Flag(F),
      description = "Use legacy scheduler"
    )
  ),
  groups = ISZ()
)

println(org.sireum.cli.JSON.fromCliOpt(transpileAltTool, T))
