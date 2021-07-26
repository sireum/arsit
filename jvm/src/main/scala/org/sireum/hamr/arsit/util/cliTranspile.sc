// #Sireum
import org.sireum._
import org.sireum.cli.CliOpt._

val transpileTool: Tool = Tool(
  name = "transpile",
  command = "transpile",
  description = "Transpile Slang Embedded Programs",
  header = "Transpile Slang Embedded Programs",
  usage = "<option>*",
  usageDescOpt = None(),
  opts = ISZ(
    Opt(name = "scheduler", longKey = "scheduler", shortKey = Some('s'),
      tpe = Type.Choice(name = "choice", sep = None(), elements = ISZ("default", "roundrobin", "static", "legacy")),
      description = "The scheduler to use.  See Demo.scala for information on 'default'"
    )
  ),
  groups = ISZ()
)

println(org.sireum.cli.JSON.fromCliOpt(transpileTool, T))
