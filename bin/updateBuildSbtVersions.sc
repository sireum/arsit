::#! 2> /dev/null                                             #
@ 2>/dev/null # 2>nul & echo off & goto BOF                   #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then             #
  exec "$0.com" "$@"                                          #
fi                                                            #
rm -f "$0.com"                                                #
if [ -z ${SIREUM_HOME} ]; then                                #
  echo "Please set SIREUM_HOME env var"                       #
  exit -1                                                     #
fi                                                            #
exec ${SIREUM_HOME}/bin/sireum slang run -n "$0" "$@"      #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin/sireum"
val sireumProps = (SIREUM_HOME / "versions.properties").properties
val mill = SIREUM_HOME / "bin/mill"

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

val buildSbtProps = SIREUM_HOME / "hamr/codegen/arsit/resources/util/buildSbt.properties"
println(s"Updating $buildSbtProps")

var props: Map[String, String] = buildSbtProps.properties

// commit id of runtime
val runtimeVersion: String = ops.StringOps(runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%H"), SIREUM_HOME / "runtime")).substring(0, 10)

// kekinian commit id when runtime was last updated
val kekinianVersion: String = {
  val runtimeCommitDate = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%ct"), SIREUM_HOME / "runtime")
  val kekinianCommits = runGit(ISZ("git", "log", s"--since=${runtimeCommitDate}", "--pretty=format:%H"), SIREUM_HOME)
  ops.StringOps(ops.ISZOps(ops.StringOps(kekinianCommits).split(c => c == '\n')).last).substring(0, 10)
}

val artVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr/codegen/art")
val artEmbeddedVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr/codegen/arsit/resources/art")
val scalaVersion = sireumProps.get("org.sireum.version.scala").get
val scalacPluginVersion = sireumProps.get("org.sireum.version.scalac-plugin").get
val scalaTestVersion = sireumProps.get("org.sireum.version.scalatest").get

if(artVersion != artEmbeddedVersion) {
  for(i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  println(s"WARNING: ART versions do not match: ${artVersion} vs ${artEmbeddedVersion}")
  for(i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
}

def update(key: String, currentVersion: String): B = {
  val propsVersion = if(!props.contains(key)) "MISSING" else props.get(key).get  
  if(propsVersion != currentVersion) {
    println(s"Updating ${key}: ${propsVersion} -> ${currentVersion}")
    val escaped = ops.StringOps(currentVersion).replaceAllChars(' ', '\u0020')
    props = props + (key ~> escaped)
    return T
  } else {
    return F
  }
}

def jitpack(): Unit = {
  println(s"Triggering jitpack on https://github.com/sireum/kekinian/tree/${kekinianVersion} ...")
  val r = mill.call(ISZ("jitPack", "--owner", "sireum", "--repo", "kekinian", "--lib", "library", "--hash", kekinianVersion)).at(SIREUM_HOME).console.run()
  r match {
    case r: Os.Proc.Result.Normal =>
      println(r.out)
      println(r.err)
      if (!r.ok) {
        eprintln(s"Exit code: ${r.exitCode}")
      }
    case r: Os.Proc.Result.Exception =>
      eprintln(s"Exception: ${r.err}")
    case _: Os.Proc.Result.Timeout =>
      eprintln("Timeout")
      eprintln()
  }
  println()
}

jitpack()

var updated = update("art.version", artVersion)
updated |= update("org.sireum.kekinian.version", kekinianVersion)
updated |= update("org.sireum.runtime.version", runtimeVersion)
updated |= update("org.sireum.version.scala", scalaVersion)
updated |= update("org.sireum.version.scalac-plugin", scalacPluginVersion)
updated |= update("org.sireum.version.scalatest", scalaTestVersion)

if(updated) {
  val pst = st"""${(props.entries.map(m => st"${m._1}=${m._2}"), "\n")}""".render
  buildSbtProps.writeOver(pst)
  println(s"$buildSbtProps updated")
  
  println("\nRunning bin/build.cmd -- will touche Library_Ext.scala")
  val build_cmd = SIREUM_HOME / "bin/build.cmd"
  Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value)).console.runCheck()
    
  println(s"\n$buildSbtProps updated and Sireum touched/rebuilt -- expect an error to follow")
  
  Os.exit(1) // return 1 to indicate versions have changed
} else {
  println(s"No updates needed")
  Os.exit(0)
}

