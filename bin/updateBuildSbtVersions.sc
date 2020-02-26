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
exec ${SIREUM_HOME}/bin/sireum slang run -s -n "$0" "$@"      #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run -s -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}




val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin/sireum"
val sireumProps = (SIREUM_HOME / "versions.properties").properties

val buildSbtProps = SIREUM_HOME / "hamr/codegen/arsit/resources/util/buildSbt.properties"
println(s"Updating $buildSbtProps")

var props: Map[String, String] = buildSbtProps.properties

val sireumVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME)
val sireumTimestamp = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%y%m%d%H%M"), SIREUM_HOME)

val sireumBuildstamp = ops.StringOps(Os.proc(ISZ(sireum.value)).run.out).split(c => c =='\n')(2) // should be 3rd line

val runtimeVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "runtime")
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

var updated = update("art.version", artVersion)
updated |= update("org.sireum.runtime.version", runtimeVersion)
updated |= update("org.sireum.version", sireumVersion)
updated |= update("org.sireum.buildstamp", sireumBuildstamp)
updated |= update("org.sireum.timestamp", sireumTimestamp)
updated |= update("org.sireum.version.scala", scalaVersion)
updated |= update("org.sireum.version.scalac-plugin", scalacPluginVersion)
updated |= update("org.sireum.version.scalatest", scalaTestVersion)

if(updated) {
  val pst = st"""${(props.entries.map(m => st"${m._1}=${m._2}"), "\n")}""".render
  buildSbtProps.writeOver(pst)
  println(s"$buildSbtProps updated")
  Os.exit(1) // return 1 to indicate versions have changed so probably need to rebuild before releasing a plugin
} else {
  println(s"No updates needed")
  Os.exit(0)
}

