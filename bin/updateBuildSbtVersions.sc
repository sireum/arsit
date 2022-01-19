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
import org.sireum.project.DependencyManager

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / "sireum"
val versions = (SIREUM_HOME / "versions.properties").properties
val mill = SIREUM_HOME / "bin" / "mill"

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

val buildSbtProps = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "util" / "buildSbt.properties"
println(s"Updating $buildSbtProps")

var props: Map[String, String] = buildSbtProps.properties

// get most recent kekinian tag
val kekinianVersion: String = runGit(ISZ("git", "describe", "--abbrev=0", "--tags"), SIREUM_HOME)

val artVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr/codegen/art")
val artEmbeddedVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr/codegen/arsit/resources/art")
val scalaVersion = versions.get("org.scala-lang%scala-library%").get
val scalacPluginVersion = versions.get("org.sireum%%scalac-plugin%").get
val scalaTestVersion = versions.get("org.scalatest%%scalatest%%").get

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
  val library = s"org.sireum.kekinian::library:$kekinianVersion"
  val art = s"org.sireum.slang-embedded-art::slang-embedded-art:$artVersion"

  val scalaVer = versions.get("org.scala-lang%scala-library%").get
  val sc = Os.tempFix("", ".sc")
  sc.writeOver(
    st"""import org.sireum._
        |Coursier.setScalaVersion("$scalaVer")
        |for (cif <- Coursier.fetch(ISZ(s"$library", s"$art"))) {
        |  println(cif.path)
        |}""".render
  )
  sc.removeOnExit()
  println("Jitpacking ...")
  proc"$sireum slang run $sc".console.runCheck()
}

jitpack()

var updated = update("art.version", artVersion)
updated |= update("org.sireum.kekinian.version", kekinianVersion)
updated |= update("org.sireum.version.scala", scalaVersion)
updated |= update("org.sireum.version.scalac-plugin", scalacPluginVersion)
updated |= update("org.sireum.version.scalatest", scalaTestVersion)

def touchePath(path: Os.Path): Unit = {
  val content = path.read
  val lineSep: String = if (Os.isWin) "\r\n" else "\n"
  val sops = ops.StringOps(content)
  val newContent: String =
    if (sops.endsWith(lineSep)) ops.StringOps(content).trim
    else s"$content$lineSep"
  path.writeOver(newContent)
  println(s"Touched ${path}")
}

if(updated) {
  val artFiles = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "arsit" / "util" / "ArsitLibrary_Ext.scala"

  val pst = st"""${(props.entries.map(m => st"${m._1}=${m._2}"), "\n")}""".render
  buildSbtProps.writeOver(pst)
  println(s"$buildSbtProps updated")
  
  //println("\nRunning bin/build.cmd")
  touchePath(artFiles)
  //val build_cmd = SIREUM_HOME / "bin" / "build.cmd"
  //Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value)).console.runCheck()
  touchePath(artFiles)

  //println(s"\n$buildSbtProps updated and Sireum touched/rebuilt -- expect an error to follow")
  println(s"\n$buildSbtProps updated -- expect an error to follow")

  Os.exit(1) // return 1 to indicate versions have changed
} else {
  println(s"No Arsit updates needed")
  Os.exit(0)
}

