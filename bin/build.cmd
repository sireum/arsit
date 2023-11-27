::/*#! 2> /dev/null                                                                                         #
@ 2>/dev/null # 2>nul & echo off & goto BOF                                                                 #
export SIREUM_HOME=$(cd -P $(dirname "$0")/.. && pwd -P)                                                    #
if [ ! -z ${SIREUM_PROVIDED_SCALA++} ]; then                                                                #
  SIREUM_PROVIDED_JAVA=true                                                                                 #
fi                                                                                                          #
"${SIREUM_HOME}/bin/init.sh"                                                                                #
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then                                                                 #
  export SIREUM_HOME=$(cygpath -C OEM -w -a ${SIREUM_HOME})                                                 #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/win/java":"${SIREUM_HOME}/bin/win/z3":"$PATH"                           #
    export PATH="$(cygpath -C OEM -w -a ${JAVA_HOME}/bin)":"$(cygpath -C OEM -w -a ${Z3_HOME}/bin)":"$PATH" #
  fi                                                                                                        #
elif [ "$(uname)" = "Darwin" ]; then                                                                        #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/mac/java/bin":"${SIREUM_HOME}/bin/mac/z3/bin":"$PATH"                   #
  fi                                                                                                        #
elif [ "$(expr substr $(uname -s) 1 5)" = "Linux" ]; then                                                   #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    if [ "$(uname -m)" = "aarch64" ]; then                                                                  #
      export PATH="${SIREUM_HOME}/bin/linux/arm/java/bin":"$PATH"                                           #
    else                                                                                                    #
      export PATH="${SIREUM_HOME}/bin/linux/java/bin":"${SIREUM_HOME}/bin/linux/z3/bin":"$PATH"             #
    fi                                                                                                      #
  fi                                                                                                        #
fi                                                                                                          #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then                                                           #
  exec "$0.com" "$@"                                                                                        #
else                                                                                                        #
  rm -fR "$0.com"                                                                                           #
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                   #
fi                                                                                                          #
:BOF
setlocal
set SIREUM_HOME=%~dp0../
call "%~dp0init.bat"
if defined SIREUM_PROVIDED_SCALA set SIREUM_PROVIDED_JAVA=true
if not defined SIREUM_PROVIDED_JAVA set PATH=%~dp0win\java\bin;%~dp0win\z3\bin;%PATH%
set NEWER=False
if exist %~dpnx0.com for /f %%i in ('powershell -noprofile -executionpolicy bypass -command "(Get-Item %~dpnx0.com).LastWriteTime -gt (Get-Item %~dpnx0).LastWriteTime"') do @set NEWER=%%i
if "%NEWER%" == "True" goto native
del "%~dpnx0.com" > nul 2>&1
"%~dp0sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
:native
%~dpnx0.com %*
exit /B %errorlevel%
::!#*/
// #Sireum
import org.sireum._

def usage(): Unit = {
  println("Arsit /build")
    println("Usage: ( compile | test | regen-clis)+")
}


if (Os.cliArgs.isEmpty) {
  usage()
  Os.exit(0)
}


val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireum : Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project-standalone.cmd"


def clone(repo: String, proj: String, location: Option[String]): B = {
  val loc: Os.Path = location match {
    case Some(l) => home / l
    case _ => home / proj
  }
  val ret: B = if (!loc.exists) {
    val args = ISZ[String]("git", "clone", "--recurse", s"${repo}/$proj") ++ (if (location.nonEmpty) ISZ(location.get) else ISZ[String]())
    Os.proc(args).at(home).console.timeout(10000).run().ok
  } else {
    Os.proc(ISZ("git", "pull")).at(loc).console.run().ok
  }
  return ret
}

def cloneProjects(): Unit = {
  /* Also clone hamr-codgen in order to get the 'common' object.  Kind of
 * strange as hamr-codgen has Arsit as a sub-module, though it isn't
 * recursively cloned
 */
  for (m <- ISZ("air", "hamr-codegen", "hamr-sysml", "parser", "runtime", "slang")) {
    clone("https://github.com/sireum", m, None())
    println()
  }
  (home / "hamr-sysml").moveOverTo(home / "sysml")
}


def tipe(): Unit = {

  println("Slang type checking ...")
  val excludes = "jvm/src/test/results"
  proc"$sireum proyek tipe --project ${project.string} --par --strict-aliasing -r -x ${excludes}".at(home).console.runCheck()
  println()
}


def compile(): Unit = {
  tipe()

  println("Compiling ...")
  proc"$sireum proyek compile --project $project -n $proyekName --par --sha3 .".at(home).console.runCheck()
  println()
}


def test(): Unit = {
  tipe()

  val packageNames: String = "org.sireum.hamr.arsit"
  val names: String = "org.sireum.hamr.arsit"

  println("Testing ...")
  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 --packages $packageNames . $names".at(home).console.runCheck()
  println()
}


def regenClis(): Unit = {
  val utilDir = home / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "arsit" / "util"
  val compileConfig =  utilDir / "cliCompile.sc"
  val runConfig = utilDir / "cliRun.sc"
  val transpileConfig = utilDir / "cliTranspile.sc"
  val transpileAltConfig = utilDir / "cliTranspile-alt.sc"
  val destDir = home / "resources" / "util"
  compileConfig.chmod("700")
  runConfig.chmod("700")
  transpileConfig.chmod("700")
  transpileAltConfig.chmod("700")
  proc"$sireum tools cligen -s cliCompile.cmd ${compileConfig}".at(destDir).console.run()
  proc"$sireum tools cligen -s cliRun.cmd ${runConfig}".at(destDir).console.run()
  proc"$sireum tools cligen -s cliTranspile.cmd ${transpileConfig}".at(destDir).console.run()
  proc"$sireum tools cligen -s cliTranspile-alt.cmd ${transpileAltConfig}".at(destDir).console.run()
}

def installToolsViaKekinian(): Unit = {
  val builtIn = home / "runtime" / "library" / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "BuiltInTypes.slang"
  if(!builtIn.exists) {
    builtIn.write(".")
  }
  val kbuild = homeBin / "kbuild.cmd"
  kbuild.downloadFrom("https://raw.githubusercontent.com/sireum/kekinian/master/bin/build.cmd")
  proc"$sireum slang run $kbuild --help".at(homeBin).runCheck()
  kbuild.remove()
  if(builtIn.size == 1) {
    (home / "runtime").removeAll()
  }
}

installToolsViaKekinian()


for (i <- 0 until Os.cliArgs.size) {
  Os.cliArgs(i) match {
    case string"compile" =>
      cloneProjects()
      compile()
    case string"test" =>
      cloneProjects()
      test()
    case string"regen-clis" =>
      regenClis()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}
