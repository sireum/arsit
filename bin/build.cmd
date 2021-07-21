::#! 2> /dev/null                                                                                           #
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
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                #
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
::!#
// #Sireum
import org.sireum._

def usage(): Unit = {
  println("Arsit /build")
  println("Usage: ( compile | test | clean | regen-clis)+")
}


if (Os.cliArgs.isEmpty) {
  usage()
  Os.exit(0)
}


val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireum : Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project4testing.cmd"

def clone(repo: String): Unit = {
  val clean = ops.StringOps(repo).replaceAllChars('-', '_')
  if (!(home / clean).exists) {
    Os.proc(ISZ("git", "clone", "--depth=1", s"https://github.com/sireum/$repo", clean)).at(home).console.runCheck()
  } else {
    Os.proc(ISZ("git", "pull")).at(home / clean).console.runCheck()
  }
  println()
}

def cloneProjects(): Unit = {
  /* Also clone hamr-codgen in order to get the 'common' object.  Kind of
 * strange as hamr-codgen has Arsit as a sub-module, though it isn't
 * recursively cloned
 */
  for (m <- ISZ("air", "hamr-codegen", "runtime")) {
    clone(m)
  }
}


def tipe(): Unit = {

  println("Slang type checking ...")
  val excludes = "jvm/src/test/results"
  Os.proc(ISZ(sireum.string, "slang", "tipe", "--verbose", "-r", "-x", excludes, "-s", home.string)).
    at(home).console.runCheck()
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


def clean(): Unit = {
  println(s"Cleaning ${home}")
  val homeResources: ISZ[Os.Path] = ISZ("air", "common", "hamr_codegen", "lib", "out", "runtime", "versions.properties").map(m => home / m)
  val homeBinResources: ISZ[Os.Path] = ISZ("sireum.jar", "sireum").map(m => homeBin / m)
  for(r <- (homeResources ++ homeBinResources) if r.exists) {
    println(s"Deleting ${r}")
    r.removeAll()
  }
}

def regenClis(): Unit = {
  val compileConfig = home / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "arsit" / "util" / "compileCli.sc"
  val destDir = home / "resources" / "util" / "compileCli.cmd"
  compileConfig.chmod("700")
  proc"$sireum tools cligen -s compileCli.cmd ${compileConfig}".at(destDir.up).console.run()
}

for (i <- 0 until Os.cliArgs.size) {
  Os.cliArgs(i) match {
    case string"clean" => clean()
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
