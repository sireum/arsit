::#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF         #
if [ -z ${SIREUM_HOME} ]; then                      #
  echo "Please set SIREUM_HOME env var"             #
  exit -1                                           #
fi                                                  #
exec ${SIREUM_HOME}/bin/sireum slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.Project

val common = "hamr-common"

val arsit = "hamr-arsit"

val homeDir = Os.slashDir.up.canon

val arsitJvm = moduleJvmPub(
  id = arsit,
  baseDir = homeDir,
  jvmDeps = ISZ(common),
  jvmIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "HAMR AADL Slang Code Generator (Arsit)",
    url = "github.com/sireum/arsit",
    licenses = bsd2,
    devs = ISZ(jasonBelt)
  )
)

val project = Project.empty + arsitJvm

projectCli(Os.cliArgs, project)