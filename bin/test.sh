#!/usr/bin/env bash
set -e
export TOOL_HOME=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )
. ${TOOL_HOME}/bin/prelude.sh
cd ${TOOL_HOME}
git submodule update --init --recursive --remote
export JAVA_HOME=${ACT_HOME}/bin/${PLATFORM}/jdk
export PATH=${JAVA_HOME}/bin:$PATH
${TOOL_HOME}/bin/mill all \
  cli.assembly \
  arsit.jvm.tests \
  cli.tests
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then
  cp out/cli/assembly/dest/out.jar bin/arsit.bat
else
  cp out/cli/assembly/dest/out.jar bin/arsit
  chmod +x bin/arsit
fi
