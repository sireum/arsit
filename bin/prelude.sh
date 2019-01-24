#!/usr/bin/env bash
set -e

: ${TOOL_HOME:=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )}
BIN_DIR=${TOOL_HOME}/bin
ZULU_VERSION=10.3+5-jdk10.0.2
MILL_URL=http://files.sireum.org/mill-standalone # see https://github.com/sireum/mill-build
MILL_SHA3=82549d532d9829b2aae29b10ea5bdf0bb92f106db3568ab5edd16fbc7f82e439
MILL=${BIN_DIR}/mill
LIB_URL=https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
LIB_SHA3=9ff4de90272023191c85b8c0cb04eab6f19e7de395096f8baeeb8b696b4cdee5
LIB=${TOOL_HOME}/versions.properties
SIREUM_URL=http://files.sireum.org/sireum # see https://github.com/sireum/kekinian
SIREUM_SHA3=032328751b66a03fe0fd201c8dbcf999825f32c822be3f8f77718574b871135b
CURL="curl -c /dev/null -Lo"
if [ -z "${PLATFORM}" ]; then
  if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then
    PLATFORM=win
  elif [ "$(uname)" == "Darwin" ]; then
    PLATFORM=mac
  elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    PLATFORM=linux
  else
    >&2 echo "Unsupported platform to build TOOL."
    exit 1
  fi
fi
if [ "${PLATFORM}" = "win" ]; then
  ZULU_URL=http://cdn.azul.com/zulu/bin/zulu${ZULU_VERSION}-win_x64.zip
  SHA3=${BIN_DIR}/win/sha3.exe
  SHA3_URL=http://files.sireum.org/win/sha3.exe
  SIREUM=${BIN_DIR}/sireum.bat
elif [ "${PLATFORM}" = "mac" ]; then
  ZULU_URL=http://cdn.azul.com/zulu/bin/zulu${ZULU_VERSION}-macosx_x64.zip
  SHA3=${BIN_DIR}/mac/sha3
  SHA3_URL=http://files.sireum.org/mac/sha3
  SIREUM=${BIN_DIR}/sireum
elif [ "${PLATFORM}" = "linux" ]; then
  ZULU_URL=http://cdn.azul.com/zulu/bin/zulu${ZULU_VERSION}-linux_x64.tar.gz
  SHA3=${BIN_DIR}/linux/sha3
  SHA3_URL=http://files.sireum.org/linux/sha3
  SIREUM=${BIN_DIR}/sireum
fi
mkdir -p ${BIN_DIR}/${PLATFORM}
cd ${BIN_DIR}/${PLATFORM}
ZULU="${ZULU_URL##*/}"
ZULU_DIR="${ZULU%.*}"
if [[ ${ZULU_DIR} == *.tar ]]; then
  ZULU_DIR="${ZULU_DIR%.*}"
fi
grep -q ${ZULU_VERSION} jdk/VER &> /dev/null && ZULU_UPDATE=false || ZULU_UPDATE=true
if [ ! -d jdk ] || [ "${ZULU_UPDATE}" = "true" ]; then
  if [ ! -f ${ZULU} ]; then
    if [ -f ${SIREUM_CACHE}/${ZULU} ]; then
      echo "Using ${SIREUM_CACHE}/${ZULU} ... "
      ln -s ${SIREUM_CACHE}/${ZULU}
      echo
    else
      echo "Downloading Zulu JDK ${ZULU_VERSION} ..."
      ${CURL} ${ZULU} ${ZULU_URL}
      echo
      if [ ! -z ${SIREUM_CACHE} ]; then
        echo "Copying to ${SIREUM_CACHE}/${ZULU} ..."
        cp ${ZULU} ${SIREUM_CACHE}/${ZULU}
        echo
      fi
    fi
  fi
  if [[ ${ZULU} == *.zip ]]; then
    unzip -oq ${ZULU}
  else
    tar xf ${ZULU}
  fi
  rm ${ZULU}
  rm -fR jdk
  mv ${ZULU_DIR} jdk
  if [ -d "jdk/bin" ]; then
    echo "${ZULU_VERSION}" > jdk/VER
  else
    >&2 echo "Could not install Zulu JDK ${ZULU_VERSION}."
    exit 1
  fi
fi
if [ ! -f ${SHA3} ]; then
  echo "Downloading sha3 ..."
  ${CURL} ${SHA3} ${SHA3_URL}
  chmod +x ${SHA3}
fi
if [ ! -f ${MILL} ] || [ "$(${SHA3} 256 < ${MILL})" != ${MILL_SHA3} ]; then
  echo "Downloading mill ..."
  ${CURL} ${MILL} ${MILL_URL}
  chmod +x ${MILL}
  MILL_SHA3_LOCAL="$(${SHA3} 256 < ${MILL})"
  if [ "$(${SHA3} 256 < ${MILL})" != ${MILL_SHA3} ]; then
    >&2 echo "Mill version mismatch (${MILL_SHA3_LOCAL} != ${MILL_SHA3}); please notify TOOL maintainers."
  fi
fi
if [ ! -f ${LIB} ] || [ "$(${SHA3} 256 < ${LIB})" != ${LIB_SHA3} ]; then
  echo "Downloading versions.properties ..."
  ${CURL} ${LIB} ${LIB_URL}
  LIB_SHA3_LOCAL="$(${SHA3} 256 < ${LIB})"
  if [ ${LIB_SHA3_LOCAL} != ${LIB_SHA3} ]; then
    >&2 echo "Library dependency versions mismatch (${LIB_SHA3_LOCAL} != ${LIB_SHA3}); please notify TOOL maintainers."
  fi
fi
if [ ! -f ${SIREUM} ] || [ "$(${SHA3} 256 < ${SIREUM})" != ${SIREUM_SHA3} ]; then
  echo "Downloading sireum ..."
  ${CURL} ${SIREUM} ${SIREUM_URL}
  chmod +x ${SIREUM}
  SIREUM_SHA3_LOCAL="$(${SHA3} 256 < ${SIREUM})"
  if [ ${SIREUM_SHA3_LOCAL} != ${SIREUM_SHA3} ]; then
    >&2 echo "Sireum version mismatch (${SIREUM_SHA3_LOCAL} != ${SIREUM_SHA3}); please notify TOOL maintainers."
  fi
fi
