// #Sireum
// @formatter:off

/*
 Copyright (c) 2019, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This file is auto-generated from cli.sc

package org.sireum

import org.sireum._

object Cli {

  @datatype trait ArsitTopOption

  @datatype class HelpOption extends ArsitTopOption

  @enum object IpcMechanism {
    'MessageQueue
    'SharedMemory
  }

  @enum object Platform {
    'JVM
    'Linux
    'Cygwin
    'MacOS
    'SeL4
  }

  @datatype class ArsitOption(
    help: String,
    args: ISZ[String],
    json: B,
    outputDir: Option[String],
    packageName: Option[String],
    noart: B,
    bless: B,
    verbose: B,
    devicesAsThreads: B,
    ipc: IpcMechanism.Type,
    behaviorDir: Option[String],
    outputCDir: Option[String],
    excludeImpl: B,
    platform: Platform.Type,
    bitWidth: Z,
    maxStringSize: Z,
    maxArraySize: Z
  ) extends ArsitTopOption
}

import Cli._

@record class Cli(pathSep: C) {

  def parseIpcMechanismH(arg: String): Option[IpcMechanism.Type] = {
    arg.native match {
      case "MessageQueue" => return Some(IpcMechanism.MessageQueue)
      case "SharedMemory" => return Some(IpcMechanism.SharedMemory)
      case s =>
        eprintln(s"Expecting one of the following: { MessageQueue, SharedMemory }, but found '$s'.")
        return None()
    }
  }

  def parseIpcMechanism(args: ISZ[String], i: Z): Option[IpcMechanism.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { MessageQueue, SharedMemory }, but none found.")
      return None()
    }
    val r = parseIpcMechanismH(args(i))
    return r
  }

  def parsePlatformH(arg: String): Option[Platform.Type] = {
    arg.native match {
      case "JVM" => return Some(Platform.JVM)
      case "Linux" => return Some(Platform.Linux)
      case "Cygwin" => return Some(Platform.Cygwin)
      case "MacOS" => return Some(Platform.MacOS)
      case "seL4" => return Some(Platform.SeL4)
      case s =>
        eprintln(s"Expecting one of the following: { JVM, Linux, Cygwin, MacOS, seL4 }, but found '$s'.")
        return None()
    }
  }

  def parsePlatform(args: ISZ[String], i: Z): Option[Platform.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { JVM, Linux, Cygwin, MacOS, seL4 }, but none found.")
      return None()
    }
    val r = parsePlatformH(args(i))
    return r
  }

  def parseArsit(args: ISZ[String], i: Z): Option[ArsitTopOption] = {
    val help =
      st"""Slang Generator
          |
          |Usage: <option>* air-file
          |
          |Available Options:
          |-j, --json               Input serialized using Json (otherwise MsgPack
          |                           assumed)
          |-o, --output-dir         Output directory for the generated project files
          |                           (expects a path; default is ".")
          |    --package-name       Base package name for Slang project (output-dir's
          |                           simple name used if not provided) (expects a string)
          |    --noart              Do not embed ART project files
          |    --bless              Generate Bless entrypoints
          |    --verbose            Enable verbose mode
          |    --devices-as-thread  Treat AADL devices as threads
          |-h, --help               Display this information
          |
          |Transpiler Options:
          |    --ipc                IPC communication mechanism (requires 'trans' option)
          |                           (expects one of { MessageQueue, SharedMemory };
          |                           default: MessageQueue)
          |    --behavior-dir       Auxiliary C source code directory (expects a path)
          |    --output-c-directory Output directory for C artifacts (expects a path)
          |    --exclude-impl       Exclude Slang component implementations
          |    --platform           Target platform (expects one of { JVM, Linux, Cygwin,
          |                           MacOS, seL4 }; default: JVM)
          |-b, --bit-width          Default bit-width for unbounded integer types (e.g.,
          |                           Z) (expects one of { 64, 32, 16, 8 })
          |    --string-size        Maximum string size (expects an integer; default is
          |                           100)
          |    --sequence-size      Default maximum sequence size (expects an integer;
          |                           default is 100)""".render

    var json: B = false
    var outputDir: Option[String] = Some(".")
    var packageName: Option[String] = None[String]()
    var noart: B = false
    var bless: B = false
    var verbose: B = false
    var devicesAsThreads: B = false
    var ipc: IpcMechanism.Type = IpcMechanism.MessageQueue
    var behaviorDir: Option[String] = None[String]()
    var outputCDir: Option[String] = None[String]()
    var excludeImpl: B = false
    var platform: Platform.Type = Platform.JVM
    var bitWidth: Z = 64
    var maxStringSize: Z = 100
    var maxArraySize: Z = 100
    var j = i
    var isOption = T
    while (j < args.size && isOption) {
      val arg = args(j)
      if (ops.StringOps(arg).first == '-') {
        if (args(j) == "-h" || args(j) == "--help") {
          println(help)
          return Some(HelpOption())
        } else if (arg == "-j" || arg == "--json") {
           val o: Option[B] = { j = j - 1; Some(!json) }
           o match {
             case Some(v) => json = v
             case _ => return None()
           }
         } else if (arg == "-o" || arg == "--output-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => outputDir = v
             case _ => return None()
           }
         } else if (arg == "--package-name") {
           val o: Option[Option[String]] = parseString(args, j + 1)
           o match {
             case Some(v) => packageName = v
             case _ => return None()
           }
         } else if (arg == "--noart") {
           val o: Option[B] = { j = j - 1; Some(!noart) }
           o match {
             case Some(v) => noart = v
             case _ => return None()
           }
         } else if (arg == "--bless") {
           val o: Option[B] = { j = j - 1; Some(!bless) }
           o match {
             case Some(v) => bless = v
             case _ => return None()
           }
         } else if (arg == "--verbose") {
           val o: Option[B] = { j = j - 1; Some(!verbose) }
           o match {
             case Some(v) => verbose = v
             case _ => return None()
           }
         } else if (arg == "--devices-as-thread") {
           val o: Option[B] = { j = j - 1; Some(!devicesAsThreads) }
           o match {
             case Some(v) => devicesAsThreads = v
             case _ => return None()
           }
         } else if (arg == "--ipc") {
           val o: Option[IpcMechanism.Type] = parseIpcMechanism(args, j + 1)
           o match {
             case Some(v) => ipc = v
             case _ => return None()
           }
         } else if (arg == "--behavior-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => behaviorDir = v
             case _ => return None()
           }
         } else if (arg == "--output-c-directory") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => outputCDir = v
             case _ => return None()
           }
         } else if (arg == "--exclude-impl") {
           val o: Option[B] = { j = j - 1; Some(!excludeImpl) }
           o match {
             case Some(v) => excludeImpl = v
             case _ => return None()
           }
         } else if (arg == "--platform") {
           val o: Option[Platform.Type] = parsePlatform(args, j + 1)
           o match {
             case Some(v) => platform = v
             case _ => return None()
           }
         } else if (arg == "-b" || arg == "--bit-width") {
           val o: Option[Z] = parseNumChoice(args, j + 1, ISZ(z"64", z"32", z"16", z"8"))
           o match {
             case Some(v) => bitWidth = v
             case _ => return None()
           }
         } else if (arg == "--string-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxStringSize = v
             case _ => return None()
           }
         } else if (arg == "--sequence-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxArraySize = v
             case _ => return None()
           }
         } else {
          eprintln(s"Unrecognized option '$arg'.")
          return None()
        }
        j = j + 2
      } else {
        isOption = F
      }
    }
    return Some(ArsitOption(help, parseArguments(args, j), json, outputDir, packageName, noart, bless, verbose, devicesAsThreads, ipc, behaviorDir, outputCDir, excludeImpl, platform, bitWidth, maxStringSize, maxArraySize))
  }

  def parseArguments(args: ISZ[String], i: Z): ISZ[String] = {
    var r = ISZ[String]()
    var j = i
    while (j < args.size) {
      r = r :+ args(j)
      j = j + 1
    }
    return r
  }

  def parsePaths(args: ISZ[String], i: Z): Option[ISZ[String]] = {
    return tokenize(args, i, "path", pathSep, F)
  }

  def parsePath(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a path, but none found.")
    }
    return Some(Some(args(i)))
  }

  def parseStrings(args: ISZ[String], i: Z, sep: C): Option[ISZ[String]] = {
    tokenize(args, i, "string", sep, F) match {
      case r@Some(_) => return r
      case _ => return None()
    }
  }

  def parseString(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a string, but none found.")
      return None()
    }
    return Some(Some(args(i)))
  }

  def parseNums(args: ISZ[String], i: Z, sep: C, minOpt: Option[Z], maxOpt: Option[Z]): Option[ISZ[Z]] = {
    tokenize(args, i, "integer", sep, T) match {
      case Some(sargs) =>
        var r = ISZ[Z]()
        for (arg <- sargs) {
          parseNumH(arg, minOpt, maxOpt) match {
            case Some(n) => r = r :+ n
            case _ => return None()
          }
        }
        return Some(r)
      case _ => return None()
    }
  }

  def tokenize(args: ISZ[String], i: Z, tpe: String, sep: C, removeWhitespace: B): Option[ISZ[String]] = {
    if (i >= args.size) {
      eprintln(s"Expecting a sequence of $tpe separated by '$sep', but none found.")
      return None()
    }
    val arg = args(i)
    return Some(tokenizeH(arg, sep, removeWhitespace))
  }

  def tokenizeH(arg: String, sep: C, removeWhitespace: B): ISZ[String] = {
    val argCis = conversions.String.toCis(arg)
    var r = ISZ[String]()
    var cis = ISZ[C]()
    var j = 0
    while (j < argCis.size) {
      val c = argCis(j)
      if (c == sep) {
        r = r :+ conversions.String.fromCis(cis)
        cis = ISZ[C]()
      } else {
        val allowed: B = c match {
          case c"\n" => !removeWhitespace
          case c" " => !removeWhitespace
          case c"\r" => !removeWhitespace
          case c"\t" => !removeWhitespace
          case _ => T
        }
        if (allowed) {
          cis = cis :+ c
        }
      }
      j = j + 1
    }
    if (cis.size > 0) {
      r = r :+ conversions.String.fromCis(cis)
    }
    return r
  }

  def parseNumChoice(args: ISZ[String], i: Z, choices: ISZ[Z]): Option[Z] = {
    val set = HashSet.empty[Z] ++ choices
    parseNum(args, i, None(), None()) match {
      case r@Some(n) =>
        if (set.contains(n)) {
          return r
        } else {
          eprintln(s"Expecting one of the following: $set, but found $n.")
          return None()
        }
      case r => return r
    }
  }

  def parseNum(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
    if (i >= args.size) {
      eprintln(s"Expecting an integer, but none found.")
      return None()
    }
    return parseNumH(args(i), minOpt, maxOpt)
  }

  def parseNumH(arg: String, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
    Z(arg) match {
      case Some(n) =>
        minOpt match {
          case Some(min) =>
            if (n < min) {
              eprintln(s"Expecting an integer at least $min, but found $n.")
              return None()
            }
          case _ =>
        }
        maxOpt match {
          case Some(max) =>
            if (n > max) {
              eprintln(s"Expecting an integer at most $max, but found $n.")
              return None()
            }
            return Some(n)
          case _ =>
        }
        return Some(n)
      case _ =>
        eprintln(s"Expecting an integer, but found '$arg'.")
        return None()
    }
  }

  def select(mode: String, args: ISZ[String], i: Z, choices: ISZ[String]): Option[String] = {
    val arg = args(i)
    var cs = ISZ[String]()
    for (c <- choices) {
      if (ops.StringOps(c).startsWith(arg)) {
        cs = cs :+ c
      }
    }
    cs.size match {
      case z"0" =>
        eprintln(s"$arg is not a mode of $mode.")
        return None()
      case z"1" => return Some(cs(0))
      case _ =>
        eprintln(
          st"""Which one of the following modes did you mean by '$arg'?
              |${(cs, "\n")}""".render)
        return None()
    }
  }
}