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

  @enum object Ipcmech {
    'MessageQueue
    'SharedMemory
  }

  @datatype class ArsitOption(
    help: String,
    args: ISZ[String],
    json: B,
    outputDir: Option[String],
    packageName: Option[String],
    noart: B,
    genTrans: B,
    ipc: Ipcmech.Type,
    baTranslate: B,
    baAddViz: B,
    baExposeState: B
  ) extends ArsitTopOption
}

import Cli._

@record class Cli(pathSep: C) {

  def parseIpcmechH(arg: String): Option[Ipcmech.Type] = {
    arg.native match {
      case "MessageQueue" => return Some(Ipcmech.MessageQueue)
      case "SharedMemory" => return Some(Ipcmech.SharedMemory)
      case s =>
        eprintln(s"Expecting one of the following: { MessageQueue, SharedMemory }, but found '$s'.")
        return None()
    }
  }

  def parseIpcmech(args: ISZ[String], i: Z): Option[Ipcmech.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { MessageQueue, SharedMemory }, but none found.")
      return None()
    }
    val r = parseIpcmechH(args(i))
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
          |-h, --help               Display this information
          |
          |Transpiler Options:
          |    --trans              Generate Slang/C code required for transpiler
          |    --ipc                IPC communication mechanism (requires 'trans' option)
          |                           (expects one of { MessageQueue, SharedMemory };
          |                           default: MessageQueue)
          |
          |BA Options:
          |    --ba-translate       Translate state machines to Slang
          |    --ba-add-visualizer  Add state machine visualization
          |    --ba-expose-state    Expose component state""".render

    var json: B = false
    var outputDir: Option[String] = Some(".")
    var packageName: Option[String] = None[String]()
    var noart: B = false
    var genTrans: B = false
    var ipc: Ipcmech.Type = Ipcmech.MessageQueue
    var baTranslate: B = false
    var baAddViz: B = false
    var baExposeState: B = false
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
         } else if (arg == "--trans") {
           val o: Option[B] = { j = j - 1; Some(!genTrans) }
           o match {
             case Some(v) => genTrans = v
             case _ => return None()
           }
         } else if (arg == "--ipc") {
           val o: Option[Ipcmech.Type] = parseIpcmech(args, j + 1)
           o match {
             case Some(v) => ipc = v
             case _ => return None()
           }
         } else if (arg == "--ba-translate") {
           val o: Option[B] = { j = j - 1; Some(!baTranslate) }
           o match {
             case Some(v) => baTranslate = v
             case _ => return None()
           }
         } else if (arg == "--ba-add-visualizer") {
           val o: Option[B] = { j = j - 1; Some(!baAddViz) }
           o match {
             case Some(v) => baAddViz = v
             case _ => return None()
           }
         } else if (arg == "--ba-expose-state") {
           val o: Option[B] = { j = j - 1; Some(!baExposeState) }
           o match {
             case Some(v) => baExposeState = v
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
    return Some(ArsitOption(help, parseArguments(args, j), json, outputDir, packageName, noart, genTrans, ipc, baTranslate, baAddViz, baExposeState))
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