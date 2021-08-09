// #Sireum
package org.sireum.hamr.arsit.util

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlProcessor, AadlThread, SymbolTable}

object SchedulerUtil {

  def getThreadTimingPropertiesName(thread: AadlThread): String = {
    return s"${thread.path}_timingProperties"
  }

  def getProcessorTimingPropertiesName(processor: AadlProcessor): String = {
    return s"${processor.path}_timingProperties"
  }

  def getSchedulerTouches(symbolTable: SymbolTable): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    ret = ret ++ (for(p <- symbolTable.getAllBoundProcessors()) yield {
      st"println(Schedulers.${getProcessorTimingPropertiesName(p)})"
    })
    ret = ret ++ (for(t <- symbolTable.getThreads()) yield {
      st"println(Schedulers.${getThreadTimingPropertiesName(t)})"
    })
    return ret
  }

  def getThreadTimingProperties(symbolTable: SymbolTable, devicesAsThreads: B): ISZ[ST] = {

    return for(t <- symbolTable.getThreads()) yield {
      val computeExecutionTime: String = t.getComputeExecutionTime() match {
        case Some((low, high)) => s"Some((${low}, ${high}))"
        case _ => "None()"
      }
      val domain: String = t.getDomain(symbolTable) match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val name = getThreadTimingPropertiesName(t)
      st"""val ${name}: ThreadTimingProperties = ThreadTimingProperties(
          |  computeExecutionTime = ${computeExecutionTime},
          |  domain = ${domain})"""
    }
  }

  def getProcessorTimingProperties(symbolTable: SymbolTable): ISZ[ST] = {
    return for(p <- symbolTable.getAllBoundProcessors()) yield {
      val clockPeriod: String = p.getClockPeriod() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val framePeriod: String = p.getFramePeriod() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val maxDomain: String = p.getMaxDomain() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val slotTime: String = p.getSlotTime() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val name = getProcessorTimingPropertiesName(p)
      st"""val ${name}: ProcessorTimingProperties = ProcessorTimingProperties(
          |  clockPeriod = ${clockPeriod},
          |  framePeriod = ${framePeriod},
          |  maxDomain = ${maxDomain},
          |  slotTime = ${slotTime})"""
    }
  }

  val defaultFramePeriod: Z = 1000 // 1000 ms


  def getFramePeriod(symbolTable: SymbolTable): Z = {
    var processors:ISZ[AadlProcessor] = ISZ()
    var singleProcessor: B = T
    for(p <- symbolTable.getThreads().map(m => m.getParent(symbolTable))) {
      p.getBoundProcessor(symbolTable) match {
        case Some(processor) =>
          if(processors.isEmpty) processors = processors :+ processor
          else singleProcessor = singleProcessor & processors(0) == processor
        case _ =>
      }
    }
    if(singleProcessor) {
      processors(0).getFramePeriod() match {
        case Some(z) => return z
        case _ =>
      }
    }
    return defaultFramePeriod
  }
}
