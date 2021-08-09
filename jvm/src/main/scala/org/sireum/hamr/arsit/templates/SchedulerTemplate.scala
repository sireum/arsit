// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._

object SchedulerTemplate {
  def schedulers(packageName: String,
                 bridges: ISZ[String],
                 processorTimingProperties: ISZ[ST],
                 threadTimingProperties: ISZ[ST],
                 framePeriod: Z): ST = {
    val ids = bridges.map((b: String) => s"val ${b}_id: Art.PortId = Arch.${b}.id")
    val slots = bridges.map((b: String) => s"Slot(${b}_id, maxExecutionTime)")
    val ret = st"""// #Sireum
                  |package ${packageName}
                  |
                  |import org.sireum._
                  |import art.Art
                  |import art.scheduling.legacy.Legacy
                  |import art.scheduling.roundrobin.RoundRobin
                  |import art.scheduling.static.Schedule.{DSchedule, DScheduleSpec, Slot}
                  |import art.scheduling.static.StaticScheduler
                  |
                  |${StringTemplate.doNotEditComment(None())}
                  |
                  |@datatype class ProcessorTimingProperties(val clockPeriod: Option[Z],
                  |                                          val framePeriod: Option[Z],
                  |                                          val maxDomain: Option[Z],
                  |                                          val slotTime: Option[Z])
                  |
                  |@datatype class ThreadTimingProperties(val domain: Option[Z],
                  |                                       val computeExecutionTime: Option[(Z, Z)])
                  |
                  |object Schedulers {
                  |
                  |  ${(ids, "\n")}
                  |
                  |  ${(processorTimingProperties, "\n\n")}
                  |
                  |  ${(threadTimingProperties, "\n\n")}
                  |
                  |  // roundRobinSchedule represents the component dispatch order
                  |  val roundRobinSchedule: ISZ[art.Bridge] = Arch.ad.components
                  |
                  |  val framePeriod: Z = ${framePeriod}
                  |  val numComponents: Z = Arch.ad.components.size
                  |  val maxExecutionTime: Z = numComponents / framePeriod
                  |
                  |  // staticSchedule represents the component dispatch order
                  |  val staticSchedule: DScheduleSpec = DScheduleSpec(0, 0, DSchedule(ISZ(
                  |    ${(slots, ",\n")}
                  |  )))
                  |
                  |
                  |  def getRoundRobinScheduler(schedule: Option[ISZ[art.Bridge]]): RoundRobin = {
                  |    if(roundRobinSchedule.isEmpty) {} // line needed for transpiler; do not remove
                  |    schedule match {
                  |      case Some(s) => return RoundRobin(s)
                  |      case _ => return RoundRobin(ScheduleProviderI.getRoundRobinOrder())
                  |    }
                  |  }
                  |
                  |  def getStaticScheduler(schedule: Option[DScheduleSpec]): StaticScheduler = {
                  |    if(staticSchedule.schedule.slots.isEmpty) {} // line needed for transpiler; do not remove
                  |    schedule match {
                  |      case Some(s) => return StaticScheduler(Arch.ad.components, s)
                  |      case _ => return StaticScheduler(Arch.ad.components, ScheduleProviderI.getStaticSchedule())
                  |    }
                  |  }
                  |
                  |  def getLegacyScheduler(): Legacy = {
                  |    return Legacy(Arch.ad.components)
                  |  }
                  |}
                  |
                  |@ext(name = "ScheduleProvider") object ScheduleProviderI {
                  |  def getRoundRobinOrder(): ISZ[art.Bridge] = $$
                  |  def getStaticSchedule(): DScheduleSpec = $$
                  |}"""
    return ret
  }

  def scheduleProvider(packageName: String): ST = {
    val ret: ST = st"""package ${packageName}
                      |
                      |import org.sireum._
                      |import art.scheduling.static.Schedule.DScheduleSpec
                      |
                      |${StringTemplate.doNotEditComment(None())}
                      |
                      |object ScheduleProvider {
                      |
                      |  def getRoundRobinOrder(): ISZ[art.Bridge] = {
                      |    return Schedulers.roundRobinSchedule
                      |  }
                      |
                      |  def getStaticSchedule(): DScheduleSpec = {
                      |    return Schedulers.staticSchedule
                      |  }
                      |}
                      |"""
    return ret
  }

  def c_legacy(): ST = {
    val ret: ST = st"""#include <all.h>
                      |
                      |Unit art_scheduling_legacy_LegacyInterface_computePhase(STACK_FRAME IS_7E8796 bridges) {
                      |  printf("Infeasible.  You should not get here in C");
                      |  exit(1);
                      |}"""
    return ret
  }

  def c_roundRobin(packageName: String): ST = {
    val methodName = s"${packageName}_ScheduleProviderI_getRoundRobinOrder"
    val symbol = s"${packageName}_Schedulers_roundRobinSchedule"

    val ret: ST = st"""#include <all.h>
                      |#include <signal.h>
                      |
                      |__attribute__((weak)) IS_7E8796 ${symbol}(STACK_FRAME_ONLY);
                      |
                      |volatile sig_atomic_t shouldStop = 0;
                      |
                      |void ${methodName}(STACK_FRAME IS_7E8796 result) {
                      |
                      |    if(${symbol}) {
                      |        IS_7E8796 order = ${symbol}();
                      |        memcpy(result->value, order->value, sizeof(union art_Bridge) * order->size);
                      |        result->size = order->size;
                      |
                      |        printf("Using the round robin order provided in Schedulers. Edit method \n");
                      |        printf("  ${methodName}\n");
                      |        printf("to supply your own\n");
                      |    } else {
                      |        printf("Schedulers.roundRobinSchedule not found.  You'll need to supply your own order in C\n");
                      |        exit(-1);
                      |    }
                      |}
                      |
                      |void sigHandler(int signo) {
                      |  shouldStop = 1;
                      |}
                      |
                      |Unit art_scheduling_roundrobin_RoundRobinExtensions_init(STACK_FRAME_ONLY){
                      |  int sigs[] = {SIGINT, SIGTERM, SIGQUIT};
                      |  for(int i = 0; i < sizeof(sigs) / sizeof(int); i++){
                      |    if(signal(sigs[i], sigHandler) == SIG_ERR) {
                      |      printf("Error occurred while setting signal handler for %i\\n", sigs[i]);
                      |      exit(-1);
                      |    }
                      |  }
                      |}
                      |
                      |B art_scheduling_roundrobin_RoundRobinExtensions_shouldStop(STACK_FRAME_ONLY){
                      |    return shouldStop == 1;
                      |}
                      |"""
    return ret
  }

  def c_static_schedule(packageName: String): ST = {
    val methodName = s"${packageName}_StaticScheduleProvider_getStaticSchedule"
    val symbol = s"${packageName}_Schedulers_staticSchedule"

    val ret:ST = st"""#include <all.h>
                     |
                     |__attribute__((weak)) art_scheduling_static_Schedule_DScheduleSpec ${symbol}(STACK_FRAME_ONLY);
                     |
                     |void ${methodName}(STACK_FRAME art_scheduling_static_Schedule_DScheduleSpec result){
                     |
                     |    if(${symbol}) {
                     |        art_scheduling_static_Schedule_DScheduleSpec schedule = ${symbol}();
                     |        result->hyperPeriod = schedule->hyperPeriod;
                     |        result->maxDomain = schedule->maxDomain;
                     |        memcpy(&result->schedule, &schedule->schedule, sizeof(struct art_scheduling_static_Schedule_DSchedule));
                     |
                     |        printf("Using the static schedule provided Schedulers. Edit method \n");
                     |        printf("  ${methodName}\n");
                     |        printf("to supply your own\n");
                     |    } else {
                     |        printf("Schedulers.staticSchedule not found.  You'll need to supply your own order in C\n");
                     |        exit(-1);
                     |    }
                     |
                     |}"""
    return ret
  }

  def c_process(): ST = {
    val ret: ST = st"""#include <all.h>
                      |
                      |#include <sys/time.h>
                      |#include <time.h>
                      |
                      |/** Returns current system time in milliseconds
                      |  * NOTE: this requires returning 64bit ints
                      |  */
                      |S64 art_Process_time(STACK_FRAME_ONLY) {
                      |  struct timeval tv; //Get a time structure
                      |  gettimeofday(&tv, NULL); //Get the current time
                      |  int64_t t = tv.tv_sec;
                      |  t *= 1000;
                      |  t += tv.tv_usec/1000;
                      |  return  t;
                      |}
                      |
                      |Unit Os_Ext_exit(STACK_FRAME Z code) {
                      |  exit(code);
                      |}
                      |"""
    return ret
  }
}