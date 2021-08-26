// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._

object SchedulerTemplate {
  def schedulers(packageName: String,
                 bridges: ISZ[String],
                 processorTimingProperties: ISZ[ST],
                 threadTimingProperties: ISZ[ST],
                 framePeriod: Z): ST = {
    val slots = bridges.map((b: String) => s"Slot(Arch.${b}.id, maxExecutionTime)")
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

  def c_roundRobin(packageName: String,
                   bridges: ISZ[String],
                   filepath: String): ST = {
    val slangPath = s"architecture/${packageName}/Schedulers.scala"
    val slangMethodName = s"${packageName}.ScheduleProviderI.getRoundRobinOrder"
    val cMethodName = s"${packageName}_ScheduleProviderI_getRoundRobinOrder"
    val slangSymbol = s"${packageName}.Schedulers.roundRobinSchedule"
    val symbol = s"${packageName}_Schedulers_roundRobinSchedule"
    val iszUpdates = bridges.map((m: String) => s"IS_7E8796_up(result, i++, (art_Bridge) ${m}(SF_LAST));")

    val ret: ST = st"""#include <all.h>
                      |#include <signal.h>
                      |
                      |// Transpiled signature of the Slang variable ${slangSymbol}
                      |// in ${slangPath}.  This weak function declaration allows
                      |// ${cMethodName} to detect whether the Slang variable was deleted
                      |__attribute__((weak)) IS_7E8796 ${symbol}(STACK_FRAME_ONLY);
                      |
                      |volatile sig_atomic_t shouldStop = 0;
                      |
                      |/*!
                      | * Example C implementation of the Slang extension method ${slangMethodName}()
                      | * defined in ${slangPath}
                      | *
                      | * @param result an empty schedule.  Add components in the order you want them to be dispatched.
                      | *               IS_7E8796=ISZ[art.Bridge], i.e. an immutable sequence of art.Bridge
                      | */
                      |void ${cMethodName}(STACK_FRAME IS_7E8796 result) {
                      |
                      |  if(${symbol}) {
                      |    printf("Using the round robin order provided in ${slangPath}. Edit method \n");
                      |    printf("  ${cMethodName} located in ${filepath}\n");
                      |    printf("to supply your own\n");
                      |
                      |    IS_7E8796 order = ${symbol}();
                      |    memcpy(result->value, order->value, sizeof(union art_Bridge) * order->size);
                      |    result->size = order->size;
                      |
                      |  } else {
                      |    printf("Transpiled Slang variable ${slangSymbol} not found.  Using an example schedule from method");
                      |    printf("  ${cMethodName} located in ${filepath}\n");
                      |
                      |    // example schedule
                      |    int i = 0;
                      |    ${(iszUpdates, "\n")}
                      |
                      |    result->size = i;
                      |  }
                      |}
                      |
                      |/*!
                      | * signal handler that sets shouldStop to true when invoked
                      | */
                      |void sigHandler(int signo) {
                      |  shouldStop = 1;
                      |}
                      |
                      |/*!
                      | * Example C implementation of Slang extension method art.scheduling.roundrobin.RoundRobinExtensions.init()
                      | * defined in art/scheduling/roundrobin/RoundRobin.scala.  The scheduler calls this
                      | * during the initialization phase
                      | *
                      | * It registers a signal handler that is used to shut down the demo when it receives
                      | * SIGINT (CTRL+C), SIGTERM, or SIGQUIT
                      | */
                      |Unit art_scheduling_roundrobin_RoundRobinExtensions_init(STACK_FRAME_ONLY){
                      |  int sigs[] = {SIGINT, SIGTERM, SIGQUIT};
                      |  for(int i = 0; i < sizeof(sigs) / sizeof(int); i++){
                      |    if(signal(sigs[i], sigHandler) == SIG_ERR) {
                      |      printf("Error occurred while setting signal handler for %i\n", sigs[i]);
                      |      exit(-1);
                      |    }
                      |  }
                      |}
                      |
                      |/*!
                      | * Example C implementation of Slang extension method art.scheduling.roundrobin.RoundRobinExtensions.shouldStop()
                      | * defined in art/scheduling/roundrobin/RoundRobin.scala.  The scheduler calls this
                      | * during the compute phase to determine when it should transition to the finalize phase
                      | */
                      |B art_scheduling_roundrobin_RoundRobinExtensions_shouldStop(STACK_FRAME_ONLY){
                      |    return shouldStop == 1;
                      |}
                      |"""
    return ret
  }

  def c_static_schedule(packageName: String,
                        bridges: ISZ[String],
                        filepath: String): ST = {
    val slangPath = s"architecture/${packageName}/Schedulers.scala"
    val slangMethodName = s"${packageName}.ScheduleProviderI.getStaticSchedule"
    val cMethodName = s"${packageName}_ScheduleProviderI_getStaticSchedule"
    val slangSymbol = s"${packageName}.Schedulers.staticSchedule"
    val symbol = s"${packageName}_Schedulers_staticSchedule"
    val slotSequences = bridges.map((m: String) => s"fillInSlot(&slotSequence, i++, ${m}(SF_LAST)->id, length);")

    val ret:ST = st"""#include <all.h>
                     |
                     |// Transpiled signature of the Slang variable ${slangSymbol}
                     |// in ${slangPath}.  This weak function declaration allows
                     |// ${cMethodName} to detect whether the Slang variable was deleted
                     |__attribute__((weak)) art_scheduling_static_Schedule_DScheduleSpec ${symbol}(STACK_FRAME_ONLY);
                     |
                     |// helper method
                     |void fillInSlot(IS_5AA467 slotSequence, int index, Z bridgeId, int length);
                     |
                     |/*!
                     | * Example C implementation of the Slang extension method ${slangMethodName}()
                     | * defined in ${slangPath}
                     | *
                     | * @param result an empty schedule. Add slots in the order you want components to be dispatched.
                     | */
                     |void ${cMethodName}(STACK_FRAME art_scheduling_static_Schedule_DScheduleSpec result){
                     |
                     |  if(${symbol}) {
                     |    printf("Using the static schedule provided in ${slangPath}. Edit method \n");
                     |    printf("  ${cMethodName} located in ${filepath}\n");
                     |    printf("to supply your own\n");
                     |
                     |    art_scheduling_static_Schedule_DScheduleSpec schedule = ${symbol}(SF_LAST);
                     |    result->hyperPeriod = schedule->hyperPeriod;
                     |    result->maxDomain = schedule->maxDomain;
                     |    memcpy(&result->schedule, &schedule->schedule, sizeof(struct art_scheduling_static_Schedule_DSchedule));
                     |
                     |  } else {
                     |    printf("Transpiled Slang variable ${slangSymbol} not found.  Using an example schedule from method");
                     |    printf("  ${cMethodName} located in ${filepath}\n");
                     |
                     |    // IS_5AA467=IS[Z, art.scheduling.static.Schedule.Slot], i.e. an immutable sequence of art.scheduling.static.Schedule.Slot
                     |    DeclNewIS_5AA467(slotSequence);
                     |
                     |    Z length = 1000 / ${bridges.size};
                     |
                     |    int i = 0;
                     |    ${(slotSequences, "\n")}
                     |    slotSequence.size = i;
                     |
                     |    DeclNewart_scheduling_static_Schedule_DSchedule(dschedule);
                     |    art_scheduling_static_Schedule_DSchedule_apply(SF &dschedule, &slotSequence);
                     |
                     |    Z maxDomain = 100;
                     |    Z hyperPeriod = 1000;
                     |
                     |    art_scheduling_static_Schedule_DScheduleSpec_apply(SF result, maxDomain, hyperPeriod, &dschedule);
                     |  }
                     |}
                     |
                     |void fillInSlot(IS_5AA467 slotSequence, int index, Z bridgeId, int length) {
                     |  slotSequence->value[index].bridgeId = bridgeId;
                     |  slotSequence->value[index].length = length;
                     |}
                     |"""
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