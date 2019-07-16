// #Sireum

package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

@record class BlessGen(basePackage: String,
                       component: Component,
                       componentNames: Names) {

  var btsStates: Map[String, BTSStateDeclaration] = Map.empty

  var declaredStates: ISZ[String] = ISZ()
  var initialState: String = ""

  var globalVariables: Map[String, BTSVariableDeclaration] = Map.empty

  var stateMachines: Map[BTSStateCategory.Type, ISZ[GuardedTransition]] = Map.empty

  var transitionMethods: ISZ[ST] = ISZ()

  def process(a: BTSBLESSAnnexClause): ST = {
    if(a.assertions.nonEmpty) {
      println(s"Need to handle assertions")
    } // TODO
    if(a.invariant.nonEmpty){
      println(s"Need to handle invariants")
    } // TODO

    val packageName: String = componentNames.packageName
    val imports: ISZ[ST] = ISZ()
    val states: ISZ[ST] = a.states.map(s => visitStates(s))
    val componentName = componentNames.componentImpl
    val bridgeName = componentNames.bridge
    val globalVars: ISZ[ST] = a.variables.map(v => visitVariable(v))
    var methods: ISZ[ST] = a.transitions.map(t => visitTransition(t))
    val extensions: ISZ[ST] = ISZ()

    for(entry <- stateMachines.entries) {
      methods = methods :+ buildSM(entry._1, entry._2)
    }

    return BlessST.main(packageName, imports, states, componentName, bridgeName, initialState,
      globalVars, methods, extensions)
  }

  def buildSM(t: BTSStateCategory.Type, l: ISZ[GuardedTransition]) : ST = {

    if(t == BTSStateCategory.Initial) {
      val inits = l.filter(f => f.srcState == this.initialState).map(m =>
        (m.transCondition, st"${m.actionMethodName}()"))
      assert(inits.length == 1)

      val options = BlessST.ifST(inits(0), ISZOps(inits).tail)

      return BlessST.method("Initialize", options)
    } else {
      var cases: ISZ[ST] = ISZ()

      for(e <- btsStates.entries) {
        val stateName = e._1
        val gts: ISZ[(ST, ST)] = l.filter(f => f.srcState == stateName).map(m =>
          (m.transCondition, st"${m.actionMethodName}()")
        )
        val options: ST = if(gts.isEmpty) {
          st"// empty"
        } else {
          BlessST.ifST(gts(0), ISZOps(gts).tail)
        }

        val _case = st"""case CompleteState.${stateName} =>
                        |  $options"""

        cases = cases :+ _case

      }

      val body = st"""currentState match {
                     |  ${(cases, "\n")}
                     |}"""

      return BlessST.method("Compute", body)
    }
  }

  def visitStates(state: BTSStateDeclaration): ST = {
    assert(state.assertion.isEmpty)
    assert(state.id.name.length == 1)

    val stateName = state.id.name(0)
    declaredStates = declaredStates :+ stateName

    btsStates = btsStates + (stateName ~> state)

    if(state.categories.filter(f => f == BTSStateCategory.Initial).nonEmpty){
      initialState = stateName
    }

    return st"'${stateName}"
  }

  def visitVariable(variable: BTSVariableDeclaration): ST = {
    val typ = variable.category // TODO
    val assignExp = variable.assignExpression // TODO
    val arraySize = variable.arraySize // TODO
    val variableAssertion = variable.variableAssertion // TODO

    val varName: String = Util.getLastName(variable.name)

    val varType = getType(variable.varType)

    globalVariables = globalVariables + (varName ~> variable)

    return BlessST.typeDec(varName, varType)
  }

  def visitTransition(t: BTSTransition): ST = {

    assert(t.assertion.isEmpty)

    assert(t.sourceStates.length == 1)
    assert(t.sourceStates(0).name.length == 1)
    assert(t.destState.name.length == 1)

    val src: String = t.sourceStates(0).name(0)
    val dst: String = t.destState.name(0)

    val cond: ST = t.transitionCondition match {
      case Some(c) => visitTransitionCondition(c)
      case _ => st"true"
    }

    assert(t.label.priority.isEmpty)
    val label = t.label.id.name(0)

    val actions: ISZ[ST] = t.actions match {
      case Some(actions) => ISZ()
      case _ => ISZ()
    }

    val transActions = st"do_${label}"
    assert(btsStates.get(src).get.categories.length == 1)

    val key: BTSStateCategory.Type = btsStates.get(src).get.categories(0)
    val list: ISZ[GuardedTransition] = stateMachines.getOrElse(key, ISZ())

    val gt = GuardedTransition(src, dst, cond, transActions)

    stateMachines = stateMachines + (key ~> (list :+ gt))

    return st"" // TODO opt ret
  }

  def visitTransitionCondition(condition: BTSTransitionCondition): ST = {
    condition match {
      case c: BTSDispatchCondition => return visitBTSDispatchCondition(c)
      case _ => halt("Unexpected trans cond")
    }
  }

  def visitBTSDispatchCondition(condition: BTSDispatchCondition): ST = {
    if(condition.dispatchTriggers.isEmpty) {
      assert(Util.isPeriodic(this.component))

      return st"true"
    } else {

      return ISZOps(condition.dispatchTriggers).foldLeft((r: ST, t) =>
        st"$r && ${visitBTSDispatchConjunction(t)}", st"")
    }
  }

  def visitBTSDispatchConjunction(conjunction: BTSDispatchConjunction): ST = {
    return ISZOps(conjunction.conjunction).foldLeft((r: ST, t) =>
      st"$r || ${visitBTSDispatchTrigger(t)}", st"")
  }

  def visitBTSDispatchTrigger(trigger: BTSDispatchTrigger): ST = {
    val ret: ST = trigger match {
      case BTSDispatchTriggerStop() => st"STOP"
      case BTSDispatchTriggerPort(port) =>
        val portName = Util.getLastName(port)
        st"api.get${portName}.nonEmpty"
      case BTSDispatchTriggerTimeout(ports, time) => st"TIMEOUT"
    }

    return ret
  }

  def getType(t: BTSType): String = {
    t match {
      case o: BTSClassifier => return Util.getName(o.name)
      case _ =>
        halt(s"Need to handle type $t")
    }
  }
}

@datatype class GuardedTransition(srcState: String,
                                  dstState: String,
                                  transCondition: ST,
                                  actionMethodName: ST)