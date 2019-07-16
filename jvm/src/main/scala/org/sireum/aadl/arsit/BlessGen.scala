// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

@record class BlessGen(basePackage: String,
                       component: Component,
                       componentNames: Names) {

  val completeStateEnumName: String = s"${componentNames.component}_CompleteState"

  var btsStates: Map[String, BTSStateDeclaration] = Map.empty

  var declaredStates: ISZ[String] = ISZ()
  var initialState: String = ""

  var globalVariables: Map[String, BTSVariableDeclaration] = Map.empty

  var stateMachines: Map[BTSStateCategory.Type, ISZ[GuardedTransition]] = Map.empty

  var transitionMethods: Map[String, ST] = Map.empty

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
    val globalVars: ISZ[ST] = a.variables.map(v => visitBTSVariableDeclaration(v))
    a.transitions.map(t => visitBTSTransition(t))
    val extensions: ISZ[ST] = ISZ()

    var methods = stateMachines.entries.map(entry => buildSM(entry._1, entry._2))

    methods = methods ++ this.transitionMethods.values

    return BlessST.main(packageName, imports, completeStateEnumName, states, componentName, bridgeName, initialState,
      globalVars, methods, extensions)
  }

  def buildSM(t: BTSStateCategory.Type, l: ISZ[GuardedTransition]) : ST = {

    if(t == BTSStateCategory.Initial) {
      val inits = l.filter(f => f.srcState == this.initialState).map(m =>
        (m.transCondition, st"${m.actionMethodName}()"))
      assert(inits.length == 1)

      val body = BlessST.ifST(inits(0), ISZOps(inits).tail)

      return BlessST.method(st"Initialize_EntryPoint", ISZ(), body, st"Unit")
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

        val _case = st"""case ${completeStateEnumName}.${stateName} =>
                        |  $options"""

        cases = cases :+ _case

      }

      val body = st"""currentState match {
                     |  ${(cases, "\n")}
                     |}"""

      return BlessST.method(st"Compute_EntryPoint", ISZ(), body, st"Unit")
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

  def visitBTSVariableDeclaration(variable: BTSVariableDeclaration): ST = {
    val typ = variable.category // TODO
    val assignExp = variable.assignExpression // TODO
    val arraySize = variable.arraySize // TODO
    val variableAssertion = variable.variableAssertion // TODO

    val varName: String = Util.getLastName(variable.name)

    val varType = visitBTSType(variable.varType)

    globalVariables = globalVariables + (varName ~> variable)

    return BlessST.variableDec(varName, varType)
  }

  def visitBTSTransition(t: BTSTransition): Unit = {

    assert(t.assertion.isEmpty)

    assert(t.sourceStates.length == 1)
    assert(t.sourceStates(0).name.length == 1)
    assert(t.destState.name.length == 1)

    val src: String = t.sourceStates(0).name(0)
    val dst: String = t.destState.name(0)

    val cond: ST = t.transitionCondition match {
      case Some(c) => visitBTSTransitionCondition(c)
      case _ => st"T"
    }

    assert(t.label.priority.isEmpty)
    val label = t.label.id.name(0)


    val actionMethodName = st"do_${label}"

    var body: ST = t.actions match {
      case Some(behaviorActions) => visitBTSBehaviorActions(behaviorActions)
      case _ => st"// empty body"
    }

    body = st"""${body}
               |
               |currentState = ${completeStateEnumName}.${dst}"""

    val doMethod = BlessST.method(actionMethodName, ISZ(), body, st"Unit")
    transitionMethods = transitionMethods + (actionMethodName.render ~> doMethod)

    assert(btsStates.get(src).get.categories.length == 1)

    val key: BTSStateCategory.Type = btsStates.get(src).get.categories(0)
    val list: ISZ[GuardedTransition] = stateMachines.getOrElse(key, ISZ())

    val gt = GuardedTransition(src, dst, cond, actionMethodName)

    stateMachines = stateMachines + (key ~> (list :+ gt))
  }

  def visitBTSBehaviorActions(actions: BTSBehaviorActions): ST = {
    assert(actions.executionOrder == BTSExecutionOrder.Sequential)

    val _a = actions.actions.map(assertedAction => visitBTSAssertedAction(assertedAction))

    return st"${(_a, "\n")}"
  }

  def visitBTSAssertedAction(action: BTSAssertedAction): ST = {
    assert(action.precondition.isEmpty)
    assert(action.postcondition.isEmpty)

    return visitBTSAction(action.action)
  }

  def visitBTSAction(action: BTSAction): ST = {
    val ret: ST = action match {
      case c: BTSIfBLESSAction => visitBTSIfBLESSAction(c)
      case c: BTSExistentialLatticeQuantification => visitBTSExistentialLatticeQuantification(c)
      case c: BTSAssignmentAction => visitBTSAssignmentAction(c)
      case c: BTSPortOutAction => visitBTSPortOutAction(c)
      case c: BTSSkipAction => st"// skip"
    }

    return ret;
  }

  def visitBTSAssignmentAction(action: BTSAssignmentAction): ST = {
    val lhs = visitBTSExp(action.lhs)

    val rhs = visitBTSExp(action.rhs)

    return st"$lhs = $rhs"
  }

  def visitBTSPortOutAction(action: BTSPortOutAction): ST = {
    val portName = Util.getLastName(action.name)

    val arg: ST = if(action.exp.nonEmpty) {
      visitBTSExp(action.exp.get)
    } else {
      st""
    }

    return BlessST.portSend(portName, arg)
  }

  def visitBTSIfBLESSAction(action: BTSIfBLESSAction): ST = {
    assert(action.availability.isEmpty)

    val _actions = action.alternatives.map(a => {
      val guard = visitBTSExp(a.guard)
      val action = visitBTSAssertedAction(a.action)

      (guard, action)
    })

    return BlessST.ifST(_actions(0), ISZOps(_actions).tail)
  }

  def visitBTSExistentialLatticeQuantification(quantification: BTSExistentialLatticeQuantification): ST = {
    assert(quantification.timeout.isEmpty) // TODO
    assert(quantification.catchClause.isEmpty) // TODO

    val localVars: ISZ[ST] = quantification.quantifiedVariables.map(m => visitBTSVariableDeclaration(m))

    val actions = visitBTSBehaviorActions(quantification.actions)

    val body = st"""$localVars
                   |
                   |$actions"""

    return st"""// DO ME TOO
               |
               |$body"""
  }

  def visitBTSTransitionCondition(condition: BTSTransitionCondition): ST = {
    condition match {
      case c: BTSDispatchCondition => return visitBTSDispatchCondition(c)
      case _ => halt("Unexpected trans cond")
    }
  }

  def visitBTSDispatchCondition(condition: BTSDispatchCondition): ST = {
    if(condition.dispatchTriggers.isEmpty) {
      assert(Util.isPeriodic(this.component))

      return st"T"
    } else {
      val ret = visitBTSDispatchConjunction(condition.dispatchTriggers(0))

      val tail = ISZOps(condition.dispatchTriggers).tail
      return ISZOps(tail).foldLeft((r: ST, t) =>
        st"$r && ${visitBTSDispatchConjunction(t)}", ret)
    }
  }

  def visitBTSDispatchConjunction(conjunction: BTSDispatchConjunction): ST = {
    val ret = visitBTSDispatchTrigger(conjunction.conjunction(0))

    val tail = ISZOps(conjunction.conjunction).tail
    return ISZOps(tail).foldLeft((r: ST, t) =>
      st"$r || ${visitBTSDispatchTrigger(t)}", ret)
  }

  def visitBTSDispatchTrigger(trigger: BTSDispatchTrigger): ST = {
    val ret: ST = trigger match {
      case BTSDispatchTriggerStop() => st"STOP"
      case BTSDispatchTriggerPort(port) =>
        val portName = Util.getLastName(port)
        st"api.get${portName}().nonEmpty"
      case BTSDispatchTriggerTimeout(ports, time) => st"TIMEOUT"
    }

    return ret
  }

  def visitBTSExp(e: BTSExp): ST = {
    val ret: ST = e match {
      case c: BTSBinaryExp => visitBTSBinaryExp(c)
      case c: BTSLiteralExp => visitBTSLiteralExp(c)
      case c: BTSNameExp => st"${Util.getLastName(c.name)}"
      case c: BTSFunctionCall => visitBTSFunctionCall(c)
    }
    return ret
  }

  def visitBTSLiteralExp(exp: BTSLiteralExp): ST = {
    val ret: ST = exp.typ match {
      case BTSLiteralType.BOOLEAN =>
        if(exp.exp == "true") st"T" else st"F"
      case BTSLiteralType.STRING => st"${exp.exp}"
    }
    return ret
  }

  def visitBTSBinaryExp(exp: BTSBinaryExp): ST = {
    val lhs = visitBTSExp(exp.lhs)
    val rhs = visitBTSExp(exp.rhs)

    val op: String = exp.op match {
      case BTSBinaryOp.AND => "&&"
      case BTSBinaryOp.ANDTHEN => "&"
      case BTSBinaryOp.OR => "||"
      case BTSBinaryOp.ORELSE => "|"

      case BTSBinaryOp.EQ => "=="
      case BTSBinaryOp.NEQ => "!="
      case BTSBinaryOp.LT=> "<"
      case BTSBinaryOp.LTE=> "<="
      case BTSBinaryOp.GT => ">"
      case BTSBinaryOp.GTE => ">="

      case BTSBinaryOp.PLUS => "+"
      case BTSBinaryOp.MINUS => "-"
      case BTSBinaryOp.DIV => "/"
      case BTSBinaryOp.MULT=> "*"
      case BTSBinaryOp.MOD => "%"
      case BTSBinaryOp.REM => "rem"

      case BTSBinaryOp.EXP => "??? EXP"
      case BTSBinaryOp.XOR => "??? XOR"
    }

    return st"($lhs $op $rhs)"
  }

  def visitBTSFunctionCall(call: BTSFunctionCall): ST = {
    val fname = Util.getLastName(call.name)

    val args: ISZ[ST] = ISZ()
    assert(call.args.isEmpty)

    return st"$fname(${(args, ",")})"
  }

  def visitBTSType(t: BTSType): String = {
    t match {
      case o: BTSClassifier =>
        return Util.getNamesFromClassifier(o.classifier, this.basePackage).component
      case _ =>
        halt(s"Need to handle type $t")
    }
  }
}

@datatype class GuardedTransition(srcState: String,
                                  dstState: String,
                                  transCondition: ST,
                                  actionMethodName: ST)