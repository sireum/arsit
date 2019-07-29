// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

@record class BlessGen(basePackage: String,
                       componentDirectory: String,
                       component: Component,
                       componentNames: Names,
                       aadlTypes: AadlTypes,
                       addViz: B,
                       genDebugObjects: B) {

  val completeStateEnumName: String = s"${componentNames.componentImpl}_CompleteStates"

  var btsStates: Map[String, BTSStateDeclaration] = Map.empty

  var declaredStates: ISZ[String] = ISZ()
  var initialState: String = ""

  var globalVariables: Map[String, GlobalVar] = Map.empty

  var completeStateMachines: Map[BTSStateCategory.Type, ISZ[GuardedTransition]] = Map.empty
  var executeStateMachines: Map[String, ISZ[GuardedTransition]] = Map.empty

  var transitionMethods: Map[String, ST] = Map.empty

  var subprograms: Map[Name, Subprogram] = Map.empty

  var vizEntries: ISZ[ST] = ISZ()

  def process(a: BTSBLESSAnnexClause): BlessResults = {
    if(a.assertions.nonEmpty) {
      println(s"Need to handle assertions")
    } // TODO
    if(a.invariant.nonEmpty){
      println(s"Need to handle invariants")
    } // TODO

    val packageName: String = componentNames.packageName
    val imports: ISZ[ST] = ISZ()
    //val states: ISZ[ST] = a.states.map(s => visitStates(s))
    a.states.map(s => visitBTSStateDeclaration(s))

    val componentName = componentNames.componentImpl
    val bridgeName = componentNames.bridge

    for(v <- a.variables){
      val gv = visitBTSVariableDeclaration(v)
      globalVariables = globalVariables + (gv.slangName ~> gv)
    }

    a.transitions.map(t => visitBTSTransition(t))

    if(addViz) {

      val trans = a.transitions.map(bts => {
        val label = Util.getLastName(bts.label.id)
        val src = Util.getLastName(bts.sourceStates(0))
        val dst = Util.getLastName(bts.destState)

        BlessST.vizTrans(src, dst, label)
      })

      val stateDecs: ISZ[ST] = a.states.map(m => {
        val stateTypes = m.categories.map(m => BlessST.vizStateType(m))
        BlessST.vizCreate(Util.getLastName(m.id), st"state desc", stateTypes)
      })

      val stateNames: ISZ[ST] = a.states.map(m => st"${Util.getLastName(m.id)}")

      val id = st"Arch.${Util.getName(component.identifier)}.id".render

      val viz = BlessST.vizBuildSm(stateDecs, id, componentNames.componentImpl, initialState, stateNames, trans)

      vizEntries = vizEntries :+ viz
    }


    // DONE WALKING
    var methods = completeStateMachines.entries.map(entry => buildSM(entry._1, entry._2))

    methods = methods ++ executeStateMachines.entries.map(entry => buildExecutionStateMachine(entry._1, entry._2))

    methods = methods ++ transitionMethods.values

    methods = methods :+ BlessST.tranpilerWorkaroundContains()

    var extensions: ISZ[ST] = ISZ()

    if(subprograms.nonEmpty) {
      extensions = extensions :+ BlessST.externalObjectSlang(extSubprogramObject(F),
        subprograms.values.map(sp => st"${sp.extMethod}$$"))


      val extName = extSubprogramObject(T)
      val eo = BlessST.externalObjectJVM(st"${packageName}", extName, ISZ(), subprograms.values.map(sp =>
        st"""${sp.extMethod} {
            |  // intentionally empty, perhaps return dummy value
            |}"""))

      Util.writeFileString(st"${componentDirectory}/${extName}.scala".render, eo, false)
    }

    val globalVars: ISZ[ST] = if(genDebugObjects) {
      val fields = globalVariables.values.map(m => st"var ${m.slangName}: ${m.slangTypeName}")
      val names = globalVariables.values.map(m => st"${m.slangName}")
      val initExps = globalVariables.values.map(m => m.initExp)

      extensions = extensions :+ BlessST.debugObject(componentNames.componentImpl, fields, names)
      ISZ(BlessST.debugObjectDec(componentNames.componentImpl, initExps))
    } else {
      globalVariables.values.map(v => BlessST.variableDec(v.slangName, v.slangTypeName, v.initExp))
    }

    val completeStates = a.states.filter(s => !isExecuteState(Util.getLastName(s.id))).map(m => st"'${Util.getLastName(m.id)}")

    val ci = BlessST.main(packageName, imports, completeStateEnumName, completeStates, componentName, bridgeName, initialState,
      globalVars, methods, extensions)

    return BlessResults(ci, vizEntries)
  }

  def buildExecutionStateMachine(stateName: String, transitions: ISZ[GuardedTransition]): ST = {
    val t = transitions.map(m => (m.transCondition, st"${m.actionMethodName}()"))

    var body = BlessST.ifST(t(0), ISZOps(t).tail)

    if(addViz) {
      body = st"""${BlessST.vizCallTransitionWithStateName(basePackage, stateName)}
                 |
                 |${body}"""
    }

    val ret = BlessST.method(executeStateMethodName(stateName), ISZ(), body, st"Unit")
    return ret
  }

  def buildSM(t: BTSStateCategory.Type, l: ISZ[GuardedTransition]) : ST = {

    if(t == BTSStateCategory.Initial) {
      val inits = l.filter(f => f.srcState == this.initialState).map(m =>
        (m.transCondition, st"${m.actionMethodName}()"))
      assert(inits.length == 1)

      var body = BlessST.ifST(inits(0), ISZOps(inits).tail)

      if(addViz) {
        body = st"""${BlessST.vizCallCreateStateMachines(basePackage)}
                   |
                   |$body"""
      }

      if(genDebugObjects) {
        body = st"""${BlessST.debugRegister()}
                   |
                   |$body"""
      }

      return BlessST.method(st"Initialize_Entrypoint", ISZ(), body, st"Unit")
    } else {
      var cases: ISZ[ST] = ISZ()

      for(e <- btsStates.entries.filter(f => isCompleteState(f._1) || isFinalState(f._1))) {
        val stateName = e._1
        val gts: ISZ[(ST, ST)] = l.filter(f => f.srcState == stateName).map(m =>
          (m.transCondition, st"${m.actionMethodName}()")
        )
        val options: ST = if(gts.isEmpty) {
          st"// no transitions defined leaving this state"
        } else {
          BlessST.ifST(gts(0), ISZOps(gts).tail)
        }

        val _case = st"""case ${completeStateEnumName}.${stateName} =>
                        |  $options"""

        cases = cases :+ _case

      }

      val body = st"""currentState match {
                     |  ${(cases, "\n")}
                     |  case _ => halt(s"Unexpected: $$currentState")
                     |}"""

      return BlessST.method(st"Compute_Entrypoint", ISZ(BlessST.dispatchedPortsDec()), body, st"Unit")
    }
  }

  def visitBTSStateDeclaration(state: BTSStateDeclaration): ST = {
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

  def visitBTSVariableDeclaration(variable: BTSVariableDeclaration): GlobalVar = {
    val typ = variable.category // TODO
    val arraySize = variable.arraySize // TODO
    val variableAssertion = variable.variableAssertion // TODO

    val varName: String = Util.getLastName(variable.name)

    var isEnum: B = F

    val aadlType: AadlType = variable.varType match {
      case BTSClassifier(classifier) =>
        val r = aadlTypes.typeMap.get(classifier.name).get
        isEnum = r.isInstanceOf[EnumType]
        r
      case _ => halt("finish this ${variable}")
    }

    val assignExp: ST = if(variable.assignExpression.nonEmpty) {
      visitBTSExp(variable.assignExpression.get)
    } else {
      // emit default value
      st"${initType(aadlType)}"
    }

    var varType:ST = visitBTSType(variable.varType)
    if(isEnum) {
      varType = st"${varType}.Type"
    }

    return GlobalVar(varName, varType, aadlType, assignExp)
  }

  def initType(a: AadlType): ST = {
    a match {
      case c: ArrayType => halt("")

      case c: BaseType => return st"0f"

      case c: EnumType => return st"${c.slangTypeName}.${c.values(0)}"

      case c: RecordType =>

        val fields = c.fields.values.map(m => initType(m))

        return st"${c.slangTypeName}(${(fields, ", ")})"

      case _ =>
        halt("TODO")
    }
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
    val transLabel = t.label.id.name(0)

    val actionMethodName = st"do_${transLabel}"

    var actions: ST = t.actions match {
      case Some(behaviorActions) => visitBTSBehaviorActions(behaviorActions)
      case _ => st"// empty actions"
    }

    if(isExecuteState(dst)) {
      // going to an execute state
      actions = st"""${actions}
                    |
                    |${executeStateMethodName(dst)}()"""
    } else {
      // going to a complete state
      actions = st"""${actions}
                    |
                    |currentState = ${completeStateEnumName}.${dst}"""

      if(addViz) {
        actions = st"""${actions}
                      |${BlessST.vizCallTransition(basePackage)}"""
      }
    }

    val doMethod = BlessST.method(actionMethodName, ISZ(), actions, st"Unit")
    transitionMethods = transitionMethods + (actionMethodName.render ~> doMethod)

    assert(btsStates.get(src).get.categories.length == 1)


    if(isCompleteState(src) || isFinalState(src) || isInitialState(src)) {
      // transitioning to a complete state
      val gt = GuardedTransition(src, dst, cond, actionMethodName)

      val key: BTSStateCategory.Type = btsStates.get(src).get.categories(0) // TODO assumes single state type per state
      val list: ISZ[GuardedTransition] = completeStateMachines.getOrElse(key, ISZ())
      completeStateMachines = completeStateMachines + (key ~> (list :+ gt))
    } else {
      // transitioning to an execute state
      assert(isExecuteState(src))

      var actions = actionMethodName
      /*
      if(addViz) {
        actions = st"""${BlessST.vizCallTransitionWithStateName(basePackage, src)}
                      |
                      |$actions"""
      }
      */
      val gt = GuardedTransition(src, dst, cond, actions)

      val key = src
      val list: ISZ[GuardedTransition] = executeStateMachines.getOrElse(key, ISZ())
      executeStateMachines = executeStateMachines + (key ~> (list :+ gt))
    }
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
      case c: BTSIfBAAction => visitBTSIfBAAction(c)
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


  def visitBTSIfBAAction(action: BTSIfBAAction): ST = {
    val ifb = visitBTSConditionalActions(action.ifBranch)

    val elsifs = action.elseIfBranches.map(e => visitBTSConditionalActions(e)).map(x => st"else ${x}")

    val elseb: ST = if(action.elseBranch.nonEmpty) {
      visitBTSBehaviorActions(action.elseBranch.get)
    } else {
      st""
    }

    return st"""${ifb}
               |${(elsifs, "\n")}
               |${elseb}"""
  }

  def visitBTSConditionalActions(actions: BTSConditionalActions): ST = {

    val cond = visitBTSExp(actions.cond);

    val body = visitBTSBehaviorActions(actions.actions)

    return st"""if($cond) {
               |  $body
               |}"""
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

    val localVars: ISZ[ST] = quantification.quantifiedVariables.map(m => {
      val gt = visitBTSVariableDeclaration(m)
      BlessST.variableDec(gt.slangName, gt.slangTypeName, gt.initExp)
    })

    val actions = visitBTSBehaviorActions(quantification.actions)

    val body = st"""${(localVars, "\n")}
                   |
                   |$actions"""

    return body
  }

  def visitBTSTransitionCondition(condition: BTSTransitionCondition): ST = {
    condition match {
      case c: BTSDispatchCondition => return visitBTSDispatchCondition(c)
      case c: BTSExecuteConditionExp => return visitBTSExecuteConditionExp(c)
      case _ => halt("Unexpected trans cond")
    }
  }

  def visitBTSExecuteConditionExp(condition: BTSExecuteConditionExp): ST = {
    return visitBTSExp(condition.exp)
  }

  def visitBTSDispatchCondition(condition: BTSDispatchCondition): ST = {
    if(condition.dispatchTriggers.isEmpty) {
      assert(Util.isPeriodic(this.component))

      return st"T"
    } else {
      val ret = visitBTSDispatchConjunction(condition.dispatchTriggers(0))

      val tail = ISZOps(condition.dispatchTriggers).tail
      return ISZOps(tail).foldLeft((r: ST, t) =>
        st"$r || ${visitBTSDispatchConjunction(t)}", ret)
    }
  }

  def visitBTSDispatchConjunction(conjunction: BTSDispatchConjunction): ST = {
    val ret = visitBTSDispatchTrigger(conjunction.conjunction(0))

    val tail = ISZOps(conjunction.conjunction).tail
    return ISZOps(tail).foldLeft((r: ST, t) =>
      st"$r && ${visitBTSDispatchTrigger(t)}", ret)
  }

  def visitBTSDispatchTrigger(trigger: BTSDispatchTrigger): ST = {
    val ret: ST = trigger match {
      case BTSDispatchTriggerStop() => st"STOP"
      case BTSDispatchTriggerPort(port) =>
        val name = Util.getLastName(port)

        val d = this.component.features.filter(f => f.identifier.name == port.name )
        assert(d.size == 1)
        if(!Util.isEventPort(d(0).asInstanceOf[FeatureEnd])){
          println(s"WARNING: Processing dispatch trigger in ${componentNames.componentImpl}.  '${name}' is not an event port so will not be dispatched")
        }

        BlessST.portQuery(name)
      case BTSDispatchTriggerTimeout(ports, time) => st"TIMEOUT"
    }

    return ret
  }

  def visitBTSExp(e: BTSExp): ST = {
    val ret: ST = e match {
      case c: BTSAccessExp => visitBTSAccessExp(c)
      case c: BTSBinaryExp => visitBTSBinaryExp(c)
      case c: BTSLiteralExp => visitBTSLiteralExp(c)
      case c: BTSNameExp => visitBTSNameExp(c)
      case c: BTSFunctionCall => visitBTSFunctionCall(c)
    }
    return ret
  }

  def visitBTSNameExp(e: BTSNameExp): ST = {
    val n = Util.getLastName(e.name)
    val st: ST = if(e.name.name.size > 1) {

      val _feature = component.features.filter(f => f.identifier.name == e.name.name)
      if(_feature.nonEmpty){
        assert(_feature.size == 1)
        val feature = _feature(0).asInstanceOf[FeatureEnd]

        assert(Util.isInFeature(feature))
        assert(Util.isDataPort(feature) || Util.isEventDataPort(feature))

        return BlessST.portGet(n)
      } else {
        halt(s"Need to handle ${e}")
      }
    } else {
      assert(e.name.name.size == 1)

      if (genDebugObjects && isGlobalVariables(n)) {
        BlessST.debugObjectAccess(n)
      }
      else {
        st"${n}"
      }
    }

    return st
  }

  def visitBTSAccessExp(exp: BTSAccessExp): ST = {
    val e = visitBTSExp(exp.exp)
    val a = exp.attributeName

    return st"${e}.${a}"
  }

  def visitBTSLiteralExp(exp: BTSLiteralExp): ST = {
    val ret: ST = exp.typ match {
      case BTSLiteralType.BOOLEAN =>
        if(exp.exp == "true") st"T" else st"F"
      case BTSLiteralType.STRING =>
        val so = StringOps(exp.exp)
        if(so.contains("#Enumerators")) {
          // TODO need to fix bless grammar
          st"${so.replaceAllLiterally("#Enumerators", "")}"
        } else {
          st"${exp.exp}"
        }
      case BTSLiteralType.INTEGER =>
        st"${exp.exp}"
      case BTSLiteralType.FLOAT =>
        st"${exp.exp}f"
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
    val sb = resolveSubprogram(call.name)

    assert(sb.params.size == call.args.size)

    var args: ISZ[ST] = ISZ()
    for(i <- 0 until call.args.size) {
      val pair = call.args(0)
      val pname = Util.getLastName(pair.paramName.get)

      assert(pname == sb.params(i)._1) // TODO

      val exp = visitBTSExp(pair.exp.get)

      args = args :+ exp
    }

    return st"${sb.qualifiedName}(${(args, ",")})"
  }

  def visitBTSType(t: BTSType): ST = {
    t match {
      case o: BTSClassifier =>
        return st"${Util.getNamesFromClassifier(o.classifier, this.basePackage).component}"
      case _ =>
        halt(s"Need to handle type $t")
    }
  }


  def resolveSubprogram(name: Name): Subprogram = {

    if(!subprograms.contains(name)) {
      val _c = component.subComponents.filter(sc => sc.identifier.name == name.name)
      assert(_c.size == 1)
      val subprog = _c(0)
      val featureSize = subprog.features.size

      assert(subprog.category == ComponentCategory.Subprogram)
      assert(subprog.features.size >= 1)

      val methodName = st"${Util.getLastName(subprog.identifier)}"
      var i = 0
      val params: ISZ[(ST, ST)] = subprog.features.map(f => {
        f match {
          case fe: FeatureEnd =>
            // last param must be the return type, all others must be in
            assert(if (i == featureSize - 1) fe.direction == Direction.Out else fe.direction == Direction.In)

            val paramName = Util.getLastName(fe.identifier)
            var paramType: ST = st"${Util.getNamesFromClassifier(fe.classifier.get, basePackage).component}"
            if(isEnum(fe.classifier.get)) {
              paramType = st"${paramType}.Type"
            }

            i = i + 1

            (st"$paramName", paramType)
          case _ => halt(s"unexpected param ${f}")
        }
      })

      val soParams = ISZOps(params)
      val last = soParams.last
      val slangParams = soParams.dropRight(1)

      val extMethod = BlessST.extMethod(methodName, slangParams.map(s => st"${s._1} : ${s._2}"), last._2)

      val qualifiedName = st"${extSubprogramObject(F)}.${methodName}"

      subprograms = subprograms + (name ~> Subprogram(qualifiedName, slangParams.map(s => (s._1.render, s._2.render)), extMethod))
    }

    return subprograms.get(name).get
  }

  def isInitialState(str: String): B = {
    return isStateType(str, BTSStateCategory.Initial)
  }

  def isFinalState(str: String): B = {
    return isStateType(str, BTSStateCategory.Final)
  }

  def isCompleteState(name: String): B = {
    return isStateType(name, BTSStateCategory.Complete)
  }

  def isExecuteState(name: String): B = {
    return isStateType(name, BTSStateCategory.Execute)
  }

  def isStateType(name: String, typ: BTSStateCategory.Type): B = {
    val states = btsStates.entries.filter(e => e._1 == name)
    assert(states.size == 1)

    val state: BTSStateDeclaration = states(0)._2

    return ISZOps(state.categories).contains(typ)
  }

  def executeStateMethodName(s: String): ST = {
    return st"executeState_${s}"
  }

  def isEnum(c: Classifier): B = {
    aadlTypes.typeMap.get(c.name) match {
      case Some(c: EnumType)=> return T
      case _ => return F
    }
  }

  def isGlobalVariables(name: String): B = {
    return globalVariables.get(name).nonEmpty
  }

  def extSubprogramObject(isJvm: B): ST = {
    return st"${componentNames.componentImpl}_Subprograms${if(isJvm) "_Ext" else ""}"
  }
}

@datatype class Subprogram(qualifiedName: ST,
                           params: ISZ[(String, String)], // paramName : paramType
                           extMethod: ST)

@datatype class GuardedTransition(srcState: String,
                                  dstState: String,
                                  transCondition: ST,
                                  actionMethodName: ST)

@datatype class GlobalVar(slangName: String,
                          slangTypeName: ST,
                          typ: AadlType,
                          initExp: ST
                         )

@datatype class BlessResults(component: ST,
                             optVizEntries: ISZ[ST])