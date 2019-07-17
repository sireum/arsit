// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

@record class BlessGen(basePackage: String,
                       component: Component,
                       componentNames: Names,
                       aadlTypes: AadlTypes) {

  val completeStateEnumName: String = s"${componentNames.componentImpl}_CompleteStates"

  var btsStates: Map[String, BTSStateDeclaration] = Map.empty

  var declaredStates: ISZ[String] = ISZ()
  var initialState: String = ""

  var globalVariables: Map[String, BTSVariableDeclaration] = Map.empty

  var stateMachines: Map[BTSStateCategory.Type, ISZ[GuardedTransition]] = Map.empty

  var transitionMethods: Map[String, ST] = Map.empty

  var subprograms: Map[Name, Subprogram] = Map.empty

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

    var methods = stateMachines.entries.map(entry => buildSM(entry._1, entry._2))

    methods = methods ++ this.transitionMethods.values

    var extensions: ISZ[ST] = ISZ()

    if(subprograms.nonEmpty) {
      extensions = extensions :+ BlessST.externalObject(extSubprogramObject(), subprograms.values.map(sp => sp.extMethod))
    }

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
    val arraySize = variable.arraySize // TODO
    val variableAssertion = variable.variableAssertion // TODO

    val varName: String = Util.getLastName(variable.name)

    globalVariables = globalVariables + (varName ~> variable)

    var isEnum: B = F

    val assignExp: ST = if(variable.assignExpression.nonEmpty) {
      visitBTSExp(variable.assignExpression.get)
    } else {
      // emit default value
      variable.varType match {
        case BTSClassifier(classifier) =>
          aadlTypes.typeMap.get(classifier.name) match {
            case Some(o) =>
              isEnum = o.isInstanceOf[EnumType]
              st"${initType(o)} // dummy value"
            case _ => halt(s"not in type map $classifier")
          }
        case _ => st"finish this ${variable}"
      }
    }

    var varType:ST = visitBTSType(variable.varType)
    if(isEnum) {
      varType = st"${varType}.Type"
    }

    return BlessST.variableDec(varName, varType, assignExp)
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

    val localVars: ISZ[ST] = quantification.quantifiedVariables.map(m => visitBTSVariableDeclaration(m))

    val actions = visitBTSBehaviorActions(quantification.actions)

    val body = st"""${(localVars, "\n")}
                   |
                   |$actions"""

    return body
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
        BlessST.portQuery(portName)
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
      st"${n}"
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

      val qualifiedName = st"${extSubprogramObject}.${methodName}"

      subprograms = subprograms + (name ~> Subprogram(qualifiedName, slangParams.map(s => (s._1.render, s._2.render)), extMethod))
    }

    return subprograms.get(name).get
  }

  def isEnum(c: Classifier): B = {
    aadlTypes.typeMap.get(c.name) match {
      case Some(c: EnumType)=> return T
      case _ => return F
    }
  }

  def extSubprogramObject(): ST = {
    return st"${componentNames.componentImpl}_Subprograms"
  }
}

@datatype class Subprogram(qualifiedName: ST,
                           params: ISZ[(String, String)], // paramName : paramType
                           extMethod: ST)

@datatype class GuardedTransition(srcState: String,
                                  dstState: String,
                                  transCondition: ST,
                                  actionMethodName: ST)