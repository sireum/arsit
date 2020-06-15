package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates.{StubTemplate, StringTemplate}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes

class StubGenerator(dirs: ProjectDirectories,
                    m: Aadl,
                    arsitOptions: Cli.ArsitOption,
                    symbolTable: SymbolTable,
                    types: AadlTypes,
                    previousPhase: Result)  {

  var toImpl : ISZ[(ST, ST)] = ISZ()
  val basePackage: String = arsitOptions.packageName
  var seenComponents : HashSet[String] = HashSet.empty
  var resources: ISZ[Resource] = ISZ()

  def generator() : Result = {

    for(c <- m.components) {
      gen(c)
    }

    return PhaseResult(previousPhase.resources() ++ resources,
      previousPhase.maxPort,
      previousPhase.maxComponent)
  }

  def gen(m: Component) : Unit = {
    m.category match {
      case ComponentCategory.System => genContainer(m)
      case ComponentCategory.Process => genContainer(m)

      case ComponentCategory.ThreadGroup => genThreadGroup(m)

      case _ =>
        for (_c <- m.subComponents) {
          gen(_c)
        }
    }
  }

  def genContainer(m: Component) : Unit = {
    assert(m.category == ComponentCategory.Process || m.category == ComponentCategory.System)

    if(!CommonUtil.isThread(m)) {
      genSubprograms(m)
    }

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.System => genContainer(c)
        case ComponentCategory.Process => genContainer(c)

        case ComponentCategory.ThreadGroup => genThreadGroup(c)

        case ComponentCategory.Thread => genThread(c)
        case ComponentCategory.Device if arsitOptions.devicesAsThreads => genThread(c)

        case ComponentCategory.Subprogram => // ignore

        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor | ComponentCategory.VirtualProcessor =>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.category} component: ${CommonUtil.getName(c.identifier)}")

        case _ => throw new RuntimeException(s"Not handling ${c.category}: ${m}")
      }
    }
  }

  def genThreadGroup(m: Component) : Unit = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for (c <- m.subComponents) {
      assert(c.category == ComponentCategory.Thread)
      genThread(c)
    }
  }

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ SlangUtil.createResource(baseDir, paths, content, overwrite)
  }

  def genThread(m: Component) : Unit = {
    assert(CommonUtil.isDevice(m) || CommonUtil.isThread(m))

    val names = Names(m, basePackage)
    val filename: String = SlangUtil.pathAppend(dirs.componentDir, ISZ(names.packagePath, s"${names.componentImpl}.scala"))

    val dispatchTriggers: Option[ISZ[String]] = SlangUtil.getDispatchTriggers(m)

    val componentName = "component"
    val ports: ISZ[Port] = SlangUtil.getPorts(m, types, basePackage, z"-1000")

    val bridgeTestSuite: ST = StubTemplate.bridgeTestSuite(basePackage, names, ports)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, s"${names.testName}.scala"), bridgeTestSuite, F)

    if(seenComponents.contains(filename)) {
      return
    }

    val dispatchProtocol = PropertyUtil.getDispatchProtocol(m) match {
      case Some(x) => x
      case x =>
        if(CommonUtil.isDevice(m)) {
          Dispatch_Protocol.Periodic
        } else {
          halt(s"HAMR codegen only supports Periodic or Sporadic threads: ${x}")
        }
    }

    val bridge = StubTemplate.bridge(
      basePackage,
      names.packageName,
      names.bridge,
      dispatchProtocol,
      componentName,
      names.component,
      names.componentImpl,
      ports,
      dispatchTriggers,
      types,
      arsitOptions.bless)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.bridge}.scala"), bridge, T)

    if(!arsitOptions.bless) {
      val component = StubTemplate.componentTrait(
        basePackage,
        names.packageName,
        dispatchProtocol,
        names.component,
        names.bridge,
        ports)
      addResource(dirs.componentDir, ISZ(names.packagePath, s"${names.component}.scala"), component, T)
    }

    val block = StubTemplate.componentImplBlock(
      names.component,
      names.bridge,
      names.componentImpl,
      dispatchProtocol,
      ports,
      arsitOptions.bless)

    val componentImpl = StubTemplate.slangPreamble(T, names.packageName, basePackage,
      genSubprograms(m) match {
        case Some(x) => ISZ(block, x)
        case _ => ISZ(block)
      })

    addResource(filename, ISZ(), componentImpl, F)

    var testSuite = SlangUtil.getLibraryFile("BridgeTestSuite.scala").render
    testSuite = ops.StringOps(testSuite).replaceAllLiterally("__PACKAGE_NAME__", names.packageName)
    testSuite = ops.StringOps(testSuite).replaceAllLiterally("__BASE_PACKAGE_NAME__", basePackage)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, "BridgeTestSuite.scala"), st"${testSuite}", T)

    seenComponents = seenComponents + filename
  }

  def genSubprograms(m: Component) : Option[ST] = {
    val subprograms: ISZ[(ST, ST)] = m.subComponents.filter(p => p.category == ComponentCategory.Subprogram).map(p => {
      // only expecting in or out parameters
      SlangUtil.getFeatureEnds(p.features).elements.forall(f => f.category == FeatureCategory.Parameter && f.direction != Direction.InOut)

      val methodName = CommonUtil.getLastName(p.identifier)
      val params: ISZ[String] = SlangUtil.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isInFeature(f))
        .map(param => {
          val pType = SlangUtil.getFeatureEndType(param, types)
          s"${CommonUtil.getLastName(param.identifier)} : ${SlangUtil.getDataTypeNames(pType, basePackage).referencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = SlangUtil.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isOutFeature(f))
      assert(rets.size == 1)
      val rType = SlangUtil.getFeatureEndType(rets(0), types)
      val returnType = SlangUtil.getDataTypeNames(rType, basePackage).referencedTypeName

      StubTemplate.subprogram(methodName, params, returnType)
    })

    if (subprograms.nonEmpty) {
      val names = Names(m, basePackage)
      val objectName = s"${names.component}_subprograms"

      val body = StubTemplate.slangBody("@ext ", objectName, subprograms.map(_._1))

      if(!CommonUtil.isThread(m)) {
        val a = StubTemplate.slangPreamble(T, basePackage, names.packageName, ISZ(body))
        addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}.scala"), a, T)
      }

      val b = StubTemplate.slangPreamble(
        F,
        basePackage,
        names.packageName,
        ISZ(StubTemplate.slangBody("", s"${objectName}_Ext", subprograms.map(_._2))))
      addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}_Ext.scala"), b, F)

      return Some(body)
    } else {
      return None[ST]()
    }
  }
}

object StubGenerator {
  def apply(dirs: ProjectDirectories, m: Aadl, o: Cli.ArsitOption, symbolTable: SymbolTable, types: AadlTypes, previousPhase: Result) =
    new StubGenerator(dirs, m, o, symbolTable, types, previousPhase).generator()
}