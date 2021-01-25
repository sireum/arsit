// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.bts.{BTSGen, BTSResults}
import org.sireum.hamr.arsit.templates.{ApiTemplate, StubTemplate, TestTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.ExperimentalOptions
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir._
import org.sireum.message.Reporter

@record class StubGenerator(dirs: ProjectDirectories,
                            rootSystem: AadlSystem,
                            arsitOptions: ArsitOptions,
                            symbolTable: SymbolTable,
                            types: AadlTypes,
                            reporter: Reporter,
                            previousPhase: Result) {

  var toImpl: ISZ[(ST, ST)] = ISZ()
  val basePackage: String = arsitOptions.packageName
  var seenComponents: HashSet[String] = HashSet.empty
  var resources: ISZ[Resource] = ISZ()

  val processBTSNodes: B = ExperimentalOptions.processBtsNodes(arsitOptions.experimentalOptions)

  def generate(): Result = {

    gen(rootSystem)

    return PhaseResult(previousPhase.resources() ++ resources,
      previousPhase.maxPort,
      previousPhase.maxComponent)
  }

  def gen(m: AadlComponent): Unit = {
    m match {
      case s: AadlSystem => genContainer(s)
      case s: AadlProcess => genContainer(s)

      case s: AadlThreadGroup => genThreadGroup(s)

      case _ =>
        for (_c <- m.subComponents) {
          gen(_c)
        }
    }
  }

  def genContainer(m: AadlComponent): Unit = {
    assert(m.isInstanceOf[AadlSystem] || m.isInstanceOf[AadlProcess])

    if (!m.isInstanceOf[AadlThread]) {
      genSubprograms(m)
    }

    for (c <- m.subComponents) {
      c match {
        case s: AadlSystem => genContainer(s)
        case s: AadlProcess => genContainer(s)

        case s: AadlThreadGroup => genThreadGroup(s)

        case s: AadlThread => genThread(s)

        case s: AadlDevice =>
          if (arsitOptions.devicesAsThreads) {
            genThread(s)
          }

        case s: AadlSubprogram => // ignore

        case _ =>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.component.category} component: ${c.path}")
      }
    }
  }

  def genThreadGroup(m: AadlThreadGroup): Unit = {

    for (c <- m.subComponents) {
      c match {
        case s: AadlThread => genThread(s)

        case x => halt(s"Unexpected Thread Group subcomponent: ${x}")
      }
    }
  }

  def genThread(m: AadlThreadOrDevice): Unit = {
    assert(!m.isInstanceOf[AadlDevice] || arsitOptions.devicesAsThreads)

    var imports: ISZ[ST] = ISZ()

    val names = Names(m.component, basePackage)
    val filename: String = Util.pathAppend(dirs.componentDir, ISZ(names.packagePath, s"${names.componentSingletonType}.scala"))

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m.component)

    val componentName: String = "component"

    imports = imports :+ st"${names.packageName}.{${names.componentSingletonType} => component}"

    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, z"-1000")

    val bridgeTestApis: ST = TestTemplate.bridgeTestApis(basePackage, names, ports)
    addResource(dirs.testUtilDir, ISZ(names.packagePath, s"${names.testApisName}.scala"), bridgeTestApis, T)

    val bridgeTestSuite: ST = TestTemplate.bridgeTestSuite(names.packageName, names, ports)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, s"${names.testName}.scala"), bridgeTestSuite, F)

    if (seenComponents.contains(filename)) {
      return
    }

    val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(m.component) match {
      case Some(x) => x
      case x =>
        if (CommonUtil.isDevice(m.component)) {
          Dispatch_Protocol.Periodic
        } else {
          halt(s"HAMR codegen only supports Periodic or Sporadic threads: ${x}")
        }
    }

    val blessAnnexes : ISZ[Annex] = m.component.annexes.filter(a => a.name == "behavior_specification")

    val genBlessEntryPoints: B = blessAnnexes.nonEmpty && processBTSNodes

    val bridge = StubTemplate.bridge(
      topLevelPackageName = basePackage,
      packageName = names.packageName,
      imports = imports,
      bridgeName = names.bridge,
      dispatchProtocol = dispatchProtocol,
      componentName = componentName,
      componentType = names.componentSingletonType,
      apiType = names.componentType,
      ports = ports,
      dispatchTriggers = dispatchTriggers,
      names = names,
      isBless = genBlessEntryPoints)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.bridge}.scala"), bridge, T)

    val api = ApiTemplate.api(
      names.packageName,
      basePackage,
      names,
      ports)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.api}.scala"), api, T)

    var blocks: ISZ[ST] = ISZ()

    if(!genBlessEntryPoints) {
      val componentImplBlock = StubTemplate.componentImplBlock(
        componentType = names.componentSingletonType,
        bridgeName = names.bridge,
        names = names,
        dispatchProtocol = dispatchProtocol,
        ports = ports,
        isBless = genBlessEntryPoints)
      blocks = blocks :+ componentImplBlock
    } else {
      assert(blessAnnexes.size == 1, s"Length is ${blessAnnexes.size}")

      val btsNode = blessAnnexes(0).clause.asInstanceOf[BTSBLESSAnnexClause]

      val br: BTSResults = BTSGen(
        directories = dirs,
        basePackage = basePackage,
        aadlComponent = m,
        componentNames = names,
        aadlTypes = types,
        addViz = F,
        genDebugObjects = F).process(btsNode)

      assert(br.maxPort == -1 && br.maxComponent == -1 && br.optVizEntries.isEmpty) // TODO

      blocks = blocks :+ br.component

      resources = resources ++ br.resources
    }

    genSubprograms(m) match {
      case Some(x) => blocks = blocks :+ x
      case _ =>
    }

    val componentImpl = StubTemplate.slangPreamble(
      inSlang = T,
      packageName = names.packageName,
      topLevelPackageName = basePackage,
      blocks = blocks)

    addResource(filename, ISZ(), componentImpl, genBlessEntryPoints)

    var testSuite = Util.getLibraryFile("BridgeTestSuite.scala").render
    testSuite = ops.StringOps(testSuite).replaceAllLiterally("__BASE_PACKAGE_NAME__", basePackage)
    addResource(dirs.testUtilDir, ISZ(basePackage, "BridgeTestSuite.scala"), st"${testSuite}", T)

    seenComponents = seenComponents + filename
  }

  def genSubprograms(s: AadlComponent): Option[ST] = {
    val m = s.component
    val subprograms: ISZ[(ST, ST)] = m.subComponents.filter(p => p.category == ComponentCategory.Subprogram).map(p => {
      // only expecting in or out parameters
      assert(ops.ISZOps(Util.getFeatureEnds_DEPRECATED(p.features))
        .forall(f => f.category == FeatureCategory.Parameter && f.direction != Direction.InOut))

      val methodName = CommonUtil.getLastName(p.identifier)
      val params: ISZ[String] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isInFeature(f))
        .map(param => {
          val pType = Util.getFeatureEndType(param, types)
          s"${CommonUtil.getLastName(param.identifier)} : ${Util.getDataTypeNames(pType, basePackage).referencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isOutFeature(f))
      assert(rets.size == 1)
      val rType: AadlType = Util.getFeatureEndType(rets(0), types)
      val emptyType: Option[ST] =
        if(rType == TypeUtil.EmptyType) { None() }
        else {
          val emptyValue: String = Util.getDataTypeNames(rType, basePackage).empty()
          Some(st"${emptyValue}")
        }

      val returnType = Util.getDataTypeNames(rType, basePackage).referencedTypeName

      StubTemplate.subprogram(methodName, params, returnType, emptyType)
    })

    if (subprograms.nonEmpty) {
      val names = Names(m, basePackage)
      val objectName = s"${names.componentSingletonType}_subprograms"

      val body = StubTemplate.slangBody(
        slangAnnotation = "@ext ",
        objectName = objectName,
        body = subprograms.map(m => m._1))

      if (!CommonUtil.isThread(m)) {
        val a = StubTemplate.slangPreamble(T, basePackage, names.packageName, ISZ(body))
        addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}.scala"), a, T)
      }

      val b = StubTemplate.slangPreamble(
        F,
        basePackage,
        names.packageName,
        ISZ(StubTemplate.slangBody(
          slangAnnotation = "",
          objectName = s"${objectName}_Ext",
          body = subprograms.map(m => m._2))))
      addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}_Ext.scala"), b, F)

      return Some(body)
    } else {
      return None[ST]()
    }
  }

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ Util.createResource(baseDir, paths, content, overwrite)
  }
}
