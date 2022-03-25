// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.bts.{BTSGen, BTSResults}
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.templates.{ApiTemplate, StubTemplate, TestTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit.util.ReporterUtil.reporter

@record class StubGenerator(dirs: ProjectDirectories,
                            rootSystem: AadlSystem,
                            arsitOptions: ArsitOptions,
                            symbolTable: SymbolTable,
                            types: AadlTypes,
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

    // get/form component naming used to generate file names and program identifiers for component code
    val names = Names(m.component, basePackage)
    val filename: String = Util.pathAppend(dirs.componentDir, ISZ(names.packagePath, s"${names.componentSingletonType}.scala"))
    val componentName: String = "component" // Jason: What is this used for ???

    var imports: ISZ[ST] = ISZ()
    imports = imports :+ st"${names.packageName}.{${names.componentSingletonType} => component}"

    // get model port information needed to generate APIs
    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, z"-1000")
    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m.component)

    // -------  g e n e r a t e    c o m p o n e n t    u n i t    t e s t s  ----------
    // -- Jason: why is this here?  Can we locate it with the unit test infrastructure code below
    val bridgeTestApis: ST = TestTemplate.bridgeTestApis(basePackage, names, ports)
    addResource(dirs.testUtilDir, ISZ(names.packagePath, s"${names.testApisName}.scala"), bridgeTestApis, T)

    val bridgeTestSuite: ST = TestTemplate.bridgeTestSuite(names.packageName, names, ports)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, s"${names.testName}.scala"), bridgeTestSuite, F)

    if (seenComponents.contains(filename)) {
      return
    }

    // ------- component infrastructure:  g e n e r a t e    b r i d g e   f i l e  -------------------

    val dispatchProtocol: Dispatch_Protocol.Type = m.dispatchProtocol

    // experimental code used to generate component structures dedicated to BLESS/BA state transition systems
    val btsAnnexes : ISZ[AnnexInfo] =
      symbolTable.annexInfos.get(m).get.filter(m => m.isInstanceOf[BTSAnnexInfo])
    val genBlessEntryPoints: B = btsAnnexes.nonEmpty && processBTSNodes

    // generate code for component bridge (container)
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

    // -------- component infrastructure: g e n e r a t e    p o r t    A P I s   f i l e  -------------

    // generate contract information for *integration constraints* on ports.
    //  Integration constraints are invariants on port values.
    //  They apply to all entry points and
    //  they are independent of the component dispatch mode and
    //  structure of compute entry points.
    //  Therefore these types of contracts are represented as invariants on ports instead of
    //  pre/post-conditions on thread entry points.
    val integrationContracts = GumboGen.processIntegrationContract(m, symbolTable, basePackage)

    // generate port API file for component
    val api = ApiTemplate.api(
      names.packageName,
      basePackage,
      names,
      ports,
      integrationContracts)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.api}.scala"), api, T)

    //---- component application logic:  g e n e r a t e   e n t r y    p o i n t    s k e l e t o n s   ----

    // --- BLESS version of entry points
    var blocks: ISZ[ST] = ISZ()

    if(!genBlessEntryPoints) {
      // --- generate HAMR syle entry points
      val componentImplBlock = StubTemplate.componentImplBlock(
        componentType = names.componentSingletonType,
        bridgeName = names.bridge,
        names = names,
        dispatchProtocol = dispatchProtocol,
        ports = ports,
        isBless = genBlessEntryPoints,
        excludeComponentImpl = arsitOptions.excludeImpl
      )
      blocks = blocks :+ componentImplBlock
    } else {
      // --- generate BLESS style entry points
      assert(btsAnnexes.size == 1, s"Expecting exactly one BTS annex but found ${btsAnnexes.size}")

      val btsAnnexInfo = btsAnnexes(0).asInstanceOf[BTSAnnexInfo]

      val br: BTSResults = BTSGen(
        directories = dirs,
        basePackage = basePackage,
        aadlComponent = m,
        componentNames = names,

        symbolTable = symbolTable,
        btsSymbolTable = btsAnnexInfo.btsSymbolTable,
        aadlTypes = types,

        addViz = F,
        genDebugObjects = F).process(btsAnnexInfo.annex)

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

    //---- g e n e r a t e   u n i t    t e s t    i n f r a s t r u c t u r e  ----

    // add BridgeTestSuite class in test/util directory.
    // The unit test skeleton file for each thread inherits from this class.
    // BridgeTestSuite adds test set and tear down infrastructure (e.g., to initial the
    //  state of a particular thread component) that will be called before and after the
    //  user code in each unit test.
    var testSuite = Util.getLibraryFile("BridgeTestSuite.scala").render
    testSuite = ops.StringOps(testSuite).replaceAllLiterally("__BASE_PACKAGE_NAME__", basePackage)
    addResource(dirs.testUtilDir, ISZ(basePackage, "BridgeTestSuite.scala"), st"${testSuite}", T)

    // add GUMBOCheck library file in test/util directory to hold value generators for each AADL Base Type
    // GUMBOCheck auto-generated and developer-supplied generators use these.
    var baseTypeGen = Util.getLibraryFile("BaseTypeGen.scala").render
    baseTypeGen = ops.StringOps(baseTypeGen).replaceAllLiterally("__BASE_PACKAGE_NAME__", basePackage)
    addResource(dirs.testUtilDir, ISZ(basePackage, "BaseTypeGen.scala"), st"${baseTypeGen}", T)

    // add GUMBOCheck library file in test/util directory to statically configurable bounds and
    //  profiles for configuring GUMBOCheck
    var gumboCheck = Util.getLibraryFile("GUMBOCheck.scala").render
    gumboCheck = ops.StringOps(gumboCheck).replaceAllLiterally("__BASE_PACKAGE_NAME__", basePackage)
    addResource(dirs.testUtilDir, ISZ(basePackage, "GUMBOCheck.scala"), st"${gumboCheck}", T)

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
          s"${CommonUtil.getLastName(param.identifier)} : ${Util.getDataTypeNames(pType, basePackage).qualifiedReferencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isOutFeature(f))
      assert(rets.size <= 1, s"Expecting a single out param but found ${rets.size}")


      val (returnType, exampleType) : (Option[String], Option[ST]) =
        if(rets.isEmpty) {
          (None(), None())
        }
        else {
          val rType: AadlType = Util.getFeatureEndType(rets(0), types)
          val _exampleValue: String = Util.getDataTypeNames(rType, basePackage).example()
          val returnType = Util.getDataTypeNames(rType, basePackage).qualifiedReferencedTypeName

          (Some(returnType), Some(st"${_exampleValue}"))
        }

      StubTemplate.subprogram(methodName, params, returnType, exampleType)
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
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(baseDir, paths), content, overwrite)
  }
}
