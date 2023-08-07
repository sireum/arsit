// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.Util.nameProvider
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.BehaviorEntryPointObjectContributions
import org.sireum.hamr.arsit.plugin.{ArsitPlugin, BehaviorEntryPointElementProvider, BehaviorEntryPointProviders}
import org.sireum.hamr.arsit.templates.{ApiTemplate, StubTemplate, TestTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{FileResource, Marker}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir._

@record class StubGenerator(dirs: ProjectDirectories,
                            rootSystem: AadlSystem,
                            arsitOptions: ArsitOptions,
                            symbolTable: SymbolTable,
                            types: AadlTypes,
                            previousPhase: Result) {

  val basePackage: String = arsitOptions.packageName
  var seenComponents: HashSet[String] = HashSet.empty
  var resources: ISZ[FileResource] = ISZ()

  def generate(plugins: MSZ[Plugin]): Result = {

    gen(rootSystem, plugins)

    return ArsitResult(
      resources = previousPhase.resources() ++ resources,
      auxResources = previousPhase.auxResources,
      maxPort = previousPhase.maxPort,
      maxComponent = previousPhase.maxComponent,
      maxConnection = previousPhase.maxConnection
    )
  }

  def gen(m: AadlComponent, plugins: MSZ[Plugin]): Unit = {
    m match {
      case s: AadlSystem => genContainer(s, plugins)
      case s: AadlProcess => genContainer(s, plugins)

      case s: AadlThreadGroup => genThreadGroup(s, plugins)

      case _ =>
        for (_c <- m.subComponents) {
          gen(_c, plugins)
        }
    }
  }

  def genContainer(m: AadlComponent, plugins: MSZ[Plugin]): Unit = {
    assert(m.isInstanceOf[AadlSystem] || m.isInstanceOf[AadlProcess])

    for (c <- m.subComponents) {
      c match {
        case s: AadlSystem => genContainer(s, plugins)
        case s: AadlProcess => genContainer(s, plugins)

        case s: AadlThreadGroup => genThreadGroup(s, plugins)

        case s: AadlThread => genThread(s, plugins)

        case s: AadlDevice =>
          if (arsitOptions.devicesAsThreads) {
            genThread(s, plugins)
          }

        case s: AadlSubprogram => // ignore

        case _ =>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.component.category} component: ${c.path}")
      }
    }
  }

  def genThreadGroup(m: AadlThreadGroup, plugins: MSZ[Plugin]): Unit = {

    for (c <- m.subComponents) {
      c match {
        case s: AadlThread => genThread(s, plugins)

        case x => halt(s"Unexpected Thread Group subcomponent: ${x}")
      }
    }
  }

  def genThread(m: AadlThreadOrDevice, plugins: MSZ[Plugin]): Unit = {
    assert(!m.isInstanceOf[AadlDevice] || arsitOptions.devicesAsThreads)

    val names = nameProvider(m.component, basePackage)
    val filename: String = Util.pathAppend(dirs.componentDir, ISZ(names.packagePath, s"${names.componentSingletonType}.scala"))

    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, z"-1000")

    resources = resources ++ TestTemplate.bridgeTestApis(basePackage, names, dirs, ports)

    val bridgeTestSuite: ST = TestTemplate.bridgeTestSuite(names.packageName, names)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, s"${names.testName}.scala"), bridgeTestSuite, F)

    if (seenComponents.contains(filename)) {
      return
    }

    val annexClauseInfos: ISZ[AnnexClauseInfo] = symbolTable.annexClauseInfos.get(m) match {
      case Some(infos) => infos
      case _ => ISZ()
    }

    val epp = ArsitPlugin.getEntryPointProvider(plugins, m, annexClauseInfos)

    val bridgeCode = ArsitPlugin.getBridgeCodeProviders(plugins).generate(
      nameProvider = names,
      component = m,
      ports = ports,
      entryPointProvider = epp,
      symbolTable = symbolTable,
      aadlTypes = types,
      reporter = reporter)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.bridge}.scala"), bridgeCode.bridge, T)
    resources = resources ++ bridgeCode.resources


    val integrationContracts = GumboGen.processIntegrationContract(m, symbolTable, types, basePackage)

    val api = ApiTemplate.api(
      names.packageName,
      basePackage,
      names,
      ports,
      integrationContracts)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.api}.scala"), api, T)


    if (ArsitPlugin.canHandleBehaviorProviders(plugins, m, annexClauseInfos)) {
      // TODO: probably should only allow one provider (i.e. assume the first one wins)
      for (bp <- ArsitPlugin.getBehaviorProviders(plugins)) {
        resources = resources ++ bp.handleBehaviorProvider(
          component = m,
          resolvedAnnexSubclauses = annexClauseInfos,
          suggestedFilename = filename,
          componentDirectory = ISZ(dirs.componentDir, names.packagePath),
          symbolTable = symbolTable,
          aadlTypes = types,
          reporter = reporter)
      }
    } else {
      var blocks: ISZ[ST] = ISZ()

      var behaviorCodeContributions: ISZ[BehaviorEntryPointObjectContributions] = ISZ()
      for (entryPoint <- ISZ(EntryPoints.initialise, EntryPoints.compute, EntryPoints.activate, EntryPoints.deactivate, EntryPoints.finalise, EntryPoints.recover)) {
        entryPoint match {
          case EntryPoints.compute if m.isSporadic() =>
            val inEventPorts = m.features.filter(
              f => f.isInstanceOf[AadlFeatureEvent] &&
                f.asInstanceOf[AadlFeatureEvent].direction == Direction.In).map((m: AadlFeature) => m.asInstanceOf[AadlPort])
            var isFirst = T
            for (inEventPort <- inEventPorts) {
              val methodSig: String = BehaviorEntryPointElementProvider.genMethodSignature(entryPoint, names, Some(inEventPort))
              val defaultMethodBody: ST = BehaviorEntryPointElementProvider.genComputeMethodBody(Some(inEventPort), m, isFirst, arsitOptions.excludeImpl)

              behaviorCodeContributions = behaviorCodeContributions :+ BehaviorEntryPointProviders.offer(
                entryPoint, Some(inEventPort), m, names, arsitOptions.excludeImpl, methodSig, defaultMethodBody, annexClauseInfos,
                basePackage, symbolTable, types, dirs, arsitOptions, plugins, reporter)

              isFirst = F
            }
          case _ =>
            val methodSig: String = BehaviorEntryPointElementProvider.genMethodSignature(entryPoint, names, None())
            val defaultMethodBody: ST = entryPoint match {
              case EntryPoints.compute => BehaviorEntryPointElementProvider.genComputeMethodBody(None(), m, T, arsitOptions.excludeImpl)
              case _ => BehaviorEntryPointElementProvider.genMethodBody(entryPoint, m, arsitOptions.excludeImpl)
            }

            behaviorCodeContributions = behaviorCodeContributions :+ BehaviorEntryPointProviders.offer(entryPoint, None(),
              m, names,
              arsitOptions.excludeImpl, methodSig, defaultMethodBody, annexClauseInfos,
              basePackage, symbolTable, types, dirs, arsitOptions, plugins, reporter)
        }
      }

      genSubprograms(m) match {
        case Some(x) =>
          blocks = blocks :+ x
          halt("Need to revisit subprograms")
        case _ =>
      }

      behaviorCodeContributions = behaviorCodeContributions :+
        BehaviorEntryPointProviders.finalise(annexClauseInfos, m, names, basePackage, symbolTable, types, dirs, arsitOptions, plugins, reporter)

      val markers = BehaviorEntryPointProviders.getMarkers(behaviorCodeContributions)
      val componentImpl: ST = BehaviorEntryPointElementProvider.genComponentImpl(names, behaviorCodeContributions)
      addResourceWithMarkers(filename, ISZ(), componentImpl, markers, F)

      // add external resources
      resources = resources ++ behaviorCodeContributions.flatMap((f: BehaviorEntryPointObjectContributions) => f.resources)
    }

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
          s"${CommonUtil.getLastName(param.identifier)} : ${pType.nameProvider.qualifiedReferencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isOutFeature(f))
      assert(rets.size <= 1, s"Expecting a single out param but found ${rets.size}")


      val (returnType, exampleType): (Option[String], Option[ST]) =
        if (rets.isEmpty) {
          (None(), None())
        }
        else {
          val rType: AadlType = Util.getFeatureEndType(rets(0), types)
          val _exampleValue: String = rType.nameProvider.example()
          val returnType = rType.nameProvider.qualifiedReferencedTypeName

          (Some(returnType), Some(st"${_exampleValue}"))
        }

      StubTemplate.subprogram(methodName, params, returnType, exampleType)
    })

    if (subprograms.nonEmpty) {
      val names = nameProvider(m, basePackage)
      val objectName = s"${names.componentSingletonType}_subprograms"

      val body = StubTemplate.slangBody(
        slangAnnotation = "@ext ",
        objectName = objectName,
        body = subprograms.map(m => m._1))

      if (!CommonUtil.isThread(m)) {
        val a = StubTemplate.slangPreamble(T, basePackage, names.packageName, ISZ(), ISZ(body))
        addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}.scala"), a, T)
      }

      val b = StubTemplate.slangPreamble(
        F,
        basePackage,
        names.packageName,
        ISZ(),
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
    addResourceWithMarkers(baseDir, paths, content, ISZ(), overwrite)
  }

  def addResourceWithMarkers(baseDir: String, paths: ISZ[String], content: ST, markers: ISZ[Marker], overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResourceWithMarkers(Util.pathAppend(baseDir, paths), content, markers, overwrite)
  }
}
