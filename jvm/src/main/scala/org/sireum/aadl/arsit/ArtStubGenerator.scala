package org.sireum.aadl.arsit

import java.io.File
import org.sireum._
import org.sireum.aadl.ir._
import scala.language.implicitConversions

class ArtStubGenerator {

  var outDir : File = _
  var toImpl : ISZ[(ST, ST)] = ISZ()
  var basePackage: String = _
  var arsitOptions : Cli.ArsitOption = _
  var seenComponents : HashSet[String] = HashSet.empty
  var typeMap: Map[String, AadlType] = Map.empty

  var optVizEntries: ISZ[ST] = ISZ()

  def generator(dir: File, m: Aadl, packageName: String, o: Cli.ArsitOption) : Unit = {
    assert(dir.exists)

    outDir = dir
    basePackage = Util.sanitizeName(packageName)
    arsitOptions = o

    processDataTypes(m.dataComponents)

    for(c <- m.components)
      gen(c)

    if(optVizEntries.nonEmpty) {
      val eo = BlessST.vizExtObject(basePackage, ISZ(), optVizEntries)
      val so = BlessST.vizSlangObject(basePackage)
      val bv = BlessST.vizBlessViz(basePackage)

      val utilDir = new File(outDir, s"component/${basePackage}/util")

      Util.writeFile(new File(utilDir, s"${BlessST.vizObjectName}.scala"), so, true)
      Util.writeFile(new File(utilDir, s"${BlessST.vizObjectName}_Ext.scala"), eo,true)
      Util.writeFile(new File(utilDir, s"${BlessST.vizBlessVizName.render}.scala"), bv,true)
    }
  }

  def gen(m: Component) : Unit = {
    m.category match {
      case ComponentCategory.Process | ComponentCategory.System =>
        genContainer(m)
      case ComponentCategory.ThreadGroup =>
        genThreadGroup(m)
      case _ =>
        for(_c <- m.subComponents)
          gen(_c)
    }
  }

  def genContainer(m: Component) : Unit = {
    assert(m.category == ComponentCategory.Process || m.category == ComponentCategory.System)

    if(!Util.isThread(m)) {
      genSubprograms(m)
    }

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.Process | ComponentCategory.System => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device => genThread(c)
        case ComponentCategory.Subprogram => // ignore
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor=>
          println(s"Skipping: ${c.category} component: ${Util.getName(c.identifier)}")
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

  def genThread(m: Component) : Unit = {
    assert(m.category == ComponentCategory.Device || m.category == ComponentCategory.Thread)

    val names = Util.getNamesFromClassifier(m.classifier.get, basePackage)
    val filename = s"component/${names.packagePath}/${names.componentImpl}.scala"

    if(seenComponents.contains(filename)) {
      return
    }

    val componentName = "component"
    var ports: ISZ[Port] = ISZ()

    for(f <- Util.getFeatureEnds(m.features) if Util.isPort(f)) {
      ports :+= Port(f, m, basePackage)
    }

    val dispatchProtocol: String = {
      Util.getDiscreetPropertyValue[ValueProp](m.properties, Util.Prop_DispatchProtocol) match {
        case Some(x) => x.value
        case _ =>
          if(m.category == ComponentCategory.Device) "Periodic"
          else ???
      }
    }

    val blessAnnexes : ISZ[Annex] = m.annexes.filter(a => a.name == org.sireum.String("BLESS"))

    val genBlessEntryPoints: B = blessAnnexes.nonEmpty && arsitOptions.baTranslate

    val b = Template.bridge(basePackage, names.packageName, names.bridge, dispatchProtocol, componentName,
                            names.component, names.componentImpl, ports, genBlessEntryPoints)
    Util.writeFile(new File(outDir, s"bridge/${names.packagePath}/${names.bridge}.scala"), b)


    val compOutDir = new File(outDir, s"component/${names.packagePath}")

    if(!genBlessEntryPoints) {

      val compTrait = Template.componentTrait(basePackage, names.packageName, dispatchProtocol, names.component, names.bridge, ports)
      Util.writeFile(new File(compOutDir, s"/${names.component}.scala"), compTrait)

      val block = Template.componentImplBlock(names.component, names.bridge, names.componentImpl)

      val compImpl = Template.slangPreamble(T, names.packageName, basePackage,
        genSubprograms(m) match {
          case Some(x) => ISZ(block, x)
          case _ => ISZ(block)
        })

      Util.writeFile(new File(outDir, filename), compImpl, false)

    } else {

      assert(blessAnnexes.length == 1)

      val br = BlessGen(basePackage, compOutDir.getAbsolutePath, m, names, AadlTypes(typeMap),
        arsitOptions.baAddViz, arsitOptions.baExposeState).
        process(blessAnnexes(0).clause.asInstanceOf[BTSBLESSAnnexClause])

      Util.writeFile(new File(outDir, filename), br.component, true)

      optVizEntries = optVizEntries ++ br.optVizEntries
    }

    seenComponents = seenComponents + filename
  }

  def genSubprograms(m: Component) : Option[ST] = {
    val subprograms: ISZ[(ST, ST)] = m.subComponents.filter(p => p.category == ComponentCategory.Subprogram).map(p => {
      // only expecting in or out parameters
      Util.getFeatureEnds(p.features).elements.forall(f => f.category == FeatureCategory.Parameter && f.direction != Direction.InOut)

      val methodName = Util.getLastName(p.identifier)
      val params: ISZ[String] = Util.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && Util.isInFeature(f))
        .map(param => s"${Util.getLastName(param.identifier)} : ${Util.getDataTypeNames(param, basePackage).referencedTypeName}")
      val rets = Util.getFeatureEnds(p.features).filter(f => f.category == FeatureCategory.Parameter && Util.isOutFeature(f))
      assert(rets.size == 1)
      val returnType = Util.getDataTypeNames(rets(0), basePackage).referencedTypeName

      Template.subprogram(methodName, params, returnType)
    })

    if (subprograms.nonEmpty) {
      val names = Util.getNamesFromClassifier(m.classifier.get, basePackage)
      val objectName = s"${names.component}_subprograms"

      val body = Template.slangBody("@ext ", objectName, subprograms.map(_._1))

      if(!Util.isThread(m)) {
        val a = Template.slangPreamble(T, basePackage, names.packageName, ISZ(body))
        Util.writeFile(new File(outDir, s"component/${names.packagePath}/${objectName}.scala"), a)
      }

      val b = Template.slangPreamble(F, basePackage, names.packageName,
        ISZ(Template.slangBody("", s"${objectName}_Ext", subprograms.map(_._2))))
      Util.writeFile(new File(outDir, s"component/${names.packagePath}/${objectName}_Ext.scala"), b, F)

      return Some(body)
    } else {
      return None[ST]()
    }
  }


  def processDataTypes(values: ISZ[Component]): Unit = {
    for (v <- values) {
      typeMap = typeMap + (v.classifier.get.name ~> processType(v))
    }
  }

  def processType(c: Component): AadlType = {
    assert(c.category == ComponentCategory.Data)
    val cname = c.classifier.get.name
    val names = Util.getNamesFromClassifier(c.classifier.get, basePackage)

    if(Util.isEnumType(c)) {
      return  EnumType(cname, c, names.component, Util.getEnumValues(c))

    } else if(Util.isBaseType(c)) {

      return BaseType(cname, c, "F32")

    } else if(Util.isArrayType(c)) {
      halt("")

    } else if(Util.isRecordType(c)) {
      var fields: Map[String, AadlType] = Map.empty

      for(sc <- c.subComponents){
        val fieldName = Util.getLastName(sc.identifier)
        fields = fields + (fieldName ~> processType(sc))
      }

      return RecordType(cname, c, names.component, fields)

    } else {
      halt(s"Can't identify data component type: ${c}")
    }
  }

  // @formatter:off
  object Template {
    @pure def bridge(topLevelPackageName: String,
                     packageName : String,
                     bridgeName : String,
                     dispatchProtocol : String,
                     componentName : String,
                     componentType : String,
                     componentImplType : String,
                     ports : ISZ[Port],
                     genBlessEntryPoints: B) : ST = {
      return st"""// #Sireum
                  |
                  |package $packageName
                  |
                  |import org.sireum._
                  |import art._
                  |import ${topLevelPackageName}._
                  |
                  |${Util.doNotEditComment()}
                  |
                  |@record class $bridgeName(
                  |  val id: Art.BridgeId,
                  |  val name: String,
                  |  val dispatchProtocol: DispatchPropertyProtocol,
                  |
                  |  ${(ports.map(p => s"${p.name}: Port[${p.portType.qualifiedReferencedTypeName}]"), ",\n")}
                  |  ) extends Bridge {
                  |
                  |  val ports : Bridge.Ports = Bridge.Ports(
                  |    all = ISZ(${(ports.map(_.name), ",\n")}),
                  |
                  |    dataIns = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isInFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    dataOuts = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isOutFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventIns = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isInFeature(v.feature)).map(_.name), ",\n")}),
                  |
                  |    eventOuts = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isOutFeature(v.feature)).map(_.name), ",\n")})
                  |  )
                  |
                  |  val api : ${bridgeName}.Api =
                  |    ${bridgeName}.Api(
                  |      id,
                  |      ${(ports.map(p => s"${p.name}.id"), ",\n")}
                  |    )
                  |
                  |  val entryPoints : Bridge.EntryPoints =
                  |    ${bridgeName}.EntryPoints(
                  |      id,
                  |
                  |      ${(ports.map(p => s"${p.name}.id"), ",\n")},
                  |
                  |      ${componentImplType}(api)
                  |    )
                  |}
                  |
                  |object $bridgeName {
                  |
                  |  @record class Api(
                  |    id : Art.BridgeId,
                  |    ${(ports.map(p => s"${addId(p.name)} : Art.PortId"), ",\n")}) {
                  |
                  |    ${(ports.filter(p => Util.isEventPort(p.feature)).map(p => Template.eventPortApi(p).render), "\n\n")}
                  |
                  |    ${(ports.filter(p => Util.isDataPort(p.feature)).map(p => Template.dataPortApi(p).render), "\n\n")}
                  |
                  |    def logInfo(msg: String): Unit = {
                  |      Art.logInfo(id, msg)
                  |    }
                  |
                  |    def logDebug(msg: String): Unit = {
                  |      Art.logDebug(id, msg)
                  |    }
                  |
                  |    def logError(msg: String): Unit = {
                  |      Art.logError(id, msg)
                  |    }
                  |  }
                  |
                  |  @record class EntryPoints(
                  |    ${bridgeName}Id : Art.BridgeId,
                  |
                  |    ${(ports.map(p => s"${addId(p.name)} : Art.PortId"), ",\n")},
                  |
                  |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
                  |
                  |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isInFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventInPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isInFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isDataPort(v.feature) && Util.isOutFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${
                         (ports.filter(v => Util.isEventPort(v.feature) && Util.isOutFeature(v.feature)).map(p => addId(p.name)), ",\n")})
                  |
                  |    def initialise(): Unit = {
                  |      ${componentName}.${if(genBlessEntryPoints) "Initialize_Entrypoint()" else "initialise()"}
                  |    }
                  |
                  |    def compute(): Unit = {
                  |      ${computeBody(bridgeName + "Id", componentName, ports, dispatchProtocol, genBlessEntryPoints)}
                  |    }
                  |
                  |    def activate(): Unit = {
                  |      ${componentName}.${if(genBlessEntryPoints) "Activate_Entrypoint()" else "activate()"}
                  |    }
                  |
                  |    def deactivate(): Unit = {
                  |      ${componentName}.${if(genBlessEntryPoints) "Deactivate_Entrypoint()" else "deactivate()"}
                  |    }
                  |
                  |    def recover(): Unit = {
                  |      ${componentName}.${if(genBlessEntryPoints) "Recover_Entrypoint()" else "recover()"}
                  |    }
                  |
                  |    def finalise(): Unit = {
                  |      ${componentName}.${if(genBlessEntryPoints) "Finalize_Entrypoint()" else "finalise()"}
                  |    }
                  |  }
                  |}"""
    }

    @pure def computeBody(bridgeName: String,
                          componentName: String,
                          ports: ISZ[Port],
                          dispatchProtocol:String,
                          genBlessEntryPoints: B) : ST = {
      if(!genBlessEntryPoints) {
        dispatchProtocol.toString match {
          case "Sporadic" =>
            return st"""val EventTriggered(portIds) = Art.dispatchStatus(${bridgeName})
                       |Art.receiveInput(portIds, dataInPortIds)
                       |
                       |for(portId <- portIds) {
                       |  ${var s = ""
                            for (p <- ports if Util.isEventPort(p.feature) && Util.isInFeature(p.feature))
                              s += "\n" + Template.portCase(componentName, p, s == "").render
                            s
                          }
                       |}
                       |
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
          case "Periodic" =>
            return st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                       |${componentName}.timeTriggered()
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        }
      } else {
        dispatchProtocol.toString match {
          case "Sporadic" =>
            return st"""val EventTriggered(dispatchedPortIds) = Art.dispatchStatus(${bridgeName})
                       |Art.receiveInput(dispatchedPortIds, dataInPortIds)
                       |${componentName}.Compute_Entrypoint(dispatchedPortIds)
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
          case "Periodic" =>
            return st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                       |${componentName}.Compute_Entrypoint(ISZ())
                       |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        }
      }
    }

    @pure def demo() : ST =
      return st"""object Demo extends App {
                 |  art.Art.run(Arch.ad)
                 |}"""

    @pure def componentTrait(topLevelPackageName: String,
                             packageName : String,
                             dispatchProtocol : String,
                             componentType : String,
                             bridgeName : String,
                             ports : ISZ[Port]) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import ${topLevelPackageName}._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@msig trait ${componentType} {
                 |
                 |  def api : ${bridgeName}.Api
                 |
                 |  def initialise(): Unit = {}
                 |
                 |  def finalise(): Unit = {}
                 |  ${dispatchProtocol.toString match {
                        case "Sporadic" =>
                          var s = ""
                          for (p <- ports if Util.isEventPort(p.feature) && Util.isInFeature(p.feature))
                            s += "\n" + Template.portCaseMethod(p).render + "\n"
                          s
                        case "Periodic" => "\ndef timeTriggered() : Unit = {}\n"
                      }
                    }
                 |  def activate(): Unit = {}
                 |
                 |  def deactivate(): Unit = {}
                 |
                 |  def recover(): Unit = {}
                 |}"""
    }

    @pure def addId(s: String) : String = s + "_Id"

    @pure def putValue(p: Port) : ST =
      return st"""Art.putValue(${addId(p.name)}, ${p.portType.qualifiedPayloadName}${if(Util.isEmptyType(p.portType)) "()" else "(value)"})"""

    @pure def apiCall(componentName : String, portName: String): String =
      return s"${componentName}.${portName}Api(${portName}.id)"

    @pure def apiSig(portName:String, portType:String) : ST = {
      return st"""${portName} : ${portName}Api"""
    }

    @pure def getterApi(p: Port): ST = {
      val typeName = p.portType.qualifiedReferencedTypeName
      return st"""def get${p.name}() : Option[${typeName}] = {
                 |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
                 |    case Some(${typeName.toString.replace(".Type", "")}${if (Util.isEmptyType(p.portType)) "()) => Some(art.Empty())" else "_Payload(v)) => Some(v)"}
                 |    case _ => None[${typeName}]()
                 |  }
                 |  return value
                 |}"""
    }

    @pure def eventPortApi(p: Port) : ST = {
      if(Util.isInFeature(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def send${p.name}(${if (Util.isEmptyType(p.portType)) "" else s"value : ${p.portType.qualifiedReferencedTypeName}"}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
        }
    }

    @pure def dataPortApi(p: Port) : ST = {
      if(Util.isInFeature(p.feature)) {
        return getterApi(p)
      } else {
        return st"""def set${p.name}(value : ${p.portType.qualifiedReferencedTypeName}) : Unit = {
                   |  ${putValue(p)}
                   |}"""
      }
    }

    @pure def portCase(cname:String, v: Port, first : B) : ST = {
      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}){
                     |  val Some(${v.portType.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
                     |  ${cname}.handle${v.name}(value)
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""${if(!first) "else " else ""}if(portId == ${addId(v.name)}) {
                     |  ${cname}.handle${v.name}()
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def portCaseMethod(v: Port) : ST = {
      v.feature.category match {
        case FeatureCategory.EventDataPort =>
          return st"""def handle${v.name}(value : ${v.portType.qualifiedReferencedTypeName}): Unit = {
                     |  api.logInfo(s"received ${"${value}"}")
                     |  api.logInfo("default ${v.name} implementation")
                     |}"""
        case FeatureCategory.EventPort =>
          return st"""def handle${v.name}(): Unit = {
                     |  api.logInfo("received ${v.name}")
                     |  api.logInfo("default ${v.name} implementation")
                     |}"""
        case _ => throw new RuntimeException("Unexpected " + v.feature.category)
      }
    }

    @pure def componentImplBlock(componentType : String,
                                 bridgeName : String,
                                 componentImplType : String) : ST = {
      return st"""@record class $componentImplType (val api : ${bridgeName}.Api) extends ${componentType}"} {
                 |}"""
    }

    @pure def subprogram(methodName: String,
                         params: ISZ[String],
                         returnType : String): (ST, ST) = {
      return (st"""def ${methodName}(${(params, ",\n")}): ${returnType} = ${"$"}""",
              st"""def ${methodName}(${(params, ",\n")}): ${returnType} = {
                  |  ${if(returnType != org.sireum.String("")) s"return ${returnType}()" else ""}
                  |}""")
    }

    @pure def slangPreamble(inSlang: B,
                            packageName: String,
                            topLevelPackageName: String,
                            blocks: ISZ[ST]): ST = {
      return st"""${if(inSlang) { "// #Sireum\n\n"} else ""}package $packageName
                 |
                 |import org.sireum._
                 |import ${basePackage}._
                 |
                 |${(blocks, "\n\n")}
                 |"""
    }

    @pure def slangBody(slangAnnotation: String,
                        objectName: String,
                        body: ISZ[ST]) : ST = {
      return st"""${slangAnnotation}object ${objectName} {
                 |
                 |  ${(body, "\n\n")}
                 |}"""
    }
  }
  // @formatter:on
}


object ArtStubGenerator {
  def apply(dir: File, m: Aadl, packageName: String, o: Cli.ArsitOption) = new ArtStubGenerator().generator(dir, m, packageName, o)
}