package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import java.nio.file.Paths
import java.util.UUID
import com.github.javaparser.ast.{ImportDeclaration, Modifier, NodeList, PackageDeclaration}
import com.github.javaparser.ast.`type`.VoidType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{MethodCallExpr, NameExpr, NullLiteralExpr}
import com.github.javaparser.ast.nodeTypes.{NodeWithScope, NodeWithSimpleName}
import com.github.javaparser.ast.stmt.{BlockStmt, ExpressionStmt}
import com.github.javaparser.ast.expr.{Name => JName}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, FileWithPath, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm => AP, _}
import org.combinators.ep.language.java.ContextSpecificResolver.ImportInfo
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.ep.language.java.{CodeGenerator, CompilationUnitCtxt, Config, ContextSpecificResolver, FreshNameCleanup, ImportCleanup, JavaNameProvider, MethodBodyCtxt, ProjectCtxt, Syntax, TestCtxt}
import org.combinators.templating.persistable.{BundledResource, JavaPersistable}
import org.combinators.ep.language.java.ResourcePersistable

import scala.util.Try
import scala.jdk.CollectionConverters._

trait AnyParadigm extends AP {
  val config: Config
  val syntax: Syntax.default.type = Syntax.default
  import syntax._

  type ProjectContext = ProjectCtxt
  type CompilationUnitContext = CompilationUnitCtxt
  type TestContext = TestCtxt
  type MethodBodyContext = MethodBodyCtxt


  val projectCapabilities: ProjectCapabilities =
    new ProjectCapabilities {

      val canRegisterImportForNameInProject: Understands[ProjectContext, RegisterImportForName] =
        new Understands[ProjectContext, RegisterImportForName] {
          def perform(
                       context: ProjectContext,
                       command: RegisterImportForName
                     ): (ProjectContext, Unit) = {

              ContextSpecificResolver.importsByTypeName += (
                command.name -> ImportInfo(command.importList, command.importAll)
              )

            (context, ())
          }
        }


      implicit val canDebugInProject: Understands[ProjectCtxt, Debug] =
        new Understands[ProjectCtxt, Debug] {
          def perform(
            context: ProjectCtxt,
            command: Debug
          ): (ProjectCtxt, Unit) = {

            context.units.foreach (u => System.err.println (command.tag + ": " + u))
            (context,())
          }
        }

      implicit val canAddCompilationUnitInProject: Understands[ProjectCtxt, AddCompilationUnit[Name, CompilationUnitCtxt]] =
        new Understands[ProjectCtxt, AddCompilationUnit[Name, CompilationUnitCtxt]] {
          def perform(
            context: ProjectCtxt,
            command: AddCompilationUnit[Name, CompilationUnitCtxt]
          ): (ProjectCtxt, Unit) = {
            val unit = new com.github.javaparser.ast.CompilationUnit()
            val tgtPackage = config.targetPackage.clone
            // [a, b, C]  and "trivially" --> trivially.a.b.C
            // .init drops last element ONLY if not empty
            if (command.name.nonEmpty) {
              tgtPackage.setName(
                command.name.init.foldLeft(tgtPackage.getName){case (qualName,suffix) => new com.github.javaparser.ast.expr.Name(qualName, suffix.mangled)}
              )
            }

            unit.setPackageDeclaration(tgtPackage)

            val (uc, _) =
              Command.runGenerator(
                command.unit,
                CompilationUnitCtxt(
                  context.resolver,
                  unit,
                  isTest = false)
              )

            if(uc.unit.getPackageDeclaration != tgtPackage) {
              uc.unit.setPackageDeclaration(tgtPackage)
            }

            val (newUnits, newTestUnits) =
              if (uc.isTest) {
                (context.units, context.testUnits :+ uc.unit)
              } else {
                (context.units :+ uc.unit, context.testUnits)
              }
            (context.copy(resolver = uc.resolver, units = newUnits, testUnits = newTestUnits), ())
          }
        }
      implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]] =
        new Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]] {
          def perform(
            context: ProjectContext,
            command: AddTypeLookup[MethodBodyCtxt, Type]
          ): (ProjectContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[MethodBodyCtxt, Type] = {
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._methodTypeResolution(k)(tpe)
              }
            }
            (context.copy(resolver = context.resolver.copy(_methodTypeResolution = newLookup)), ())
          }
        }
      override implicit val canNoop: Understands[ProjectCtxt, Noop] =
        new Understands[ProjectCtxt, Noop] {
          /** Returns the updated context and the result of the command. */
          override def perform(context: ProjectCtxt, command: Noop): (ProjectCtxt, Unit) = {
            (context, ())
          }
        }
    }

  val compilationUnitCapabilities: CompilationUnitCapabilities =
    new CompilationUnitCapabilities {

      override implicit val canNoop: Understands[CompilationUnitCtxt, Noop] =
        new Understands[CompilationUnitCtxt, Noop] {
          /** Returns the updated context and the result of the command. */
          override def perform(context: CompilationUnitCtxt, command: Noop): (CompilationUnitCtxt, Unit) = {
            (context, ())
          }
        }

      implicit val canDebugInCompilationUnit: Understands[CompilationUnitCtxt, Debug] =
        new Understands[CompilationUnitCtxt, Debug] {
          def perform(
            context: CompilationUnitCtxt,
            command: Debug
          ): (CompilationUnitCtxt, Unit) = {

            System.err.println (command.tag + ": " + context.unit)
            (context,())
          }
        }

      implicit val canAddImportInCompilationUnit: Understands[CompilationUnitCtxt, AddImport[ImportDeclaration]] =
        new Understands[CompilationUnitCtxt, AddImport[ImportDeclaration]] {
          def perform(
            context: CompilationUnitCtxt,
            command: AddImport[ImportDeclaration]
          ): (CompilationUnitContext, Unit) = {
            val oldUnit = context.unit
            val newUnit =
              if (!oldUnit.getImports.contains(command.imp)) {
                val nextUnit = oldUnit.clone()
                nextUnit.addImport(command.imp.clone())
                nextUnit
              } else oldUnit
            newUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
            (context.copy(unit = newUnit), ())
          }
        }
      implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] =
        new Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] {
          def perform(
            context: CompilationUnitCtxt,
            command: AddTestSuite[Name, TestCtxt]
          ): (CompilationUnitContext, Unit) = {
            val clsToAdd = new ClassOrInterfaceDeclaration()
            clsToAdd.setPublic(true)
            val (testRes, _) =
              Command.runGenerator(
                command.suite,
                TestCtxt(
                  context.resolver,
                  Seq.empty,
                  clsToAdd))
            val updatedUnit = {
              val oldUnit = context.unit
              val testClass = testRes.testClass
              val nextUnit = oldUnit.clone()
              testRes.extraImports.foreach { imp =>
                if (!nextUnit.getImports.contains(imp))
                  nextUnit.addImport(imp.clone())
              }
              val classToAdd = testClass.clone()
              classToAdd.setName(command.name.mangled)
              nextUnit.addType(classToAdd)
              nextUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
              nextUnit
            }
            (context.copy(resolver = testRes.resolver, unit = updatedUnit, isTest = true), ())
          }
        }

      implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnitContext, FreshName[Name]] =
        new Understands[CompilationUnitContext, FreshName[Name]] {
          def perform(context: CompilationUnitContext, command: FreshName[Name]): (CompilationUnitContext, Name) = {
            val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
            val updatedResolver = context.resolver.copy(
              generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
            )
            (context.copy(resolver = updatedResolver), freshName)
          }
        }
    }

  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {

      implicit val canDebugInMethodBody: Understands[MethodBodyCtxt, Debug] =
        new Understands[MethodBodyCtxt, Debug] {
          def perform(
            context: MethodBodyCtxt,
            command: Debug
          ): (MethodBodyCtxt, Unit) = {

            System.err.println (command.tag + ": " + context.method)
            (context,())
          }
        }

      implicit val canOutputToConsole: Understands[MethodBodyCtxt, OutputToConsole[Expression]] =
        new Understands[MethodBodyCtxt, OutputToConsole[Expression]] {
          def perform(
                       context: MethodBodyCtxt,
                       command: OutputToConsole[Expression]
                     ): (MethodBodyCtxt, Unit) = {
            (context.copy(), ())
          }
        }

      implicit val canAddImportInMethodBody: Understands[MethodBodyCtxt, AddImport[ImportDeclaration]] =
        new Understands[MethodBodyCtxt, AddImport[ImportDeclaration]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddImport[ImportDeclaration]
          ): (MethodBodyCtxt, Unit) = {
            val extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())
            (context.copy(extraImports = extraImports), ())
          }
        }

      implicit val canMakeRawImportInMethodBody: Understands[MethodBodyCtxt, MakeRawImport[ImportDeclaration, Name]] =
        new Understands[MethodBodyCtxt, MakeRawImport[ImportDeclaration, Name]] {
          def perform(
                       context: MethodBodyCtxt,
                       command: MakeRawImport[ImportDeclaration, Name]
                     ): (MethodBodyCtxt, ImportDeclaration) = {

            val name = command.name.tail.foldLeft(new com.github.javaparser.ast.expr.Name(command.name.head.mangled)){
              case (qualName,suffix) => new com.github.javaparser.ast.expr.Name(qualName, suffix.mangled)
            }
            val importExpr = new com.github.javaparser.ast.ImportDeclaration(
              name,
              false,
              false
            )
            (context, importExpr)
          }
        }

      implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] =
        new Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddBlockDefinitions[Statement]
          ): (MethodBodyCtxt, Unit) = {
            val updatedMethod = {
              val oldMethod = context.method
              val stmts = command.definitions
              val nextMethod = oldMethod.clone()
              val body = nextMethod.getBody.orElseGet(() => new BlockStmt())
              // if command.createNewBlock:
              //   new block statement
              //   add originals into new block statement
              // body.addStatement(new statement)
              stmts.foreach(stmt => body.addStatement(stmt.clone()))
              nextMethod.setBody(body)
              nextMethod
            }
            (context.copy(method = updatedMethod), ())
          }
        }

      implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyCtxt, SetReturnType[Type]] =
        new Understands[MethodBodyCtxt, SetReturnType[Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: SetReturnType[Type]
          ): (MethodBodyCtxt, Unit) = {
            val updatedMethod =  {
              val oldMethod = context.method
              val tpe = command.tpe
              val newMethod = oldMethod.clone()
              newMethod.setType(tpe.clone())
              newMethod
            }
            (context.copy(method = updatedMethod), ())
          }
        }

      implicit val canSetParametersInMethodBody: Understands[MethodBodyCtxt, SetParameters[Name, Type]] =
        new Understands[MethodBodyCtxt, SetParameters[Name, Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: SetParameters[Name, Type]
          ): (MethodBodyCtxt, Unit) = {
            val updatedMethod = {
              val oldMethod = context.method
              val params = command.params
              val newMethod = oldMethod.clone()
              newMethod.getParameters.clear()
              params.foreach { case (paramName, paramTpe) =>
                newMethod.addParameter(paramTpe.clone(), paramName.mangled)
              }
              newMethod
            }
            (context.copy(method = updatedMethod), ()) // second thing to be returned isn't optional, so make it () is like Unit
          }
        }
      implicit val canTransformTypeInMethodBody: Understands[MethodBodyCtxt, ToTargetLanguageType[Type]] =
        new Understands[MethodBodyContext, ToTargetLanguageType[Type]] {
          def perform(
            context: MethodBodyContext,
            command: ToTargetLanguageType[Type]
          ): (MethodBodyContext, Type) = {
            Command.runGenerator(context.resolver.methodTypeResolution(command.tpe), context)
          }
        }

      implicit def canReifyInMethodBody[T]: Understands[MethodBodyCtxt, Reify[T, Expression]] =
        new Understands[MethodBodyCtxt, Reify[T, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: Reify[T, Expression]
          ): (MethodBodyCtxt, Expression) = {
            Command.runGenerator(context.resolver.reificationInMethod(InstanceRep(command.tpe)(command.value)), context)
          }
        }

      implicit val canResolveImportInMethod: Understands[MethodBodyCtxt, ResolveImport[ImportDeclaration, Type]] =
        new Understands[MethodBodyCtxt, ResolveImport[ImportDeclaration, Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: ResolveImport[ImportDeclaration, Type]
          ): (MethodBodyCtxt, Option[ImportDeclaration]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              if (stripped.isClassOrInterfaceType) {
                val newImport = AnyParadigm.resolveRegisteredImport(command.forElem)
                if (context.extraImports.contains(newImport)) {
                  (context, None)
                } else {
                  (context, Some(newImport))
                }
              } else {
                (context, None)
              }
            }
          }
        }

      implicit val canApplyInMethodBody: Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] =
        new Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: Apply[Expression, Expression, Expression]
          ): (MethodBodyCtxt, Expression) = {
            val resultExp: Expression =
              if (command.functional.isMethodCallExpr) {
                val res = command.functional.asMethodCallExpr().clone()
                command.arguments.foreach(arg => res.addArgument(arg))
                res
              } else {
                val scope =
                  command.functional match {
                    case n: NodeWithScope[Expression] => n.getScope
                    case _ => null
                  }
                new MethodCallExpr(scope, command.functional.asInstanceOf[NodeWithSimpleName[_]].getNameAsString, new NodeList[Expression](command.arguments: _*))
              }
            (context, resultExp)
          }
        }

      implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] =
        new Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: GetArguments[Type, Name, Expression]
          ): (MethodBodyCtxt, Seq[(Name, Type, Expression)]) = {
            val params = context.method.getParameters.asScala.map { param =>
              (MangledName.fromAST(param.getName), param.getType, new NameExpr(param.getName))
            }
            (context, params)
          }
        }
      implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[Name]] =
        new Understands[MethodBodyContext, FreshName[Name]] {
          def perform(context: MethodBodyContext, command: FreshName[MangledName]): (MethodBodyContext, MangledName) = {
            val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
            val updatedResolver = context.resolver.copy(
              generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
            )
            (context.copy(resolver = updatedResolver), freshName)
          }
        }
      override implicit val canNoop: Understands[MethodBodyCtxt, Noop] =
        new Understands[MethodBodyContext, Noop] {
          /** Returns the updated context and the result of the command. */
          override def perform(context: MethodBodyCtxt, command: Noop): (MethodBodyCtxt, Unit) = {
            (context, ())
          }
        }
    }

  val testCapabilities: TestCapabilities =
    new TestCapabilities {
      implicit val canDebugInTest: Understands[TestContext, Debug] =
        new Understands[TestContext, Debug] {
          def perform(
            context: TestContext,
            command: Debug
          ): (TestContext, Unit) = {

            System.err.println (command.tag + ": " + context.testClass)
            (context,())
          }
        }

      implicit val canResolveImportInTest: Understands[TestContext, ResolveImport[ImportDeclaration, Type]] =
        new Understands[TestContext, ResolveImport[ImportDeclaration, Type]] {
          def perform(
                       context: TestContext,
                       command: ResolveImport[ImportDeclaration, Type]
                     ): (TestContext, Option[ImportDeclaration]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              if (stripped.isClassOrInterfaceType) {
                val importName = ObjectOriented.typeToName(stripped.asClassOrInterfaceType())
                val newImport =
                  new ImportDeclaration(
                    importName,
                    false,
                    false)
                if (context.extraImports.contains(newImport)) {
                  (context, None)
                } else {
                  (context, Some(newImport))
                }
              } else {
                (context, None)
              }
            }
          }
        }

      implicit val canAddImplementInTest: Understands[TestContext, AddImplementedTestCase[Type]] =
        new Understands[TestContext, AddImplementedTestCase[Type]] {
          def perform(
                       context: TestContext,
                       command: AddImplementedTestCase[Type]
                     ): (TestContext, Unit) = {
            val newClass = context.testClass.clone()

            newClass.addImplementedType(command.interface.toString)
            (context.copy(testClass = newClass), ())
          }
        }

      implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] =
        new Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] {
          def perform(
            context: TestContext,
            command: AddTestCase[MethodBodyContext, Name, Expression]
          ): (TestContext, Unit) = {
            val gen: Generator[MethodBodyCtxt, Unit] = {
              import methodBodyCapabilities._
              for {
                assertions <- command.code
                _ <- addBlockDefinitions(assertions.map(exp => new ExpressionStmt(exp.clone())))
              } yield ()
            }
            val testMethod = new MethodDeclaration()
            testMethod.setModifiers(Modifier.publicModifier().getKeyword)
            testMethod.setType(new com.github.javaparser.ast.`type`.VoidType())
            testMethod.setName(JavaNameProvider.addPrefix("test", command.name).toAST)
            testMethod.addMarkerAnnotation("org.junit.Test")
            val testImport = new ImportDeclaration("org.junit.Test", false, false)
            val (resultingContext, _) =
              Command.runGenerator(
                gen,
                MethodBodyCtxt(
                  context.resolver,
                  (testImport +: context.extraImports).distinct.map(_.clone),
                  testMethod)
              )
            val newClass = context.testClass.clone()
            newClass.addMember(resultingContext.method.clone)
            (context.copy(resolver = resultingContext.resolver, testClass = newClass, extraImports = resultingContext.extraImports), ())
          }
        }
    }

  private val defaultResolver: ContextSpecificResolver = {
    val emptyResolver =
      ContextSpecificResolver(
        _methodTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _constructorTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _classTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _reificationInClass = _ => rep => throw new NotImplementedError(rep.toString),
        _reificationInConstructor = _ => rep => throw new NotImplementedError(rep.toString),
        _reificationInMethod = _ => rep => throw new NotImplementedError(rep.toString),
        _importResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _instantiationOverride = _ => (tpe, args) => (tpe, args),
        generatedVariables = Map.empty
      )
    ContextSpecificResolver.updateResolver(config, TypeRep.Unit, new VoidType())(rep => new NullLiteralExpr())(emptyResolver)
  }


  override def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath] = {

    val (finalContext, _) =
      Command.runGenerator(generator,
        ProjectCtxt(
          resolver = defaultResolver,
          units = Seq.empty,
          testUnits = Seq.empty,
          extraDependencies = Seq.empty
        )
      )
    val nameEntry = config.projectName.map(n => s"""name := "$n"""").getOrElse("")
    val junitDeps = Seq(
      """"com.novocode" % "junit-interface" % "0.11" % "test"""",
      """"junit" % "junit" % "4.12" % "test""""
    )
    val deps = (junitDeps ++ finalContext.extraDependencies).mkString("Seq(\n    ", ",\n    ", "\n  )")

    // hard-code output to 1.8 for jacoco compatibility. TODO: Try to remove this dependency
    val buildFile =
      s"""
         |javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
         |$nameEntry
         |crossPaths := false
         |autoScalaLibrary := false
         |libraryDependencies ++= $deps
           """.stripMargin
    val pluginFile =
      s"""
         |addSbtPlugin("com.github.sbt" % "sbt-jacoco" % "3.0.3")
         """.stripMargin

    val cleanedUnits =
      ImportCleanup.cleaned(
        FreshNameCleanup.cleaned(finalContext.resolver.generatedVariables, finalContext.units: _*) : _*
     )
    val cleanedTestUnits =
      ImportCleanup.cleaned(
        FreshNameCleanup.cleaned(finalContext.resolver.generatedVariables, finalContext.testUnits: _*): _*
      )
    val javaFiles = cleanedUnits.map { unit =>
      FileWithPath(
        JavaPersistable.compilationUnitInstance.rawText(unit),
        JavaPersistable.compilationUnitInstance.fullPath(Paths.get("."), unit)
      )
    }
    val javaTestFiles = cleanedTestUnits.map { unit =>
      val javaPath =
        Paths.get("src", "main")
          .relativize(JavaPersistable.compilationUnitInstance.fullPath(Paths.get(""), unit))
      val testPath =
        Paths.get("src", "test").resolve(javaPath)
      FileWithPath(
        JavaPersistable.compilationUnitInstance.rawText(unit),
        testPath
      )
    }
    val gitIgnore = BundledResource("gitignore", Paths.get(".gitignore"), classOf[CodeGenerator])
      //FileWithPath(
      //ResourcePersistable.bundledResourceInstance.rawText(gitIgnore),
      //ResourcePersistable.bundledResourceInstance.path(gitIgnore)) +:
      FileWithPath(pluginFile, Paths.get("project", "plugin.sbt")) +:
      FileWithPath(buildFile, Paths.get("build.sbt")) +:
      (javaFiles ++ javaTestFiles)
  }
}

object AnyParadigm {

  def resolveRegisteredImport(elem: com.github.javaparser.ast.`type`.Type): ImportDeclaration = {

    val stripped = AnyParadigm.stripGenerics(elem)
    var importName = ObjectOriented.typeToName(stripped.asClassOrInterfaceType())
    val identifier = importName.getIdentifier
    var importAll = false

    if (ContextSpecificResolver.importsByTypeName.contains(identifier)) {

      val importInfo = ContextSpecificResolver.importsByTypeName.get(identifier).get
      val qualifier = new JName(importInfo.importList.mkString("."))
      importName = new JName(qualifier, identifier)
      importAll = importInfo.importAll
    }

    new ImportDeclaration(
      importName,
      false,
      importAll)
  }

  def stripGenerics(tpe: com.github.javaparser.ast.`type`.Type): com.github.javaparser.ast.`type`.Type = {
    if (tpe.isClassOrInterfaceType) {
      val clsTpe = tpe.asClassOrInterfaceType()
      clsTpe.clone().removeTypeArguments()
    } else {
      tpe
    }
  }

  def apply(config: Config): AnyParadigm = {
    val c = config
    new AnyParadigm {
      lazy val config: Config = c
    }
  }
}
