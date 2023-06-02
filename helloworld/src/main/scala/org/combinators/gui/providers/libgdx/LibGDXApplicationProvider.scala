package org.combinators.gui.providers.libgdx

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, forEachWithIndex}
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.graphics.GUIDomain
import org.combinators.gui.domain_model.{Element, GridLayout, Text}

import scala.collection.Seq


trait LibGDXApplicationProvider extends BaseProvider {

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")

  val domain: GUIDomain

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_dispose_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    for {

      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)

      selfRef <- selfReference()
      batchObj <- getMember(selfRef, names.mangle("batch"))
      _ <- make_method_call(batchObj, "dispose", Seq.empty)

    } yield None
  }

  def make_create_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {

      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)

      _ <- make_field_class_assignment("SpriteBatch", "batch", Seq.empty)
      _ <- make_field_class_assignment("BitmapFont", "font", Seq.empty)
      _ <- make_field_class_assignment("OrthographicCamera", "camera", Seq.empty)


    } yield None
  }

  def make_render_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    val layout: GridLayout = domain.layout

    var counter = 0;

    for {
      unitType <- toTargetLanguageType(TypeRep.Unit)
      booleanType <- toTargetLanguageType(TypeRep.Boolean)
      intType <- toTargetLanguageType(TypeRep.Int)
      doubleType <- toTargetLanguageType(TypeRep.Double)

      falseVal <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, false)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      _ <- setReturnType(unitType)

      self <- selfReference()
      cameraObj <- getMember(self, names.mangle("camera"))
      batchObj <- getMember(self, names.mangle("batch"))
      fontObj <- getMember(self, names.mangle("font"))

      gdxClass <- findClass(names.mangle("Gdx"))
      gdxGraphicsClass <- get_static_class_member(
        gdxClass,
        "graphics"
      )
      gdxGlObj <- get_static_class_member(
        gdxClass,
        "gl"
      )

      getWidthFunc <- getMember(
        gdxGraphicsClass,
        names.mangle("getWidth")
      )
      getHeightFunc <- getMember(
        gdxGraphicsClass,
        names.mangle("getHeight")
      )
      getWidthCall <- apply(getWidthFunc, Seq.empty)
      getHeightCall <- apply(getHeightFunc, Seq.empty)

      _ <- make_method_call(
        cameraObj,
        "setToOrtho",
        Seq(
          falseVal,
          getWidthCall,
          getHeightCall
        )
      )

      screenUtilsCls <- findClass(names.mangle("ScreenUtils"))

      _ <- make_static_method_call(
        screenUtilsCls,
        "clear",
        Seq(zero, zero, zero, zero)
      )

      _ <- make_method_call(
        cameraObj,
       "update",
        Seq.empty
      )

      camCombined <- getMember(
        cameraObj,
        names.mangle("combined")
      )

      _ <- make_method_call(
         batchObj,
       "setProjectionMatrix",
         Seq(camCombined)
      )

      glClearColorFunc <- getMember(
        gdxGlObj,
        names.mangle("glClearColor")
      )
      glClearFunc <- getMember(
        gdxGlObj,
        names.mangle("glClear")
      )
      gl20Class <- findClass(
        names.mangle("GL20")
      )
      colorBufferBit <- get_static_class_member(
        gl20Class,
        "GL_COLOR_BUFFER_BIT"
      )

      _ <- make_method_call(
        gdxGlObj,
        "glClearColor",
        Seq(zero, zero, zero, one)
      )

      _ <- make_method_call(
        gdxGlObj,
        "glClear",
        Seq(colorBufferBit)
      )

      _ <- make_method_call(
        batchObj,
        "begin",
        Seq.empty
      )

      numRows <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, layout.cols)
      numCols <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, layout.rows)

      numRowsVar <- declareVar(names.mangle("numRows"), intType, Some(numRows))
      numColsVar <- declareVar(names.mangle("numCols"), intType, Some(numCols))
      numRowsPlusOne <- ffiArithmetic.arithmeticCapabilities.add(numRowsVar, one)
      numColsPlusOne <- ffiArithmetic.arithmeticCapabilities.add(numColsVar, one)

      yPaddingExpr <- ffiArithmetic.arithmeticCapabilities.div(
        getHeightCall,
        numRowsPlusOne
      )
      xPaddingExpr <- ffiArithmetic.arithmeticCapabilities.div(
        getWidthCall,
        numColsPlusOne
      )

      yPaddingVar <- declareVar(names.mangle("yPadding"), doubleType, Some(yPaddingExpr))
      xPaddingVar <- declareVar(names.mangle("xPadding"), doubleType, Some(xPaddingExpr))

      startYExpr <- ffiArithmetic.arithmeticCapabilities.sub(getHeightCall, yPaddingVar)
      startYVar <- declareVar(names.mangle("startY"), doubleType, Some(startYExpr))
      startXVar <- declareVar(names.mangle("startX"), doubleType, Some(xPaddingVar))

      currYVar <- declareVar(names.mangle("currY"), doubleType, Some(startYVar))
      currXVar <- declareVar(names.mangle("currX"), doubleType, Some(startXVar))

      _ <- forEachWithIndex(layout.elements) { (elem: Element, index: Int) => {

        counter = counter + 1

        for {

          msg <- paradigm.methodBodyCapabilities.reify(
            TypeRep.String,
            elem.asInstanceOf[Text].msg
          )

          c <- paradigm.methodBodyCapabilities.reify(
            TypeRep.Int,
            index % 3
          )

          drawCall <- make_method_call(
            fontObj,
            "draw",
            Seq(msg, currXVar, currYVar)
          )
          _ <- if ((index + 1) % layout.cols == 0) {
            for {

              currYUpdateExpr <- ffiArithmetic.arithmeticCapabilities.sub(currYVar, yPaddingVar)
              yUpdate <- assignVar(currYVar, currYUpdateExpr)
              xUpdate <- assignVar(currXVar, startXVar)
              _ <- addBlockDefinitions(Seq(yUpdate, xUpdate))

            } yield (None)
          } else {
            for {

              currXUpdateExpr <- ffiArithmetic.arithmeticCapabilities.add(currXVar, xPaddingVar)
              xUpdate <- assignVar(currXVar, currXUpdateExpr)
              _ <- addBlockDefinitions(Seq(xUpdate))

            } yield (None)
          }

        } yield (None)
      }}


      _ <- make_method_call(
        batchObj,
        "end",
        Seq.empty
      )

    } yield None
  }

  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    for {

      // Make signature
      _ <- setStatic()
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
      _ <- resolveAndAddImport(arrayType)
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)
      _ <- setParameters(Seq((names.mangle("args"), arrayType)))
      // --------------

      fps <- paradigm.methodBodyCapabilities.reify(
        TypeRep.Int,
        60
      )

      title <- paradigm.methodBodyCapabilities.reify(
        TypeRep.String,
        "GDX Window"
      )

      configObj <- make_class_instantiation(
        "Lwjgl3ApplicationConfiguration",
        "config",
        Seq.empty
      )

      _ <- make_method_call(
        configObj,
        "setForegroundFPS",
        Seq(fps)
      )

      _ <- make_method_call(
        configObj,
        "setTitle",
        Seq(title)
      )

      trueExpr <- paradigm.methodBodyCapabilities.reify(
        TypeRep.Boolean,
        true
      )

      _ <- make_method_call(
        configObj,
        "useVsync",
        Seq(trueExpr)
      )

      windowClassInstantiation <- make_class_instantiation_floating(
        "Window",
        Seq.empty,
        false
      )

      lwjglInstantation <- make_class_instantiation_floating(
        "Lwjgl3Application",
        Seq(windowClassInstantiation, configObj),
        true
      )

    } yield Some(lwjglInstantation)
  }

  def register_imports(): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._

    val importsList = Seq[Seq[String]](
      Seq("com", "badlogic", "gdx", "ApplicationAdapter"),
      Seq("com", "badlogic", "gdx", "Gdx"),
      Seq("com", "badlogic", "gdx", "graphics", "GL20"),
      Seq("com", "badlogic", "gdx", "graphics", "OrthographicCamera"),
      Seq("com", "badlogic", "gdx", "graphics", "g2d", "BitmapFont"),
      Seq("com", "badlogic", "gdx", "graphics", "g2d", "SpriteBatch"),
      Seq("com", "badlogic", "gdx", "utils", "ScreenUtils"),
    )

    for {

      _ <- forEach(importsList) { (elem: Seq[String]) =>
        for {
          _ <- registerImportForName(
            elem.last,
            elem
          )
        } yield ()
      }

    } yield ()
  }

  def make_application_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {

        spriteBatchClass <- findClass(names.mangle("SpriteBatch"))
        _ <- resolveAndAddImport(spriteBatchClass)
        bitmapFontClass <- findClass(names.mangle("BitmapFont"))
        _ <- resolveAndAddImport(bitmapFontClass)
        orthographicCameraClass <- findClass(names.mangle("OrthographicCamera"))
        _ <- resolveAndAddImport(orthographicCameraClass)
        pt <- findClass(names.mangle("ApplicationAdapter"))

        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- addField(names.mangle("batch"), spriteBatchClass)
        _ <- addField(names.mangle("font"), bitmapFontClass)
        _ <- addField(names.mangle("camera"), orthographicCameraClass)
        _ <- addMethod(names.mangle("create"), make_create_func())
        _ <- addMethod(names.mangle("dispose"), make_dispose_func())
        _ <- addMethod(names.mangle("render"), make_render_func())
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(clazzName))
  }

  def make_desktop_launcher_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {
        _ <- addMethod(mainFuncName, make_main_func())
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(clazzName))
  }

  def make_window_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ffiEquality.equalityCapabilities._
    for {
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 42)
    } yield Seq(two)
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_window_test(), testWindowName)
    } yield ()
  }

  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {

      _ <- register_imports()
      _ <- make_desktop_launcher_class("DesktopLauncher")
      _ <- make_application_class("Application")
    } yield ()
  }

}

object LibGDXApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = LibGDXApplicationProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   ffiarith:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   ffiassert: Assertions.WithBase[base.MethodBodyContext, base.type],
   ffiequal: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  (
    impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type],
  )
  (_domain: GUIDomain)
  : LibGDXApplicationProvider.WithParadigm[base.type] =
    new LibGDXApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val impConstructorParadigm: Imperative.WithBase[ooParadigm.ConstructorContext, paradigm.type] = impConstructor
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
      override val domain: GUIDomain = _domain
    }
}


