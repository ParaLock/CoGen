package org.combinators.gui.providers.basic_application.libgdx

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}


trait LibGDXApplicationProvider extends BaseProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")


  /**
   * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
   *
   * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
   * in the evolution of the system over time.
   *
   * @param dtpe
   * @param canFindClass
   * @tparam Ctxt
   * @return
   */
  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  /** Provides meaningful default solution to find the base data type in many object-oriented approaches.
   *
   * This enables target-language classes to be retrieved from within the code generator in the Method, Class or Constructor contexts.
   */
  def registerTypeMapping(tpe: DataType): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod // must be present, regardless of IntelliJ
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    import ooParadigm.classCapabilities.canFindClassInClass // must be present, regardless of IntelliJ
    import ooParadigm.constructorCapabilities.canFindClassInConstructor // must be present, regardless of IntelliJ
    val dtpe = TypeRep.DataType(tpe)
    for {
      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
    } yield ()
  }

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
    for {
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)

      selfRef <- selfReference()

//      cameraObj <- getMember(selfRef, names.mangle("camera"))
//      setToOrthoFunc <- getMember(cameraObj, names.mangle("setToOrthoFunc"))
//
//      gdxClass <- findClass(names.mangle("Gdx"))
//      graphics <- getMember(gdxClass, names.mangle("graphics"))
//
//
//      _ <- apply(setToOrthoFunc, Seq(
//        falseExpr,
//
//      ))

//      camera.setToOrtho(
//        false,
//        Gdx.graphics.getWidth(),
//        Gdx.graphics.getHeight()
//      );
//
//      ScreenUtils.clear(0, 0, 0, 0);
//      camera.update();
//      batch.setProjectionMatrix(camera.combined);
//
//      batch.begin();
//
//      font.draw(
//        batch,
//        "Hello World!",
//        Gdx.graphics.getWidth() / 2,
//        Gdx.graphics.getHeight() / 2
//      );
//
//      batch.end();

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

      classType <- findClass(names.mangle("Lwjgl3Application"))
      _ <- resolveAndAddImport(classType)
      classObj <- instantiateObject(classType, Seq.empty)

    } yield Some(classObj)
  }


  def make_application_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      val libgdx_import = Seq(
        names.mangle("com"),
        names.mangle("badlogic"),
        names.mangle("gdx"),
        names.mangle("ApplicationAdapter")
      )

      for {

        spriteBatchClass <- findClass(names.mangle("SpriteBatch"))
        bitmapFontClass <- findClass(names.mangle("BitmapFont"))
        orthographicCameraClass <- findClass(names.mangle("OrthographicCamera"))

        pt <- findClass(libgdx_import : _ *)

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
   //impConstructor: Imperative.WithBase[ObjectOriented.WithBase[base.type], base.type],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   ffiarith:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   ffiassert: Assertions.WithBase[base.MethodBodyContext, base.type],
   ffiequal: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : LibGDXApplicationProvider.WithParadigm[base.type] =
    new LibGDXApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      //val impConstructorParadigm: impConstructor.type = impConstructor
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
    }
}


