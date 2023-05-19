package org.combinators.gui.providers.qt

import org.combinators.common.BaseProvider
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.graphics.GUIDomain


trait QTApplicationProvider extends BaseProvider {

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

  def make_close_event(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._
      for {

        unitType <- toTargetLanguageType(TypeRep.Unit)

        // Make signature
        _ <- paradigm.methodBodyCapabilities.setReturnType(unitType)
        qEventType <- findClass(names.mangle("QCloseEvent"))
        _ <- resolveAndAddImport(qEventType)
        _ <- setParameters(Seq((names.mangle("event"), qEventType)))
        //----

        args <- getArguments()
        (name, tpe, event) = args.head

        _ <- make_method_call(
          event,
          "accept",
          Seq.empty
        )

      } yield None
    }

  def register_imports(): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._

    val importsList = Seq[Seq[String]](
      Seq("io", "qt", "widgets", "QMainWindow")
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

  def make_init_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    val windowTitle: String = domain.window.title

    for {

      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)

      title <- paradigm.methodBodyCapabilities.reify(
        TypeRep.String,
        windowTitle
      )

      window <- make_class_instantiation(
        "QWidget",
        "window",
        Seq.empty
      )

      _ <- make_method_call(
        window,
        "setWindowTitle",
        Seq(title)
      )

      gridLayout <- make_class_instantiation(
        "QGridLayout",
        "gridLayout",
        Seq(window)
      )

      _ <- make_method_call(
        window,
        "show",
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

        args <- getArguments()
        (name, tpe, inputArgs) = args.head

        initMethod <- get_static_class_member(
          "QApplication",
          "initialize"
        )
        _ <- make_method_call(initMethod, Seq(inputArgs))

        _ <- make_class_instantiation_floating(
          "Application",
          Seq.empty,
          true
        )

        execMethod <- get_static_class_member(
          "QApplication",
          "exec"
        )
        _ <- make_method_call(execMethod, Seq.empty)

      } yield None
    }

  def make_constructor(): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    import impConstructorParadigm.imperativeCapabilities._
    import paradigm.methodBodyCapabilities._


    for {

      self <- selfReference()
      _ <- make_method_call_in_constructor(
        self,
        "init",
        Seq.empty
      )

    } yield()


  }

  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {

        pt <- findClass(names.mangle("QMainWindow"))
        _ <- resolveAndAddImport(pt)

        _ <- addParent(pt)
        _ <- addConstructor(make_constructor())
        _ <- addMethod(names.mangle("closeEvent"), make_close_event())
        _ <- addMethod(names.mangle("main"), make_main_func())
        _ <- addMethod(names.mangle("init"), make_init_func())
      } yield ()
    }

    for {

      _ <- register_imports()
      _ <- addClassToProject (makeClass, names.mangle(clazzName))

    } yield()

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
      _ <- make_class("QTBasicApplication")
    } yield ()
  }

}

object QTApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = QTApplicationProvider { val paradigm: P }
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
  (impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type])
  (_domain: GUIDomain)
  : QTApplicationProvider.WithParadigm[base.type] =
    new QTApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      //override val impConstructorParadigm: impConstructor.type = impConstructor
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


