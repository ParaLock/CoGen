package org.combinators.gui.providers.qt

import org.combinators.common.BaseProvider
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}


trait QTApplicationProvider extends BaseProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val impConstructorParadigm: Imperative.WithBase[ooParadigm.ConstructorContext, paradigm.type]
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

      } yield None
    }

  def make_constructor(): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities.reify
    import impConstructorParadigm.imperativeCapabilities._

    for {

      title <- ooParadigm.constructorCapabilities.reify(
        TypeRep.String,
        "Hello, World"
      )

//      myVar <- declareVar(
//
//      )

      //      _ <- make_method_call(
//        labelObj,
//        "setText",
//        Seq(title)
//      )

    } yield()


  }

  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      val app_import = Seq(
        names.mangle("io"),
        names.mangle("qt"),
        names.mangle("core"),
        names.mangle("QMainWindow"),
      )
      for {
        pt <- findClass(app_import : _ *)
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- addConstructor(make_constructor())
        _ <- addMethod(names.mangle("closeEvent"), make_close_event())
        _ <- addMethod(names.mangle("main"), make_main_func())
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
    }
}


