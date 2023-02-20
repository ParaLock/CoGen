package org.combinators.graphics.providers.javafx

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, NameProvider, Understands}


trait ApplicationObjectOrientedProvider extends ApplicationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")


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
    import ooParadigm.projectCapabilities.{addTypeLookupForClasses, addTypeLookupForConstructors}
    import paradigm.projectCapabilities.addTypeLookupForMethods // must be present, regardless of IntelliJ
    val dtpe = TypeRep.DataType(tpe)
    for {
      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
    } yield ()
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }


  def make_init_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {

      // call super.init()
      // print window initialized message

    } yield Some(n_mult)
  }

  def make_start_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      // Create parent group object
      // Create parent scene object
      // Add group object to scene
      // Loop over elements in domain model
      // Attach element to group object
      // Call setAlignment on each element according to style

      // Set primary stage title
      // Call set stage scene
      // Call stage show

    } yield Some(n_mult)
  }

  def make_stop_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      // Call super.stop()

    } yield Some(n_mult)
  }

  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      // Call launch

    } yield Some(n_mult)
  }

  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
         _ <- addMethod(names.mangle("init"), make_init_func())
         _ <- addMethod(names.mangle("start"), make_start_func())
         _ <- addMethod(names.mangle("stop"), make_stop_func())
         _ <- addMethod(names.mangle("main"), make_stop_func())
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(clazzName))
  }

  def implement(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- make_class("SampleApplication")
    } yield ()
  }

}

object ApplicationObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = ApplicationObjectOrientedProvider { val paradigm: P }
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
  : ApplicationObjectOrientedProvider.WithParadigm[base.type] =
    new ApplicationObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
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


