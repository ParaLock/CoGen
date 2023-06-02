package org.combinators.gui.providers.ros

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Exceptions}
import org.combinators.robotics.examples.RoboticsDomain


trait ROSApplicationProvider extends BaseProvider {

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")

  val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext,paradigm.type]

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_main_func(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import exceptions.exceptionCapabilities._
    for {

      // Make signature
      _ <- setStatic()
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
      _ <- resolveAndAddImport(arrayType)
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)
      _ <- setParameters(Seq((names.mangle("args"), arrayType)))
      // --------------

      self <- selfReference()

      testCls <- findClass(names.mangle("test1"))
      testClsExpr <- toStaticTypeExpression(testCls)
      methodRef <- getMethodReference(self, names.mangle("callback"))

      myMethod <- getMember(self, names.mangle("myMethod1"))

      _ <- make_method_call(myMethod, Seq(methodRef))


      exceptionCls <- findClass(names.mangle("Exception1"))
      exceptionCls2 <- findClass(names.mangle("Exception2"))

      _ <- addExceptionHandler(
        for {
          _ <- print_message("in the try block")
        } yield None,
        Seq(
          (exceptionCls, names.mangle("e"),

            for {
              _ <- print_message("in the first catch")
            } yield None

          ),
          (exceptionCls2, names.mangle("e"),
            for {
              _ <- print_message("in the second catch")
            } yield None
          )
        ),
        Some(
          for {
            _ <- print_message("in the finally block")
          } yield None
        )
      )

    } yield None
  }

  def make_try_contents(): Generator[MethodBodyContext, Unit] = {

    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    for {

      _ <- print_message("in the try block")

    } yield None
  }

  def make_catch_contents(): Generator[MethodBodyContext, Unit] = {

    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    for {

      _ <- print_message("in the catch block")

    } yield None
  }


  def make_class(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {
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
      _ <- make_class("ROSApplication")
    } yield ()
  }

}

object ROSApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = ROSApplicationProvider { val paradigm: P }
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
  (
    impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type],
    _exceptions: Exceptions.WithBase[base.MethodBodyContext,base.type]
  )
  (_domain: RoboticsDomain)
  : ROSApplicationProvider.WithParadigm[base.type] =
    new ROSApplicationProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      //val impConstructorParadigm: impConstructor.type = impConstructor
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
      override val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext,paradigm.type] = _exceptions
    }
}


