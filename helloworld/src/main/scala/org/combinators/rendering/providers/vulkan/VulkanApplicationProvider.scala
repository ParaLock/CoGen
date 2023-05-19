package org.combinators.rendering.providers.vulkan

import com.github.javaparser.ast.ImportDeclaration
import org.combinators.common._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.generator.Command.{Generator, lift}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented, Templating}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.graphics.RenderingDomain


trait VulkanApplicationProvider extends BaseProvider {

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val initFuncName = names.mangle("init")
  lazy val startFuncName = names.mangle("start")
  lazy val stopFuncName = names.mangle("stop")
  lazy val mainFuncName = names.mangle("main")
  lazy val testWindowName = names.mangle("windowTest")

  val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type]
  val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type]

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

  def make_frame_unit(): Generator[CompilationUnitContext, Unit] = {

    import unitTemplating.templatingCapabilities._

    for {
      _ <- loadFragment(this.getClass.getResource("/Rendering/Target_Vulkan_Triangle/Frame.java"))

    } yield ()

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
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities.addCompilationUnit
    for {
      _ <- make_class("VulkanApplication")
    } yield ()

    addCompilationUnit(make_frame_unit, names.mangle("Frame"))

  }



}

object VulkanApplicationProvider {
  type WithParadigm[P <: AnyParadigm] = VulkanApplicationProvider { val paradigm: P }
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
  )(
    _classTemplating: Templating.WithBase[oo.ClassContext, base.MethodBodyContext, base.type],
    _unitTemplating: Templating.WithBase[base.CompilationUnitContext, base.MethodBodyContext, base.type]
  )
  (impConstructor: Imperative.WithBase[oo.ConstructorContext, base.type])
  (_domain: RenderingDomain)
  : VulkanApplicationProvider.WithParadigm[base.type] =
    new VulkanApplicationProvider {
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
      override val classTemplating: Templating.WithBase[ooParadigm.ClassContext, paradigm.MethodBodyContext, paradigm.type] = _classTemplating
      override val unitTemplating: Templating.WithBase[paradigm.CompilationUnitContext, paradigm.MethodBodyContext, paradigm.type] = _unitTemplating
    }
}


