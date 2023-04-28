package org.combinators.ep.generator

import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}

trait Features {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
}

object Features {

  type WithParadigm[P <: AnyParadigm] = Features {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (
     nameProvider: NameProvider[base.syntax.Name],
     imp: Imperative.WithBase[base.MethodBodyContext, base.type],
     //impConstructor: Imperative.WithBase[ObjectOriented.WithBase[base.type], base.type],
     oo: ObjectOriented.WithBase[base.type],
     con: Console.WithBase[base.MethodBodyContext, base.type],
     arr: Arrays.WithBase[base.MethodBodyContext, base.type],
     assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
     eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
     ffiarith: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
     ffiassert: Assertions.WithBase[base.MethodBodyContext, base.type],
     ffiequal: Equality.WithBase[base.MethodBodyContext, base.type],
  )
  : Features.WithParadigm[base.type] =
    new Features {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffiarith
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiassert
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = ffiequal
    }

}
