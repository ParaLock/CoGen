package org.combinators.ep.generator.paradigm.ffi    /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply, ParametricPolymorphism}

case class CreateArray[Type](elementType: Type, dimension: Option[Int])
case class Get()
case class Set()
case class Length()

trait Arrays[Context] extends FFI {
  import base._
  import syntax._

  trait ArrayCapabilities {
    implicit val canCreate: Understands[Context, Apply[CreateArray[Type], Expression, Expression]]
    def create(elemTpe: Type, contents: Seq[Expression], dim:Option[Int]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[CreateArray[Type], Expression, Expression](CreateArray(elemTpe, dim), contents))

    implicit val canGet: Understands[Context, Apply[Get, Expression, Expression]]
    def get(array: Expression, pos:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Get, Expression, Expression](Get(), Seq(array, pos)))

    implicit val canSet: Understands[Context, Apply[Set, Expression, Expression]]
    def set(array: Expression, pos:Expression, value:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Set, Expression, Expression](Set(), Seq(array, pos, value)))

    implicit val canLength: Understands[Context, Apply[Length, Expression, Expression]]
    def length(array:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Length, Expression, Expression](Length(), Seq(array)))
  }
  val arrayCapabilities: ArrayCapabilities
}

object Arrays {
  type WithBase[Ctxt, B <: AnyParadigm] = Arrays[Ctxt] { val base: B }
}
