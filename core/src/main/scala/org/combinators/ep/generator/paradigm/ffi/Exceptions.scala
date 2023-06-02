package org.combinators.ep.generator.paradigm.ffi    /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Apply}

case class Exception[Expression,Stmt](exp:Expression) extends Command {
  type Result = Stmt
}

case class AddExceptionHandler[Statement, Context, Type, Name](
  tryContents: Generator[Context, Unit],
  catchContents: Seq[(Type, Name, Generator[Context, Unit])],
  finallyContents: Option[Generator[Context, Unit]],
) extends Command {
  type Result = Unit
}


trait Exceptions[Context] extends FFI {
  import base.syntax._

  trait ExceptionCapabilities {
    implicit val canRaise: Understands[Context, Exception[Expression, Statement]]

    def raise(exp: Expression): Generator[Context, Statement] =
      AnyParadigm.capability(Exception[Expression, Statement](exp))

    implicit val canAddExceptionHandler: Understands[Context, AddExceptionHandler[Statement, Context, Type, Name]]

    def addExceptionHandler(
                             tryContents: Generator[Context, Unit],
                             catchContents: Seq[(Type, Name, Generator[Context, Unit])],
                             finallyContents: Option[Generator[Context, Unit]] = None,
                           ): Generator[Context, Unit] = {
      AnyParadigm.capability[Context, Unit, AddExceptionHandler[Statement, Context, Type, Name]](
        AddExceptionHandler(
          tryContents,
          catchContents,
          finallyContents
        )
      )
    }

  }
  
  val exceptionCapabilities: ExceptionCapabilities
}

object Exceptions {
  type WithBase[Ctxt, B <: AnyParadigm] = Exceptions[Ctxt] { val base: B }
}
