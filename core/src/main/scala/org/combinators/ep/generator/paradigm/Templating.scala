package org.combinators.ep.generator.paradigm
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative

import java.net.URL
import java.nio.file.Path

trait TemplateVar[InnerCtxt, OuterCtxt, T] {

  val key: String

  def make_placement_context(context: OuterCtxt): InnerCtxt

  def place(generator: Generator[InnerCtxt, T], into: OuterCtxt): OuterCtxt
}

case class LoadFragment(fragmentUrl: URL) extends Command {
  type Result = Unit
}

case class Replace[InnerCtxt, OuterCtxt, T](templateVar: TemplateVar[InnerCtxt, OuterCtxt, T], generator: Generator[InnerCtxt, T]) extends Command {
  type Result = Unit
}

case class GetTemplateVar[InnerCtxt, OuterCtxt, T](templateVar: String) extends Command {
  type Result = TemplateVar[InnerCtxt, OuterCtxt, T]
}

trait Templating[OuterContext, InnerContext] {
  val base: AnyParadigm
  import base._
  import syntax._

  trait TemplatingCapabilities {
    implicit val canLoadFragment: Understands[OuterContext, LoadFragment]

    def loadFragment(fragmentUrl: URL) : Generator[OuterContext, Unit] = {
      AnyParadigm.capability(LoadFragment(fragmentUrl))
    }

    implicit def canReplace[T]: Understands[OuterContext, Replace[InnerContext, OuterContext, T]] = new Understands[OuterContext, Replace[InnerContext, OuterContext, T]] {
      /** Returns the updated context and the result of the command. */
      override def perform(context: OuterContext, command: Replace[InnerContext, OuterContext, T]): (OuterContext, Unit) = {
        (command.templateVar.place(command.generator,context), ())
      }
    }


    def replace[T](templateVar: TemplateVar[InnerContext, OuterContext, T], generator: Generator[InnerContext, T]): Generator[OuterContext, Unit] = {
      AnyParadigm.capability(Replace[InnerContext, OuterContext, T](templateVar, generator))
    }


    implicit def canGetTemplateVar[T]: Understands[OuterContext, GetTemplateVar[InnerContext, OuterContext, T]]
    def getTemplateVar[T](templateVar: String): Generator[OuterContext, TemplateVar[InnerContext, OuterContext, T]] = {
      AnyParadigm.capability(GetTemplateVar[InnerContext, OuterContext, T](
        s"@COGEN<$templateVar>"
      ))
    }


  }

  val templatingCapabilities: TemplatingCapabilities
}

object Templating {
  type WithBase[OuterContext, InnerContext, B <: AnyParadigm] = Templating[OuterContext, InnerContext] { val base: B }
}
