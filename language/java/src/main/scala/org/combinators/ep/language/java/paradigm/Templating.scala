package org.combinators.ep.language.java.paradigm

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{GetTemplateVar, LoadFragment, TemplateVar, Templating => TP}
import org.combinators.ep.language.java.{ClassCtxt, CtorCtxt, MethodBodyCtxt, CompilationUnitCtxt}

import java.nio.charset.StandardCharsets

case class ReplaceCommentInsideClass[T](key: String) extends TemplateVar[MethodBodyCtxt, ClassCtxt, T] {
  def make_placement_context(context: ClassCtxt): MethodBodyCtxt = {

    val comment = context.cls.getAllContainedComments().stream()
      .filter(comment => comment.getContent().trim().equals(key))
      .findFirst()

    val parentMethod = comment.get.getParentNode().get.getParentNode().get
    val method = parentMethod.asInstanceOf[MethodDeclaration]

    val methodBodyCtxt = MethodBodyCtxt(
      context.resolver,
      Seq.empty,
      method
    )

    methodBodyCtxt

  }

  def place(generator: Generator[MethodBodyCtxt, T], into: ClassCtxt): ClassCtxt = {

    val methodBodyContext = make_placement_context(into)
    val comment = into.cls.getAllContainedComments().stream()
      .filter(comment => comment.getContent().trim().equals(key))
      .findFirst()

    val parent = comment.get.getParentNode().get.getParentNode().get

    val (newMethodBodyContext, _) = Command.runGenerator(generator, methodBodyContext)

    into.cls.replace(
      parent,
      newMethodBodyContext.method
    )

    into.copy(cls = into.cls)
  }
}


trait TemplatingMethodInClass[AP <: AnyParadigm] extends TP[ClassCtxt, MethodBodyCtxt] {
  val base: AP

  import base.syntax._
  import base.config

  val templatingCapabilities = new TemplatingCapabilities {
    override implicit val canLoadFragment: Understands[ClassCtxt, LoadFragment] = new Understands[ClassCtxt, LoadFragment] {
      /** Returns the updated context and the result of the command. */
      override def perform(context: ClassCtxt, command: LoadFragment): (ClassCtxt, Unit) = {

        val sourceStream = command.fragmentUrl.openStream()
        val sourceStr = new String(sourceStream.readAllBytes, StandardCharsets.UTF_8)

        val cls = StaticJavaParser.parseTypeDeclaration(sourceStr).asInstanceOf[ClassOrInterfaceDeclaration];
        (context.copy(cls=cls),())
      }
    }

    override implicit def canGetTemplateVar[T]: Understands[ClassCtxt, GetTemplateVar[MethodBodyCtxt, ClassCtxt, T]] = new Understands[ClassCtxt, GetTemplateVar[MethodBodyCtxt, ClassCtxt, T]] {
      /** Returns the updated context and the result of the command. */
      override def perform(context: ClassCtxt, command: GetTemplateVar[MethodBodyCtxt, ClassCtxt, T]): (ClassCtxt, TemplateVar[MethodBodyCtxt, ClassCtxt, T]) = {

        (context, ReplaceCommentInsideClass(command.templateVar))

      }
    }
  }
}


trait TemplatingMethodInCompilationUnit[AP <: AnyParadigm] extends TP[CompilationUnitCtxt, MethodBodyCtxt] {
  val base: AP

  import base.syntax._
  import base.config

  val templatingCapabilities = new TemplatingCapabilities {
    override implicit val canLoadFragment: Understands[CompilationUnitCtxt, LoadFragment] = new Understands[CompilationUnitCtxt, LoadFragment] {
      /** Returns the updated context and the result of the command. */
      override def perform(context: CompilationUnitCtxt, command: LoadFragment): (CompilationUnitCtxt, Unit) = {

        val sourceStream = command.fragmentUrl.openStream()
        val sourceStr = new String(sourceStream.readAllBytes, StandardCharsets.UTF_8)

        val compUnit = StaticJavaParser.parse(sourceStr)

        val tgtPackage = config.targetPackage.clone

        compUnit.setPackageDeclaration(tgtPackage)

        (context.copy(unit=compUnit),())
      }
    }

    override implicit def canGetTemplateVar[T]: Understands[CompilationUnitCtxt, GetTemplateVar[MethodBodyCtxt, CompilationUnitCtxt, T]] = ???
  }
}
