package org.combinators.ep.language.java.paradigm.ffi     /*DI:LD:AI*/

import com.github.javaparser.{ParseResult, StaticJavaParser}
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{MethodDeclaration, Parameter}
import com.github.javaparser.ast.{ImportDeclaration, NodeList}
import com.github.javaparser.ast.expr.{AssignExpr, BooleanLiteralExpr, Expression, IntegerLiteralExpr, NameExpr, ObjectCreationExpr, VariableDeclarationExpr}
import com.github.javaparser.ast.stmt.{CatchClause, ThrowStmt, TryStmt}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{Apply, ffi}
import org.combinators.ep.generator.paradigm.ffi.{AddExceptionHandler, Assert, Exceptions => Excptns}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.language.java.{ContextSpecificResolver, MethodBodyCtxt, ProjectCtxt}

class Exceptions[AP <: AnyParadigm](val base: AP) extends Excptns[MethodBodyCtxt] {

  val exceptionCapabilities: ExceptionCapabilities =
    new ExceptionCapabilities {

      override implicit val canAddExceptionHandler: Understands[
        MethodBodyCtxt,
        AddExceptionHandler[Expression, Statement, MethodBodyCtxt, Type, Name]
      ] = new Understands[
        MethodBodyCtxt,
        AddExceptionHandler[Expression, Statement, MethodBodyCtxt, Type, Name]
      ] {

        override def perform(context: MethodBodyCtxt, command: AddExceptionHandler[Expression, Statement, MethodBodyCtxt, Type, Name]): (MethodBodyCtxt, Unit) = {

          val tryStmt: TryStmt = new TryStmt()

          if(command.tryExpr.isDefined) {
            tryStmt.getResources().add(command.tryExpr.get);
          }

          val (newContext, _) = Command.runGenerator(
            command.tryContents,
            MethodBodyCtxt(
              context.resolver,
              context.extraImports,
              new MethodDeclaration()
            )
          )

          tryStmt.setTryBlock(newContext.method.getBody.get)

          val catchClauses: NodeList[CatchClause] = new NodeList[CatchClause]()

          for (catchContents <- command.catchContents) {
            val (newContext, _) = Command.runGenerator(
              catchContents._3,
              MethodBodyCtxt(
                context.resolver,
                context.extraImports,
                new MethodDeclaration()
              )
            )

            val catchClause: CatchClause =
              new CatchClause(
                new Parameter(
                  catchContents._1,
                  catchContents._2.toAST
                ),
                newContext.method.getBody.get
              )

            catchClauses.add(catchClause)
          }

          tryStmt.setCatchClauses(catchClauses)

          if (command.finallyContents.isDefined) {
            val (newContext, _) = Command.runGenerator(
              command.finallyContents.get,
              MethodBodyCtxt(
                context.resolver,
                context.extraImports,
                new MethodDeclaration()
              )
            )
            tryStmt.setFinallyBlock(newContext.method.getBody.get);
          }

          context.method.getBody.get().addStatement(tryStmt)

          (context.copy(), ())
        }
      }

      override implicit val canRaise: Understands[MethodBodyCtxt, ffi.Exception[Expression, Statement, Type]] = {
        new Understands[MethodBodyCtxt, ffi.Exception[Expression, Statement, Type]] {
          def perform(
                       context: MethodBodyCtxt,
                       command: ffi.Exception[Expression, Statement, Type]
                     ): (MethodBodyCtxt, Statement) = {

            var excepName = "RuntimeException"
            if(command.excepType.isDefined) {
              excepName = command.excepType.get.asString()
            }

            val ex = new ObjectCreationExpr(null, ObjectOriented.nameToType(ObjectOriented.fromComponents(excepName)), new NodeList(command.exp))
            val throwStatement = new ThrowStmt(ex)
            (context, throwStatement)
          }
        }
      }
    }

  override def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        (context, ())
      }
    })
}
