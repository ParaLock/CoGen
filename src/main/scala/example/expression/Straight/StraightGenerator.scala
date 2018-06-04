package example.expression.Straight

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.AbstractGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightGenerator extends AbstractGenerator {
  val domain:Domain
  import domain._

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:expressions.Exp) : Map[String,Expression] = {
//    exp.attributes.map(att => Java(s"${att.name}").expression[Expression]())
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation. */
  override def recurseOn(expr:Expression, op:Operation) : Expression = {
    Java(s"""$expr.${op.name}()""").expression()
  }

  /** Return designated Java type associated with type. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Exp").tpe()
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType ${op.name}() {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /** Throws run-time exception to catch when an operation/exp pair is missing. */
  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.getClass.getSimpleName}" """)
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(domain:Model, exp:expressions.Exp) : CompilationUnit = {
    val name = exp.toString

    val methods:Seq[MethodDeclaration] = domain.ops.map(methodGenerator(exp))
    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeGenerator(att.tpe)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    Java(s"""
            |public class $name extends Exp {
            |
            |  ${constructor.toString}
            |
            |  ${atts.mkString("\n")}
            |
            |  ${methods.mkString("\n")}
            |}""".stripMargin).compilationUnit()
  }

  /** Generate the base class. */
  def generateBase(domain:Model): CompilationUnit = {

    // Allow for operations to be void; if not an issue in future, then filter out here...
    val signatures:Seq[MethodDeclaration] = domain.ops.flatMap(op => {
      val retType = op.returnType match {
        case Some(tpe) => typeGenerator(tpe)
        case _ => Java("void").tpe
      }

      Java(s"public abstract $retType ${op.name}();").methodDeclarations()
    })

    // same every time
    Java(s"""|public abstract class Exp {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }
}







