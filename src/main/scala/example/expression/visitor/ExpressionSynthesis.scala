package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import example.expression.ExpressionDomain
import expression._
import expression.data.Eval
import expression.extensions._
import expression.instances.{Instance, Lit, UnitSuite}
import expression.operations.SimplifyExpr

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel, val tests:UnitSuite) extends ExpressionDomain(domain, tests) with SemanticTypes  {

  // These take care of registering the necessary code generators, one for each operator.
  domain.data.asScala
    .foreach(exp => {
      val comb:com.github.javaparser.ast.expr.Expression = representationCodeGenerators.evalGenerators(exp).get

      addImpl(new Eval, exp,
        Java(s"""return ${comb.toString};""").statements())
    })

  domain.data.asScala
    .foreach(exp => {
      val comb:com.github.javaparser.ast.expr.Expression = representationCodeGenerators.prettypGenerators(exp).get

      addImpl(new PrettyP, exp,
        Java(s"""return ${comb.toString};""").statements())
    })

  domain.data.asScala
    .foreach(exp => {
      val comb:Seq[Statement] = representationCodeGenerators.collectGenerators(exp).get

      addImpl(new Collect, exp, comb)
    })

  /** Construct visitor abstract class. */
  @combinator object Visitor {
    def apply(): CompilationUnit = {
      val signatures = domain.data.asScala
        .map(x => s"public abstract R visit(${x.getClass.getSimpleName} exp);").mkString("\n")

      Java (s"""
               |package expression;
               |/*
               | * A concrete visitor describes a concrete operation on expressions. There is one visit
               | * method per type in the class hierarchy.
               | */
               |public abstract class Visitor<R> {
               |
           |$signatures
               |}
         """.stripMargin).compilationUnit()
    }

    val semanticType:Type = generated(generated.visitor)
  }

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  @combinator object BaseExpClass {
    def apply() : CompilationUnit =
      Java(s"""
              |package expression;
              |
         |public abstract class Exp {
              |    public abstract <R> R accept(Visitor<R> v);
              |}
              |""".stripMargin).compilationUnit()

    val semanticType:Type = exp(exp.base, new Exp)
  }

  /** Works on any subclass of Exp to produce the base class structure for a sub-type of Exp. */
  class BaseClass(expr:Exp) {
    def apply(): CompilationUnit = {

      val name = expr.getClass.getSimpleName
      Java(s"""package expression; public class $name extends Exp { }""".stripMargin).compilationUnit()
    }

    // semantic type is based on the subclass (i.e., it will be exp('Base, 'Lit) or exp('Base, 'Add)
    val semanticType:Type = exp(exp.base, expr)
  }

  /**
    * Construct class to represent subclass of Exp.
    *
    * @param sub    sub-type of Exp (i.e., Lit) for whom implementation class is synthesized.
    */
  class ImplClass(sub:Exp) {
    def apply(unit:CompilationUnit): CompilationUnit = {

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
      var params:Seq[String] = Seq.empty
      var cons:Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val capAtt = att.attName.capitalize
          val tpe = Type_toString(att.attType)
          val fields:Seq[FieldDeclaration] = Java(s"private $tpe ${att.attName};").fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$tpe ${att.attName}"
          cons   = cons   :+ s"  this.${att.attName} = ${att.attName};"

          // make the set/get methods
          val methods:Seq[MethodDeclaration] = Java(s"""
                                                       |public $tpe get$capAtt() { return ${att.attName};}
                                                       |public void set$capAtt($tpe val) { this.${att.attName} = val; }
                      """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _ =>
      }

      // make constructor and add to class
      val constructor = Java(s"""
                                |public ${sub.getClass.getSimpleName} (${params.mkString(",")}) {
                                |   ${cons.mkString("\n")}
                                |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      // make accept method call and add to class
      val visitor = Java (s"""
                             |public <R> R accept(Visitor<R> v) {
                             |   return v.visit(this);
                             |}
                   """.stripMargin).methodDeclarations()

      visitor.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

      unit
    }

    val semanticType:Type = exp(exp.base, sub) =>: exp(exp.visitor,sub)
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  class OpImpl(op:Operation) {
    def apply: CompilationUnit = {

      val name = op.getClass.getSimpleName
      val tpe = Type_toString(op.`type`)

      //implementations
      val methods = getImplementation(op)

      val mds:Iterable[MethodDeclaration] = methods.values
      val signatures = mds.mkString("\n")

      val s = Java(s"""|package expression;
                       |public class $name extends Visitor<$tpe>{
                       |$signatures
                       |}""".stripMargin)

      s.compilationUnit()
    }

    val semanticType:Type = ops (ops.visitor,op)
  }

  /**
    * Construct JUnit test cases for each registered expression.
    *
    * Within the test case method, each of the registered operations are evaluated to confirm the expect value
    * matches the actual value.
    *
    * Sure would like to convert this into a generator, since then (in covariant) I would generate the appropriate
    * instantiation based on the domain model available at the time. Note that this might still be possible,
    * assuming the representationCodeGenerators has access to the top-level domain model.
    */
  @combinator object Driver {
    def apply:CompilationUnit = {

      // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
      var testNumber = 0
      val allGen:String = allTests.iterator.asScala.map(tst => {
        // build up expression for which all tests are defined.
        val code:Option[com.github.javaparser.ast.expr.Expression] = representationCodeGenerators.instanceGenerators(tst.expression)
        testNumber = testNumber+1
        val init:String = s"""Exp exp$testNumber = ${code.get.toString};"""

        // each individual test case is evaluated within the context of this expression
        var resultNumber = 0
        val blocks:Seq[Statement] = tst.iterator().asScala.flatMap(tc => {

          //  val comb:Seq[Statement] = representationCodeGenerators.evalGenerators(tc).get

          resultNumber = resultNumber + 1
          // Convert the INSTANCE into proper instantiation code for the Visitor. A generator does this.
          val op:Operation = tc.op

          // hah! once again, I am stuck using a case statement, which will need to be expanded
          // into a code generator for maximum power
          val generated:Seq[Statement] = op match{
            case _:Eval =>
              Java(s"""|  Double result$resultNumber = (Double) exp$testNumber.accept(new Eval());
                       |  assertEquals(${tc.expected.toString}, result$resultNumber.doubleValue());
                       |""".stripMargin).statements()

            case _:PrettyP =>
              Java(s"""|  String result$resultNumber = (String) exp$testNumber.accept(new PrettyP());
                       |  assertEquals("${tc.expected.toString}", result$resultNumber);
                       |""".stripMargin).statements()

            // unless I export the full domain model visitor pattern into the solution domain,
            // we must rely on the prettyP operation for our success.
            case _:SimplifyExpr => {

              val expectedCode: Option[com.github.javaparser.ast.expr.Expression] = representationCodeGenerators.instanceGenerators(tc.expected.asInstanceOf[Instance])
              if (expectedCode.isDefined) {
                val initExpected: String = s"""Exp expectedExp$testNumber = ${expectedCode.get.toString};"""

                // TODO: Create an equal visitor for use instead of depending on prettyP
                val str: String =
                  s"""
                     |  $initExpected
                     |  Exp result$resultNumber = (Exp) exp$testNumber.accept(new SimplifyExpr());
                     |  assertEquals(expectedExp$testNumber.accept(new PrettyP()), result$resultNumber.accept(new PrettyP()));
                     |""".stripMargin
                Java(str).statements()
              }
              else {
                // skip for lack of anything better.
                Java("// skip${op.name}$testNumber(){}\n").statements()
              }
            }

            case  c:Collect => {
              var expected:String = "java.util.List<Double> match = new java.util.ArrayList<Double>();"

              expected += tc.expected.asInstanceOf[java.util.List[Lit]].asScala.map(value => {
                "match.add(" + value.value + ");"
              }).mkString("\n")

              Java(s"""|  java.util.List<Double> result$resultNumber = (java.util.List<Double>) exp$testNumber.accept(new Collect());
                       |  $expected
                       |  assertEquals(match, result$resultNumber);
                       |""".stripMargin).statements()
            }

            case _ => Java(s"""// skip${op.name}$testNumber(){}""").statements()
          }

          generated
        }).toSeq

        val codeBlock:String = s"""|public void testEval$testNumber() {
                                   |  $init
                                   |  ${blocks.mkString("\n")}
                                   |}""".stripMargin
        Java(codeBlock).methodDeclarations().mkString("\n")
    }).mkString("\n")

      Java(s"""|package expression;
               |import junit.framework.TestCase;
               |public class TestSuite extends TestCase {
               |    $allGen
               |}""".stripMargin).compilationUnit()
    }

    val semanticType:Type = driver
  }

}
