package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import example.expression.ExpressionDomain
import expression._
import expression.data.Eval
import expression.extensions._
import expression.instances.UnitSuite

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

//      s"public abstract R visit(${x.getClass.getSimpleName} exp);")

//  map.keys.foreach {
//    key =>
//      addImpl(op, key, Java(map(key)).statements())
//  }

//  registerImpl(new Eval, Map(
//
//    new Lit -> "return e.getValue();",
//    new Add -> "return e.getLeft().accept(this) + e.getRight().accept(this);",
//    new Sub -> "return e.getLeft().accept(this) - e.getRight().accept(this);",
//    new Mult -> "return e.getLeft().accept(this) * e.getRight().accept(this);",
//    new Divd -> "return e.getLeft().accept(this) / e.getRight().accept(this);",
//    new Neg -> "return -e.getExp().accept(this);"
//  ))

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

  // sample Driver
  @combinator object Driver {
    def apply:CompilationUnit = {

      var testNumber = 0
      val allGen:String = allTests.iterator.asScala.map(tst =>
        tst.iterator().asScala.map(tc => {
          // Convert the INSTANCE into proper instantiation code for the Visitor. A generator does this.
          val code:Option[com.github.javaparser.ast.expr.Expression] = representationCodeGenerators.instanceGenerators(tc.inst)
          if (code.isDefined) {
            testNumber = testNumber+1
            val op:Operation = tc.op
            val init:String = s"""Exp exp$testNumber = ${code.get.toString};"""
            val generated:String = op match{
              case _:Eval =>
                Java(s"""|public void test$testNumber() {
                    |  $init
                    |  Double result$testNumber = (Double) exp$testNumber.accept(new Eval());
                    |  assertEquals(${tc.expected.toString}, result$testNumber.doubleValue());
                    |}""".stripMargin).methodDeclarations().mkString("\n")

              case _:PrettyP =>
                Java(s"""|public void test$testNumber() {
                    |  $init
                    |String result$testNumber = (String) exp$testNumber.accept(new PrettyP());
                    |assertEquals("${tc.expected.toString}", result$testNumber);
                    |}""".stripMargin).methodDeclarations().mkString("\n")

              case _ => Java(s"""public void skip${op.name}$testNumber(){}""").methodDeclarations().mkString("\n")
            }

            generated
          }
        }).mkString("\n")
      ).mkString("\n")

      println("-=----------------------------")
      println(allGen)
      println("-=----------------------------")


      Java(s"""
              |package expression;
              |
              |import junit.framework.TestCase;
              |
              |public class TestSuite extends TestCase {
              |
              |    $allGen
              |
              |    public void oldCode() {
              |        System.out.println("======Add======");
              |        Add add = new Add(new Lit(7), new Lit(4));
              |        System.out.println(add.accept(new Eval()));
              |        System.out.println("======Sub======");
              |        Sub sub = new Sub(new Lit(7), new Lit(4));
              |        System.out.println(sub.accept(new Eval()));
              |        System.out.println("======Mult======");
              |        Mult mult = new Mult(new Lit(7), new Lit(4));
              |        System.out.println(mult.accept(new Eval()));
              |        System.out.println("======Divd======");
              |        Divd divd = new Divd(new Lit(7), new Lit(4));
              |        System.out.println(divd.accept(new Eval()));
              |        System.out.println("======Print======");
              |        Exp expr = new Mult(new Lit(5), new Add(new Lit(3), new Lit(-3)));
              |        System.out.println(expr.accept(new PrettyP()));
              |        Exp simpler = expr.accept(new SimplifyExpr());
              |        System.out.println(simpler.accept(new PrettyP()));
              |  }
              |}""".stripMargin).compilationUnit()
    }

    val semanticType:Type = driver
  }

}
