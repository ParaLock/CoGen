package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, M0, M5, MathDomain, OperationDependency}
import org.combinators.templating.twirl.Java

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with  M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain
  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Tree => Java(s"tree.Tree").tpe()      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  /** helper method for testing when goal is to check equality between tree objects . */
  def treesAreSame(treeExp1:Expression, treeExp2:Expression) : Expression = {
    Java(s"${treeExp1.toString}.same(${treeExp2.toString})").expression[Expression]()
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        // TODO: replace hard-coded DefinedSubTypes with dependent operation getSubTypeIdentifier and dispatch accordingly.

        // different strategies have different means of accessing attributes, either directly or via
        // getXXX methods. This logic method must defer that knowledge to later.
        // "this" is only valid expression when datatype as class
        exp match {   // was $litValue     ;
          case Lit =>   // ${exp.hashCode()}
            val attParams = atts.map(att => att._2.toString).mkString(",")
            result(Java(s" new tree.Node(java.util.Arrays.asList(new tree.Leaf($attParams)), ${identify(exp, Identifier)}) ").expression[Expression]())

          case Add|Sub|Mult|Divd|Neg =>
            val params = atts.map(att => att._2.toString + ".astree()").mkString(",")
            result(Java(s" new tree.Node(java.util.Arrays.asList($params), ${identify(exp, Identifier)} ) ").expression[Expression]())
          }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : MethodDeclaration = {

    // EXTRACT all SameTestCase ones and handle here
    var skip:Seq[domain.TestCase] = Seq.empty

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1

      test match {
        case ctc: SameTestCase =>
          val tree1 = dependentDispatch(convert(ctc.inst1), AsTree)  // was just dispatch
          val tree2 = dependentDispatch(convert(ctc.inst2), AsTree)

          val same = Java(s"$tree1.same($tree2)").expression[Expression]()

          if (ctc.result) {
            Java(s"assertTrue($same);").statements
          } else {
            Java(s"assertFalse($same);").statements
          }
        case _ =>
          skip = skip :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    addStatements(super.testMethod(skip), stmts)
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator :+ testMethod(M5_tests)
  }
}
