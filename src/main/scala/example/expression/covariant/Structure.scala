package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import example.expression.j.MethodMapper
import example.expression.{Base, ExpressionDomain}
import expression.data.{Add, Eval, Lit}
import expression.extensions.{Collect, Neg, PrettyP, Sub}
import expression._
import expression.types.Types

import scala.collection.JavaConverters._

/** Use Modularity2016 Java solution. Built from same domain model. */
trait Structure extends Base with SemanticTypes with MethodMapper {

  /** Add dynamic combinators as needed. */
  override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], model: DomainModel): ReflectedRepository[G] = {
    var updated = super.init(gamma, model)

    // Every extension needs its own FinalClass.
    model.data.asScala.foreach {
      sub:Exp => {
        updated = updated
          .addCombinator (new FinalClass(sub))
          .addCombinator (new SubInterface(sub))
      }
    }

    // all non-empty subsets of operations need their own class and operations
    val subsets:List[List[Operation]] = model.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)
    subsets.foreach {
      sub:List[Operation] => {
        if (sub.length == 1) {
          updated = updated.addCombinator(new AddOperation(sub.head))
        } else {
          // every subset gets its own interface
          updated = updated.addCombinator(new AddMultiOperationInterface(sub))

          model.data.asScala.foreach { exp: Exp => {
             updated = updated
              .addCombinator(new AddMultiOperation(sub, exp))
            }
          }
        }

        model.data.asScala.foreach { exp: Exp => {
            updated = updated
              .addCombinator(new FinalMultiClass(exp, sub))
          }
        }
      }
    }

    // implementations of operations: have to be defined. Note that these "raw Scala methods" could be replaced with tabular tool
    //
    //
    //  Eval     x  Lit, Neg, Add, Sub  ...  Mult Divide ...
    //  Print    |  pl,  pn,  pa,  ps
    //  Collect  |  cl,  cn,  ca,  cs   ...
    //  Simplify |  sl,  sn,  sa,  ss   ...


    // Should be able to cut implementation in half; note the duplicate combinators with Lists of
    // parameters, and sometimes with a single parameter.


    // Row entries for a given operation as expressed by the different column types
    def registerImpl (op:Operation, fm:FunctionMethod, map:Map[Exp,String]): Unit = {
      map.keys.foreach {
        key =>
          updated = updated
            .addCombinator(new AddDefaultImpl(op, fm, key, Java(map(key)).statements()))
      }
    }

    def registerExtension (op:Operation, map:Map[Exp,String]): Unit = {
      map.keys.foreach {
        key =>
          updated = updated
            .addCombinator(new AddExpOperation(key, op, Java(map(key)).statements()))
      }
    }


    // note default 'Eval' operation is handled specially since it is assumed to always exist in top Exp class
    registerImpl(new Eval,  new FunctionMethod("eval", Types.Int), Map(
      new Lit -> "return value();",
      new Neg -> "return -exp().eval();",
      new Add -> "return left().eval() + right().eval();",
      new Sub -> "return left().eval() - right().eval();"
    ))


    registerExtension(new PrettyP, Map(
      new Lit -> """return "" + value();""",
      new Add -> """return "(" + left().print() + " + " + right().print() + ")";""",
      new Sub -> """return "(" + left().print() + " - " + right().print() + ")";""",
      new Neg -> """return "-" + exp().print();""",
    ))

    val collectAll:String = """|java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                               |list.addAll(left().collectList());
                               |list.addAll(right().collectList());
                               |return list;
                               |""".stripMargin
    registerExtension(new Collect, Map(
      new Lit -> """|java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                    |list.add(value());
                    |return list;
                    |""".stripMargin,
      new Add -> collectAll,
      new Sub -> collectAll,
      new Neg -> """|java.util.List<Integer> list = new java.util.ArrayList<Integer>();
                    |list.addAll(exp().collectList());
                    |return list;
                    |""".stripMargin
    ))
    updated
  }



  /**
    * Construct class to represent subclass of Exp.
    *
    *  interface Lit extends Exp {
    *    int x();
    *    default int eval() { return x(); }
    *  }
    *
    *  but also recursive types:
    *
    *  interface Add extends Exp {
    *     Exp e1(); Exp e2();
    *     default int eval() {
    *      return e1().eval() + e2().eval();   $IMPLEMENT[
    *     }
    *   }
    *
    *  addImpl(new Eval, new Add, Java(s"""return e1().eval() + e2().eval();""").statements())
    *  addImpl(new Eval, new Lit, Java(s"""return x();""").statements())
    *
    * @param sub     Exp subclass whose interface we are generating
    */
  class SubInterface(sub:Exp) {
      def apply(): CompilationUnit = {
        val name = sub.getClass.getSimpleName

        val unit = Java (s"""
             |package ep;
             |public interface $name extends Exp {}
            """.stripMargin).compilationUnit()

        sub.ops.asScala.foreach {
          case att: Attribute =>
            val tpe = Type_toString(att.attType)

            val fields:Seq[MethodDeclaration] = Java(s"""$tpe ${att.attName}();""").methodDeclarations()

            fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          case _:FunctionMethod =>
        }

        unit
      }

    val semanticType:Type = ep(ep.interface, sub)
  }



  /**
    * i.e., LitFinal
    *
    * public class LitFinal implements Lit {
    *   Integer value;
    *   public LitFinal(int value) { this.value = value; }
    *   public Integer value() { return value; }
    * }
    *
    * @param sub  type (i.e., "Lit") for which *Final class is to be synthesized.
    */
  class FinalClass(sub:Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java (
        s"""
           |package ep;
           |public class ${name}Final implements $name {}
       """.stripMargin).compilationUnit()

      var params:Seq[String] = Seq.empty
      var cons:Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields = Java(s"""
                       |private $tpe ${att.attName};
                       |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$tpe ${att.attName}"
          cons   = cons   :+ s"  this.${att.attName} = ${att.attName};"

          val methods = Java(
            s"""
               |public $tpe ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _:FunctionMethod =>
      }

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.

      // make constructor
      val constructor = Java(
        s"""
           |public ${sub.getClass.getSimpleName}Final (${params.mkString(",")}) {
           |   ${cons.mkString("\n")}
           |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      unit
    }

    val semanticType:Type = ep(ep.finalType, sub)
  }

  /**
    * i.e., LitFinal
    *
    *

    class AddCFinal implements AddC {
    ExpC e1, e2;
    AddCFinal(ExpC e1, ExpC e2) {
        this.e1 = e1;
        this.e2 = e2;
    }
    public ExpC e1() { return e1; }
    public ExpC e2() { return e2; }
    }

    FinalMultiClass(new Add, List(new Collect))

    * @param sub    Type (i.e., Add) for which a final class is to be constructed...
    * @param ops    ...based on a List[Operation] that specifies desired capabilities
    */
  class FinalMultiClass(sub:Exp, ops:List[Operation]) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName
      val combined = ops.map(_.getClass.getSimpleName).mkString("")

      val unit = Java (s"""
                       |package ep;
                       |public class $name${combined}Final implements $name$combined {}
                       """.stripMargin).compilationUnit()

      var params:Seq[String] = Seq.empty
      var cons:Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>

          // override as needed to deal with co-variant specializations
          var revisedTypeName = Type_toString(att.attType)
          if (att.attType == Types.Exp) {
            revisedTypeName = combined
          }
          val fields:Seq[FieldDeclaration] = Java(s"""
                                                     |private $revisedTypeName ${att.attName};
                                                     |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$revisedTypeName ${att.attName}"
          cons   = cons   :+ s"  this.${att.attName} = ${att.attName};"

          val methods:Seq[MethodDeclaration] = Java(
            s"""
               |public $revisedTypeName ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }


        case _:FunctionMethod =>
      }

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.

      // make constructor
      val constructor = Java(
        s"""
           |public $name${combined}Final (${params.mkString(",")}) {
           |   ${cons.mkString("\n")}
           |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      unit
    }

    val semanticType:Type = ep(ep.finalType, sub, ops)
  }

  /**
    * Given an interface for a type, adds a default implementation of given operation
    *
    * @param op       Desired Operation
    * @param fm       Domain Model Function Method that models this operation
    * @param sub      The subType associated with....
    * @param stmts    ...the statements containing an implementation of Operation for SubType.
    */
  class AddDefaultImpl(op:Operation, fm:FunctionMethod, sub:Exp, stmts:Seq[Statement]) {
    def apply(unit:CompilationUnit): CompilationUnit = {

      val tpe = Type_toString(fm.returnType)
      val name = fm.name

      // methods are marked as default later
      val methods = Java(s"$tpe $name() { ${stmts.mkString} }").methodDeclarations()

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m) }
      unit
    }

    val semanticType:Type = ep(ep.interface, sub) =>: ep(ep.defaultMethods, sub, op)
  }


  /**
    * Given an extension to Exp and a given operation (and its stmts implementation) produce an
    * interface with default method. Overide methods that are of class Exp. Thus: AddExpOperation (Add, PrettyP, ...)
    *
    * interface AddPrettyP extends Add, PrettyP {
    *    PrettyP left();
    *    PrettyP right();
    *    default String print() {
    *      return "(" + left().print() + " + " + right().print() + ")";
    *    }
    * }
    *
    * @param exp    SubType (i.e., Add) for which an operation is to be defined.
    * @param op     Operation to be defined.
    * @param stmts  Default set of statements for implementation
    */
  class AddExpOperation(exp:Exp, op: Operation, stmts:Seq[Statement]) {
    def apply() : CompilationUnit = {
      val opName = op.getClass.getSimpleName
      val expName = exp.getClass.getSimpleName

      val unit:CompilationUnit = Java(s"""
           |package ep;
           |interface $expName$opName extends $expName, $opName { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      // methods are marked as default later
      val methods:Seq[MethodDeclaration] = Java(
        s"""
           |$tpe ${op.name}() {
           |   ${stmts.mkString("\n")}
           |}
         """.stripMargin).methodDeclarations()

      // reclassify an field of type Exp with the more precise $expName
      // PrettyP left();
      // PrettyP right();
      exp.ops.asScala.foreach {
        case att: Attribute =>
          // only redefine if originally the Exp field.
          if (att.attType == Types.Exp) {
            val fields: Seq[MethodDeclaration] = Java(s"""$opName ${att.attName}();""").methodDeclarations()

            fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }
          }

        case _:FunctionMethod =>
      }

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m) }

      unit
    }

    val semanticType:Type = ep(ep.interface, exp, op)
  }

  //  interface ExpPC extends ExpP, ExpC{}
  //  interface LitPC extends ExpPC, LitP, LitC{}
  //  interface AddPC extends ExpPC, AddP, AddC {
  //    ExpPC e1(); ExpPC e2();
  //  }
  class AddMultiOperation(ops: List[Operation], exp:Exp) {
    def apply() : CompilationUnit = {
      val name = exp.getClass.getSimpleName

      val combined:String = ops.map(_.getClass.getSimpleName).mkString("")
      val commas:String = ops.map(name + _.getClass.getSimpleName).mkString(",")

      val unit:CompilationUnit = Java(s"""|package ep;
                                          |interface $name$combined extends $combined,$commas { }
                                          |""".stripMargin).compilationUnit()

      // grab any Exp operations and be sure they appear as $combined
      exp.ops.asScala.foreach {
         case att: Attribute =>
              // only redefine if originally the Exp field.
              if (att.attType == Types.Exp) {
                val fields: Seq[MethodDeclaration] = Java(s"""$combined ${att.attName}();""").methodDeclarations()

                fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }
              }

            case _:FunctionMethod =>
        }

      unit
    }

    val semanticType:Type = ep(ep.interface, exp, ops)
  }

 // interface ExpP extends Exp { String print(); }
  class AddOperation(op: Operation) {
   def apply() : CompilationUnit = {
     val name = op.getClass.getSimpleName

     val unit:CompilationUnit = Java(s"""
              |package ep;
              |interface $name extends Exp { }
              |""".stripMargin).compilationUnit()

     val tpe = Type_toString(op.`type`)

       val methods:Seq[MethodDeclaration] = Java(s"""$tpe ${op.name}();""").methodDeclarations()

       methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

     unit
   }

   val semanticType:Type = ep(ep.interface, op)
 }


  // interface ExpP extends Exp { String print(); }
  class AddMultiOperationInterface(ops: List[Operation]) {
    def apply() : CompilationUnit = {
      val combined:String = ops.map(_.getClass.getSimpleName).mkString("")
      val names:String = ops.map(_.getClass.getSimpleName).mkString(",")

      val unit:CompilationUnit = Java(s"""
                                         |package ep;
                                         |interface $combined extends $names { }
                                         |""".stripMargin).compilationUnit()

      unit
    }

    val semanticType:Type = ep(ep.interface, ops)
  }


  /** Generate from domain. */
  @combinator object BaseExpInterface {

    val exp:Exp = new Exp

    def apply() : CompilationUnit = {
      val unit:CompilationUnit = Java(s"""
            |package ep;
            |interface Exp { }
            |""".stripMargin).compilationUnit()

      exp.ops.asScala.foreach {
        case func:FunctionMethod =>
          val tpe = Type_toString(func.returnType)

          val methods:Seq[MethodDeclaration] = Java(s"""$tpe ${func.name}();""").methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _ =>
        }

      unit
      }

    val semanticType:Type = ep(ep.interface, exp)
  }

  // sample Driver
  @combinator object Driver {
    def apply:CompilationUnit = Java(s"""
         |package ep;
         |
         |public class Driver {
         |  public static void main(String[] args) {
         |    System.out.println("======Add======");
         |    Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
         |    System.out.println(add.eval());
         |    System.out.println("======Sub======");
         |    Sub sub = new SubFinal(new LitFinal(7), new LitFinal(4));
         |    System.out.println(sub.eval());
         |    System.out.println("======Print======");
         |
         |    /* the line below causes compile-time error, if now commented out. */
         |    //AddPrettyPFinal exp = new AddPrettyPFinal(new LitFinal(7)), new LitFinal(4));
         |    AddPrettyPFinal prt = new AddPrettyPFinal(new LitPrettyPFinal(7), new LitPrettyPFinal(4));
         |    System.out.println(prt.print() + " = " + prt.eval());
         |    System.out.println("======CollectLiterals======");
         |    AddCollectFinal addc = new AddCollectFinal(new LitCollectFinal(3), new LitCollectFinal(4));
         |    System.out.println(addc.collectList().toString());
         |    System.out.println("======Composition: Independent Extensibility======");
         |    AddPrettyPCollectFinal addpc = new AddPrettyPCollectFinal(new LitPrettyPCollectFinal(3), new LitPrettyPCollectFinal(4));
         |    System.out.println(addpc.print() + " = " + addpc.eval() + " Literals: " + addpc.collectList().toString());
         |  }
         |}""".stripMargin).compilationUnit()

    val semanticType:Type = driver
  }

}
