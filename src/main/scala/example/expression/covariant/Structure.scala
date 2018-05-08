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
import expression.extensions._
import expression._
import expression.operations.SimplifyExpr
import expression.types.Types
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Use Modularity2016 Java solution. Built from same domain model. */
trait Structure extends Base with SemanticTypes with MethodMapper {

  /** Add dynamic combinators as needed. */
  override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], model: DomainModel): ReflectedRepository[G] = {
    var updated = super.init(gamma, model)

    // Every extension needs its own FinalClass. Surely there is a scala pattern that can avoid these
    // recursive helper methods...
//    updated = updateTypes(model, updated)
    updateTypes(model)

//    // if there is a parent, apply update types method, and either get it and apply or just return rep
//    def updateTypes(dm: DomainModel, rep:ReflectedRepository[G]): ReflectedRepository[G] = {
//      dm.data.asScala.foldLeft(dm.getParent.map(updateTypes(_,rep)).orElse(rep)) ((rep,exp) =>
//        rep.addCombinator(new FinalClass(exp))
//          .addCombinator(new SubInterface(exp))
//      )
//
  def updateTypes(dm: DomainModel): Unit = {
      dm.data.asScala.foreach {
        sub: Exp => {
          updated = updated
            .addCombinator(new FinalClass(sub))
            .addCombinator(new SubInterface(sub))
        }
      }

      if (dm.getParent.isPresent) {
        updateTypes(dm.getParent.get)
      }
    }

    def addMulti(dm: DomainModel, sub: List[Operation]): Unit = {
      dm.data.asScala.foreach { exp: Exp => {
        val st: Type = ep(ep.interface, exp, sub)
        // ep(ep.interface, exp, ops)
        println("++++++ add MultiOperation Interface:" + st)
        updated = updated
          .addCombinator(new AddMultiOperation(sub, exp))
      }
      }
      if (dm.getParent.isPresent) {
        addMulti(dm.getParent.get, sub)
      }
    }

    def addFinal(dm: DomainModel, sub: List[Operation]): Unit = {
      dm.data.asScala.foreach { exp: Exp => {
        updated = updated
          .addCombinator(new FinalMultiClass(sub, exp))
      }
      }
      if (dm.getParent.isPresent) {
        addFinal(dm.getParent.get, sub)
      }
    }

    // all non-empty subsets of operations need their own class and operations
    //val subsets:List[List[Operation]] = model.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)
    val subsets:List[List[Operation]] = model.flatten.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)
//
//    def updateSubsets(dm: DomainModel): List[List[Operation]] = {
//      val subs: List[List[Operation]] = dm.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)
//
//      if (dm.getParent.isPresent) {
//        subs ++ updateSubsets(dm.getParent.get)
//      } else {
//        subs
//      }
//    }
//    val subsets: List[List[Operation]] = updateSubsets(model)

    subsets.foreach {
      sub: List[Operation] => {
        println (">>> sub:" + sub.toString)
        if (sub.length == 1) {
          updated = updated.addCombinator(new AddOperation(sub.head))
        } else {
          // every subset gets its own interface
          updated = updated.addCombinator(new AddMultiOperationInterface(sub))

          addMulti(model, sub)
        }

        // do this in all cases...
        addFinal(model, sub)
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
    def registerImpl(dm:DomainModel, op: Operation, fm: FunctionMethod): Unit = {
      dm.data.asScala
        .foreach(exp => {
          val comb: Seq[Statement] = new CodeGenerators(model).evalGenerators(exp).get

          updated = updated
            .addCombinator(new AddDefaultImpl(op, fm, exp, comb))
        })

      if (dm.getParent.isPresent) {
        registerImpl(dm.getParent.get, op, fm)
      }
    }

    def registerExtension(dm:DomainModel, op: Operation, codegen: CodeGeneratorRegistry[Seq[Statement]]): Unit = {
      dm.data.asScala
        .foreach(exp => {
          val comb: Seq[Statement] = codegen(exp).get

          updated = updated
            .addCombinator(new AddExpOperation(exp, op, comb))
        })

      if (dm.getParent.isPresent) {
        registerExtension (dm.getParent.get, op, codegen)
      }
    }

    // note default 'Eval' operation is handled specially since it is assumed to always exist in top Exp class
    registerImpl(model, new Eval, new FunctionMethod("eval", Types.Double))

    // extension
    registerExtension(model, new PrettyP, new CodeGenerators(model).prettypGenerators)

    registerExtension(model, new Collect, new CodeGenerators(model).collectLitGenerators)

    // simplify.....BEGIN
    // Note: ACCEPT returns Double, which means we can't use == as comparison against two variables, but
    // can use with constants (i.e., == 0)
    // checking if x == -y is good, since it properly unboxes/boxes

//    def registerExtensionHardCoded (op:Operation, map:Map[Exp,Seq[Statement]]): Unit = {
//      map.keys.foreach {
//        key =>
//          updated = updated
//            .addCombinator(new AddExpOperation(key, op, map(key)))
//      }
//    }
//
//    // note default 'Eval' operation is handled specially since it is assumed to always exist in top Exp class
//    registerExtensionHardCoded(new SimplifyExpr,  Map(
//      new Lit -> Java(s"""return e;""").statements(),   // nothing to simplify.
//      new Add -> Java(s"""
//                         |double leftVal = e.getLeft().accept(new Eval());
//                         |double rightVal = e.getRight().accept(new Eval());
//                         |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
//                         |  return new Lit(0.0);
//                         |} else if (leftVal == 0) {
//                         |  return e.getRight().accept(this);
//                         |} else if (rightVal == 0) {
//                         |  return e.getLeft().accept(this);
//                         |} else {
//                         |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
//                         |}
//                         |""".stripMargin).statements(),
//      new Sub -> Java(s"""
//                         |if (left().eval() == right().eval()) {
//                         |  return new Lit(0.0);
//                         |} else {
//                         |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
//                         |}
//                         |""".stripMargin).statements(),
//      new Neg -> Java(s"""
//                         |if (this.eval() == 0) {
//                         |   return new LitFinal(0.0);    // HELP. MUST BE ADJUSTED
//                         |} else {
//                         |   return this;
//                         |}""".stripMargin).statements(),
//      new Mult -> Java(s"""
//                          |double leftVal = e.getLeft().accept(new Eval());
//                          |double rightVal = e.getRight().accept(new Eval());
//                          |if (leftVal == 0 || rightVal == 0) {
//                          |  return new Lit(0.0);
//                          |} else if (leftVal == 1) {
//                          |  return e.getRight();
//                          |} else if (rightVal == 1) {
//                          |  return e.getLeft();
//                          |} else {
//                          |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
//                          |}
//                          |""".stripMargin).statements(),
//      new Divd -> Java(s"""
//                          |double leftVal = e.getLeft().accept(new Eval());
//                          |double rightVal = e.getRight().accept(new Eval());
//                          |if (leftVal == 0) {
//                          |  return new Lit(0.0);
//                          |} else if (rightVal == 1) {
//                          |  return e.getLeft();
//                          |} else if (leftVal == rightVal) {
//                          |  return new Lit(1.0);
//                          |} else if (leftVal == -rightVal) {
//                          |  return new Lit(-1.0);
//                          |} else {
//                          |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
//                          |}
//                          |""".stripMargin).statements()
//    ))


    updated
  }


  /**
    * Construct class to represent subclass of Exp.
    *
    * interface Lit extends Exp {
    * int x();
    * default int eval() { return x(); }
    * }
    *
    * but also recursive types:
    *
    * interface Add extends Exp {
    * Exp e1(); Exp e2();
    * default int eval() {
    * return e1().eval() + e2().eval();   $IMPLEMENT[
    * }
    * }
    *
    * addImpl(new Eval, new Add, Java(s"""return e1().eval() + e2().eval();""").statements())
    * addImpl(new Eval, new Lit, Java(s"""return x();""").statements())
    *
    * @param sub Exp subclass whose interface we are generating
    */
  class SubInterface(sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java(
        s"""
           |package ep;
           |public interface $name extends Exp {}
            """.stripMargin).compilationUnit()

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields: Seq[MethodDeclaration] = Java(s"""$tpe ${att.attName}();""").methodDeclarations()

          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _: FunctionMethod =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, sub)
  }



  /**
    * i.e., LitFinal
    *
    * public class LitFinal implements Lit {
    * Integer value;
    * public LitFinal(int value) { this.value = value; }
    * public Integer value() { return value; }
    * }
    *
    * @param sub type (i.e., "Lit") for which *Final class is to be synthesized.
    */
  class FinalClass(sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java(
        s"""
           |package ep;
           |public class ${name}Final implements $name {}
       """.stripMargin).compilationUnit()

      var params: Seq[String] = Seq.empty
      var cons: Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields = Java(
            s"""
               |private $tpe ${att.attName};
               |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$tpe ${att.attName}"
          cons = cons :+ s"  this.${att.attName} = ${att.attName};"

          val methods = Java(
            s"""
               |public $tpe ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _: FunctionMethod =>
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

    val semanticType: Type = ep(ep.finalType, sub)
  }

  /**
    * i.e., LitFinal
    *
    *
    **
    *class AddCFinal implements AddC {
    * ExpC e1, e2;
    * AddCFinal(ExpC e1, ExpC e2) {
    *this.e1 = e1;
    *this.e2 = e2;
    * }
    * public ExpC e1() { return e1; }
    * public ExpC e2() { return e2; }
    * }
    *
    * FinalMultiClass(new Add, List(new Collect))
    *
    * @param sub Type (i.e., Add) for which a final class is to be constructed...
    * @param ops ...based on a List[Operation] that specifies desired capabilities
    */
  class FinalMultiClass(ops: List[Operation], sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName
      val combined = ops.map(_.getClass.getSimpleName).mkString("")

      val unit = Java(s"""|package ep;
                          |public class $name${combined}Final implements $name$combined {}
                          """.stripMargin).compilationUnit()

      var params: Seq[String] = Seq.empty
      var cons: Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>

          // override as needed to deal with co-variant specializations
          var revisedTypeName = Type_toString(att.attType)
          if (att.attType == Types.Exp) {
            revisedTypeName = combined
          }
          val fields: Seq[FieldDeclaration] = Java(s"""
                                                 |private $revisedTypeName ${att.attName};
                                                 |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$revisedTypeName ${att.attName}"
          cons = cons :+ s"  this.${att.attName} = ${att.attName};"

          val methods: Seq[MethodDeclaration] = Java(
            s"""
               |public $revisedTypeName ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }


        case _: FunctionMethod =>
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

    val semanticType: Type = ep(ep.finalType, sub, ops)
  }

  /**
    * Given an interface for a type, adds a default implementation of given operation
    *
    * @param op    Desired Operation
    * @param fm    Domain Model Function Method that models this operation
    * @param sub   The subType associated with....
    * @param stmts ...the statements containing an implementation of Operation for SubType.
    */
  class AddDefaultImpl(op: Operation, fm: FunctionMethod, sub: Exp, stmts: Seq[Statement]) {
    def apply(unit: CompilationUnit): CompilationUnit = {

      val tpe = Type_toString(fm.returnType)
      val name = fm.name

      // methods are marked as default later
      val methods = Java(s"$tpe $name() { ${stmts.mkString} }").methodDeclarations()

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m)
      }
      unit
    }

    val semanticType: Type = ep(ep.interface, sub) =>: ep(ep.defaultMethods, sub, op)
  }


  /**
    * Given an extension to Exp and a given operation (and its stmts implementation) produce an
    * interface with default method. Overide methods that are of class Exp. Thus: AddExpOperation (Add, PrettyP, ...)
    *
    * interface AddPrettyP extends Add, PrettyP {
    * PrettyP left();
    * PrettyP right();
    * default String print() {
    * return "(" + left().print() + " + " + right().print() + ")";
    * }
    * }
    *
    * @param exp   SubType (i.e., Add) for which an operation is to be defined.
    * @param op    Operation to be defined.
    * @param stmts Default set of statements for implementation
    */
  class AddExpOperation(exp: Exp, op: Operation, stmts: Seq[Statement]) {
    def apply(): CompilationUnit = {
      val opName = op.getClass.getSimpleName
      val expName = exp.getClass.getSimpleName

      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $expName$opName extends $expName, $opName { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      // methods are marked as default later
      val methods: Seq[MethodDeclaration] = Java(
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

        case _: FunctionMethod =>
      }

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m)
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, exp, op)
  }

  //  interface ExpPC extends ExpP, ExpC{}
  //  interface LitPC extends ExpPC, LitP, LitC{}
  //  interface AddPC extends ExpPC, AddP, AddC {
  //    ExpPC e1(); ExpPC e2();
  //  }
  class AddMultiOperation(ops: List[Operation], exp: Exp) {
    def apply(): CompilationUnit = {
      val name = exp.getClass.getSimpleName

      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val commas: String = ops.map(name + _.getClass.getSimpleName).mkString(",")

      val unit: CompilationUnit = Java(
        s"""|package ep;
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

        case _: FunctionMethod =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, exp, ops)
  }

  // interface ExpP extends Exp { String print(); }
  class AddOperation(op: Operation) {
    def apply(): CompilationUnit = {
      val name = op.getClass.getSimpleName

      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $name extends Exp { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      val methods: Seq[MethodDeclaration] = Java(s"""$tpe ${op.name}();""").methodDeclarations()

      methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

      unit
    }

    val semanticType: Type = ep(ep.interface, op)
  }


  // interface ExpP extends Exp { String print(); }
  class AddMultiOperationInterface(ops: List[Operation]) {
    def apply(): CompilationUnit = {
      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val names: String = ops.map(_.getClass.getSimpleName).mkString(",")


      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $combined extends $names { }
           |""".stripMargin).compilationUnit()

      unit
    }

    val semanticType: Type = ep(ep.interface, ops)
  }


  /** Generate from domain. */
  @combinator object BaseExpInterface {

    // no longer does the default Exp have an add Operation
    val exp: Exp = new Exp

    def apply(): CompilationUnit = {
      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface Exp { }
           |""".stripMargin).compilationUnit()

      // overkill but should work
      // create a sequence with just the Eval operator
      val evalOnly: Seq[FunctionMethod] = Seq.empty :+ new FunctionMethod("eval", Types.Double)

      evalOnly.foreach {
        case func: FunctionMethod =>
          val tpe = Type_toString(func.returnType)

          val methods: Seq[MethodDeclaration] = Java(s"""$tpe ${func.name}();""").methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _ =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, exp)
  }
  //
  //  // sample Driver
  //  @combinator object Driver {
  //    def apply:CompilationUnit = Java(s"""
  //         |package ep;
  //         |
  //         |public class Driver {
  //         |  public static void main(String[] args) {
  //         |    System.out.println("======Add======");
  //         |    Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
  //         |    System.out.println(add.eval());
  //         |    System.out.println("======Sub======");
  //         |    Sub sub = new SubFinal(new LitFinal(7), new LitFinal(4));
  //         |    System.out.println(sub.eval());
  //         |    System.out.println("======Print======");
  //         |
  //         |    /* the line below causes compile-time error, if now commented out. */
  //         |    //AddPrettyPFinal exp = new AddPrettyPFinal(new LitFinal(7)), new LitFinal(4));
  //         |    AddPrettyPFinal prt = new AddPrettyPFinal(new LitPrettyPFinal(7), new LitPrettyPFinal(4));
  //         |    System.out.println(prt.print() + " = " + prt.eval());

  // Once you know the types and the operations. then use one final factory for constructors.
  // suppose you wanted parser. you can get this to work ( at some cost to yourself) and it breaks down as
  // soon as you start applying new operations since the instantiation code contains older objects that
  // can't use those new operations

  //         |    System.out.println("======CollectLiterals======");
  //         |    AddCollectFinal addc = new AddCollectFinal(new LitCollectFinal(3), new LitCollectFinal(4));
  //         |    System.out.println(addc.collectList().toString());

  //         |    System.out.println("======Composition: Independent Extensibility======");
  //         |    AddPrettyPCollectFinal addpc = new AddPrettyPCollectFinal(new LitPrettyPCollectFinal(3), new LitPrettyPCollectFinal(4));
  //         |    System.out.println(addpc.print() + " = " + addpc.eval() + " Literals: " + addpc.collectList().toString());
  //         |  }
  //         |}""".stripMargin).compilationUnit()
  //
  //    val semanticType:Type = driver
  //  }

}