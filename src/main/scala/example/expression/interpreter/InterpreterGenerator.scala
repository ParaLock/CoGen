package example.expression.interpreter

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{Expression, SimpleName}
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.CompilationUnit
import org.combinators.templating.twirl.Java

trait InterpreterGenerator extends example.expression.Straight.StraightGenerator {
  import domain._

  /**
    * Must eliminate any operation that returns E as value, since can't handle Producer methods
    */
  override def compatible(model:Model):Model = {
    if (model.isEmpty) { return model }

    // rebuild by filtering out all operations that return Exp.
    Model (model.name, model.types, model.ops.filterNot(op => op.returnType.isDefined && op.returnType.get.equals(types.Exp)), compatible(model.last))
  }

  override def subExpressions(exp: domain.expressions.Exp): Map[String, Expression] = {
    exp.attributes.map(att => att.name -> Java(s"get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Return designated Exp type with replacement. */
  def recursiveTypeGenerator(tpe:types.Types, replacement:Type) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => replacement
      case _ => typeGenerator(tpe)
    }
  }

  def modelInterfaceName(model:Model): String = {
    model.ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("") + "Exp"
  }

  /** Find Model with operations and return that one. */
  def baseInterfaceName(m:Model): SimpleName = {
      Java(m.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("") + "Exp").simpleName()
  }


//  def baseInterfaceName(op: Operation): SimpleName = {
//    Java(s"Exp${op.name.capitalize}").simpleName()
//  }

  /**
    * Must extend base EvalExp
    * Must have constructor and access
    * Must have operation methods
    *
    * @param domain
    * @param exp
    * @return
    */
  override def generateExp(domain:Model, exp:expressions.Exp) : CompilationUnit = {
    val name = Java(s"${exp.name}").simpleName()
    val baseInterface:Type = Java(baseInterfaceName(domain.lastModelWithOperation())).tpe()

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${recursiveTypeGenerator(att.tpe, baseInterface)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${recursiveTypeGenerator(att.tpe, baseInterface)} ${att.name}")

    val getters: Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"""|public ${recursiveTypeGenerator(att.tpe, baseInterface)} get${att.name.capitalize}() {
                                             |    return this.${att.name};
                                             |}""".stripMargin).methodDeclarations())
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    // provide method declarations for all past operations (including self). But if we extend, can't we stop at last op?
    val allOps:Seq[Operation] = domain.pastOperations()
    val operations:Seq[MethodDeclaration] = allOps.map(op =>
        methodGenerator(exp)(op))

    Java(s"""
            |package interpreter;
            |public class $name implements ${baseInterface.toString} {
            |
            |  ${constructor.toString}
            |
            |  ${getters.mkString("\n")}
            |  ${atts.mkString("\n")}
            |  ${operations.mkString("\n")}
            |}""".stripMargin).compilationUnit()
   }

  def interfaceName(exp: domain.expressions.Exp, op: Operation): SimpleName = {
    Java(s"${exp.name}${op.name.capitalize}").simpleName()
  }

//
//  override def methodGenerator(exp: domain.expressions.Exp)(op: domain.Operation): MethodDeclaration = {
//    val method = super.methodGenerator(exp)(op)
//    //method.setDefault(true)
//    method.setType(
//      op.returnType match {
//        case Some(tpe) => typeGenerator(tpe)
//        case _ => Java("void").tpe
//      })
//   // method.setModifier(Modifier.PUBLIC, false)
//    method
//  }

  def generateInterface(exp: domain.expressions.Exp, parents: Seq[SimpleName], op:Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp)(op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${typeGenerator(att.tpe)} get${att.name.capitalize}();").methodDeclarations())

    Java(s"""
            |package interpreter;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |
            |  ${method}
            |}""".stripMargin).compilationUnit()
  }

  def finalInterfaceName: SimpleName = Java("FinalI").simpleName()

  /**
    * Generate one interface for model that defines an operation. If two or more operations
    * are defined in the same model, then concatenate together.
    *
    * Shall only be called on a model with operations.
    *
    * @param model
    * @return
    */
  override def generateBase(model: Model): CompilationUnit = {
    // concatenate all names
    val fullType:Type = Java(modelInterfaceName(model)).tpe()
    val methods:Seq[String] = model.ops.map(op => {
      val params: Seq[String] = op.parameters.map(tuple => {
        val name = tuple._1
        val tpe = tuple._2
        s"${typeGenerator(tpe)} $name"
      } )

      s"""public ${recursiveTypeGenerator(op.returnType.get, fullType)}  ${op.name}(${params.mkString(",")});"""
    })

    // see if we are first.
    val lastWithOps = if (model.ops.isEmpty) { model.lastModelWithOperation() }
    else { model.last.lastModelWithOperation()}

    val extension:String = if (lastWithOps.isEmpty) ""
      else "extends " + modelInterfaceName(lastWithOps)

    val str = s"""|package interpreter;
                  |public interface ${fullType.toString} $extension {
                  |  ${methods.mkString("\n")}
                  |}""".stripMargin
    println(str)
    Java(str).compilationUnit()
  }

  def lastTypesSinceAnOperation(model:Model): Seq[expressions.Exp] = {
    if (model.isEmpty || model.ops.nonEmpty) {
      Seq.empty
    } else {
      model.types ++ lastTypesSinceAnOperation(model.last)
    }
  }

  /**
    * For all new types added since last operation, need to provide implementation.
    *
    * Note: will never be base, so pass in false
    * @param model
    * @return
    */
  def generateIntermediateTypes(model:Model): Seq[CompilationUnit] = {
    if (model.isEmpty) { return Seq.empty }
    if (model.ops.nonEmpty) { return generateIntermediateTypes(model.last) }   // continue onwards.

    // compute new types since last operation

    val last = model.lastModelWithOperation()   // HACK. true is not best ansewr
    //last.ops.flatMap(op => generateForOp(model, op, lastTypesSinceAnOperation(model), isBase=true)) ++ generateIntermediateTypes(model.last)
    generateForOp(model, last.ops, lastTypesSinceAnOperation(model), isBase=true) ++ generateIntermediateTypes(model.last)
  }

  // if multiple operations in the same model, then must chain together.
  def generateBaseExtensions(model:Model) : Seq[CompilationUnit] = {
    val pastTypes:Seq[expressions.Exp] = model.pastDataTypes()

    val isBase:Boolean = model.base().equals(model)

//    var last_op:Option[Operation] = None
//    model.ops.flatMap(op => {
//      val result:Seq[CompilationUnit] = generateForOp(model, op, pastTypes, isBase, last_op)
//      last_op = Some(op)
//      result
//    })
    generateForOp(model, model.ops, pastTypes, isBase)
  }

  def generateForOp(model:Model, ops:Seq[Operation], pastTypes:Seq[expressions.Exp], isBase:Boolean) : Seq[CompilationUnit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")

      pastTypes.map(exp => {
        val name = Java(s"${exp.name}").simpleName()
        val baseInterface:Type = Java(baseInterfaceName(model.lastModelWithOperation())).tpe()

        val atts:Seq[FieldDeclaration] = if (isBase) {
          exp.attributes.flatMap(att => Java(s"${recursiveTypeGenerator(att.tpe, baseInterface)} ${att.name};").fieldDeclarations())
        } else {
          Seq.empty
        }

        val params:Seq[String] = exp.attributes.map(att => s"${recursiveTypeGenerator(att.tpe, baseInterface)} ${att.name}")
        val paramNames:Seq[String] = exp.attributes.map(att => s"${att.name}")


        val getters: Seq[MethodDeclaration] =
          exp.attributes.flatMap(att => {
            // anything that is an EXPR can be replaced
            val cast = att.tpe match {
              case types.Exp => if (!isBase) { s"(${baseInterface.toString})" } else { "" }
              case _ => ""
            }

            Java(s"""|public ${recursiveTypeGenerator(att.tpe, baseInterface)} get${att.name.capitalize}() {
                     |    return $cast this.${att.name};
                     |}""".stripMargin).methodDeclarations()
          })

        val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

        val constructor = if (isBase) {
          Java(s"""|public $combinedOps$name (${params.mkString(",")}) {
                   |   ${cons.mkString("\n")}
                   |}""".stripMargin).constructors().head
        } else {
          Java(s"""|public $combinedOps$name (${params.mkString(",")}) {
                   |   super(${paramNames.mkString(",")});
                   |}""".stripMargin).constructors().head
        }

        // provide method declarations for all past operations (including self) that are not already in our 'ops' set.
        val allOps:Seq[Operation] = model.pastOperations().filterNot(op => ops.contains(op))

        // be sure to recursively change Exp to be fullType
        val fullType:Type = Java(modelInterfaceName(model)).tpe()

        val operations:Seq[MethodDeclaration] = (allOps ++ ops).map(op => {
          val md:MethodDeclaration = methodGenerator(exp)(op)

          // be sure to recursively change any Exp into fullType, for producer capability
          val returnType:Type = md.getType
          if (returnType.equals(typeGenerator(types.Exp))) {
            md.setType(fullType)
          }

          md
        })

        // FIXME
        // For this operation (i.e., "Print") and for this Exp (i.e., "Add") go back and find most recent class to extend (i.e., "EvalAdd")
        // which is based on either (a) last operation
        // when multiple operations defined in same model, then chain together arbitrarily
        val extension:String =
          if (isBase) {
            ""
          } else {
            val past: String = model.last.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
            s"extends $past$name" // go backwards?
          }

        Java(s"""
                |package interpreter;
                |public class $combinedOps$name $extension implements ${baseInterface.toString} {
                |
                |  ${constructor.toString}
                |
                |  ${getters.mkString("\n")}
                |  ${atts.mkString("\n")}
                |  ${operations.mkString("\n")}
                |}""".stripMargin).compilationUnit()
      })
  }

//  def generateBaseInterface(model:Model, op: Operation, parents: Seq[SimpleName]): CompilationUnit = {
//
//    val retType = op.returnType match {
//      case Some(tpe) => typeGenerator(tpe)
//      case _ => Java("void").tpe
//    }
//
//    val params:String = op.parameters.map(tuple => {
//      val name:String = tuple._1
//      val tpe:types.Types = tuple._2
//
//      typeGenerator(tpe).toString + " " + name
//    }).mkString(",")
//
//
//    val methodSignature: MethodDeclaration =
//      Java(s"""public $retType ${op.name}($params);""").methodDeclarations().head
//
//    Java(
//      s"""package interpreter;
//         |
//         |public interface ${baseInterfaceName(model)} extends ${parents.mkString(", ")} {
//         |    $methodSignature
//         |}
//       """.stripMargin).compilationUnit()
//  }

}


