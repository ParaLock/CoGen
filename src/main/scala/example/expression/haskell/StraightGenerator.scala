package example.expression.haskell    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
// https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/

trait StraightGenerator extends AbstractGenerator {
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(new HaskellType("Expr"))
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
      flat.ops.map(op => generateOp(flat, op)) :+
      generateDataTypes(flat)
  }

  /** Construct args list "a1 a2 a3 ..." */
//  def standardArgs(exp:Atomic) : String = {
//    val vals:Range = 1 to exp.attributes.size
//    vals.map(v => s"a$v").mkString (" ")
//  }

  def generateOp(m:Model, op:Operation) : HaskellWithPath = {
    val name = op.name
    val opRetType = typeConverter(op.returnType.get)
    val definition = Haskell(s"$name :: Expr -> $opRetType")

    val instances = m.types.map(exp => {
      s"""$name (${exp.name.capitalize} ${standardArgs(exp).getCode}) = ${logic(exp)(op).mkString("\n")}"""
    })

    val code = Haskell(s"""|module ${name.capitalize} where
                           |import DataTypes
                           |$definition
                           |${instances.mkString("\n")}""".stripMargin)
    HaskellWithPath(code, Paths.get(s"${name.capitalize}.hs"))
  }


  def generateDataTypes(m:Model): HaskellWithPath = {
    val allTypes = m.types.map(exp => {
      val params:Seq[HaskellType] = exp.attributes.map(att => typeConverter(att.tpe))
      val list:String = params.map(f => f.toString).mkString(" ")
      Haskell(s"${exp.name.capitalize} $list")
    }).mkString("  | ")
    val code = Haskell(
      s"""|module DataTypes where
          |
          |-- All types are classified as data
          |data Expr = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

    /** Responsible for dispatching sub-expressions with possible parameter(s). */
    override def dispatch(op:domain.Operation, primary:Haskell, params:Haskell*) : Haskell = {
      val args:String = params.mkString(" ")

      Haskell(s"""${op.name} ${primary.toString} $args""")
    }

  /**
    * Determines the Haskell expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Haskell] = {
    exp.attributes.map(att => att.name -> Haskell(s"${att.name}")).toMap
  }
}