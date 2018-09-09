package example.expression.cpp.visitor

/*DI:LD:AD*/

import example.expression.cpp._
import example.expression.domain.{BaseDomain, ModelDomain}

// visitor based solution
trait CPPVisitorGenerator extends AbstractGenerator with DataTypeSubclassGenerator with CPPBinaryMethod with StandardCPPBinaryMethod {

  val domain: BaseDomain with ModelDomain

  import domain._

  def getModel: domain.Model

  /**
    * Generating a visitor OO solution requires:
    * 1. A Class for every exp data type (with Header file)
    * 2. A Base class to be superclass of them all (Exp.h)
    * 3. A visitor base class (ExpVisitor.h)
    * 4. A visitor subclass for every operation
    */
  def generatedCode(): Seq[CPPFile] = {
    val flat = getModel.flatten()
    val clazzes:Seq[CPPFile] = getModel.inChronologicalOrder   // visitors are constructed in order
      .filter(m => m.ops.nonEmpty)
      .flatMap(m =>
        m.ops.map(op => operationGenerator(flat, op)))         // one class for each op

    flat.types.map(tpe => generateExp(flat, tpe)) ++
    flat.types.map(tpe => generateExpImpl(flat, tpe)) ++
      clazzes :+
      generateBaseClass(flat) :+
      defaultHeaderFile() :+
      generateBase(flat) // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,CPPElement] = {
    exp.attributes.map(att => att.name -> new CPPElement(s"${att.name}")).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:CPPElement, op:Operation, params:CPPElement*) : CPPElement = {
    val args:String = params.mkString(",")
    new CPPElement(s"""e->get${expr.toString.capitalize}($args)""")
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep, covariantReplacement:Option[CPPType] = None) : CPPType = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(new CPPType("Exp"))
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => new CPPType("void")
    }
  }

  /** Operations are implement ala visitor. */
  def methodGenerator(exp:Atomic)(op:Operation): CPPMethod = {
    val params = parameters(op)
    new CPPMethod("void", s"Visit${exp.name}", s"(const ${exp.name}* e)", logic(exp)(op).mkString("\n"))
  }

  /** Default header file needed for most classes. */
  def defaultHeaderFile() : CPPHeaderCode = {
    new CPPHeaderCode("visitor",
      s"""
         |#ifndef _VISITOR_H_
         |#define _VISITOR_H_
         |#include <iostream>
         |#include <map>
         |#include <memory>
         |#include <sstream>
         |#include <string>
         |#include <vector>
         |#endif /* _VISITOR_H_ */
       """.stripMargin.split("\n"))
  }

  // standard headers
  def standardHeader():Seq[String] = {
    s"""#include "visitor.h" """.stripMargin.split("\n")
  }

  /**
    * Brings in classes for each operation. These can only be completed with the implementations.
    *
    * Must handle BinaryMethod (Equals) and BinaryMethodBase (Astree) specially.
    */
  def operationGenerator(model:domain.Model, op:domain.Operation): CPPFile = {
    val signatures:Seq[CPPMethod] = model.types.map(exp => methodGenerator(exp)(op))
    val tpe:CPPType = typeConverter(op.returnType.get)
    val realType:String = op match {
      case po:ProducerOperation => "Exp *"
      case _ => tpe.stmt
    }
    // access value via lookup into value_map_
    // val _retType:String, val _name:String, val _params:String, val _body:Seq[String]
    val lookup = Seq(new CPPMethod(realType, "getValue", "(const Exp& e)", Seq("return value_map_[&e];")))

    val extras = dependency(op).map(o => s"""#include "${o.name.capitalize}.h" """)

    // binary methods?
    val binaryConstructor:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod =>
        Seq(new CPPElement (s"""
                               |${op.name.capitalize} (const Exp *t) {
                               |    that = t;
                               |}""".stripMargin))
      case _ => Seq.empty
    }

    // binary fields?
    val binaryField:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod => Seq(new CPPElement (s""" const Exp *that; """))
      case _ => Seq.empty
    }

    new CPPClass (op.name.capitalize, op.name.capitalize, lookup ++ binaryConstructor ++ signatures,
      Seq(new CPPElement(s"""std::map<const Exp*, $realType  > value_map_;""")) ++ binaryField)
      .setSuperclass("ExpVisitor")
      .addHeader(Seq("""#include "ExpVisitor.h" """, """#include "visitor.h" """))
      .addHeader(extras)
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExpImpl(model:Model, sub:Atomic) : CPPFile = {
    val signatures = sub.attributes
      .filter(att => att.tpe == domain.baseTypeRep)
      .map(att => new CPPElement(s"${att.name}_->Accept(visitor);")).mkString("\n")

    val binaryMethods:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      // sub
      val method:String = sub match {
        case _:Unary | _:Binary => {
          val atts = sub.attributes
            .filter(att => att.tpe == domain.baseTypeRep)
            .map(att => s"${att.name}_->astree()").mkString(",")

         s"""
           |Tree *${sub.name.capitalize}::astree() const {
           |    std::vector<Tree *> vec_${sub.name} = { $atts };
           |    return new Node(vec_${sub.name.capitalize}, DefinedSubtypes::${sub.name.capitalize}Subtype);
           |}""".stripMargin
        }
        case lit:Atomic => {
         s"""
           |Tree *${sub.name.capitalize}::astree() const {
           |    return new Leaf(getValue());    // hard-coded and could be replaced.
           |}""".stripMargin
        }
      }

      Seq(new CPPElement(method))
    } else {
      Seq.empty
    }

    val contents =
      s"""|
         |#include "visitor.h"
         |#include "Exp.h"
         |#include "ExpVisitor.h"
         |#include "${sub.name.capitalize}.h"
         |void ${sub.name.capitalize}::Accept(ExpVisitor* visitor) const {
         |  $signatures
         |  visitor->Visit${sub.name.capitalize}(this);
         |}
         |${binaryMethods.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.name.capitalize, contents)
  }

  /** Generate the full class for the given expression sub-type (except for impl). */
  def generateExp(model:Model, sub:Atomic) : CPPFile = {
    val name = sub.name

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.name.capitalize
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPElement(s"const $tpe* ${att.name}_;")

      // prepare for constructor
      params = params :+ s"const $tpe* ${att.name}"
      cons = cons :+ s"${att.name}_(${att.name})"

      // make the set/get methods
      addedMethods = addedMethods :+ new CPPElement(s"const $tpe* get$capAtt() const { return ${att.name}_; }")
    })

    // make constructor
    addedMethods = addedMethods :+ new CPPElement (s"${sub.name} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // Method declaration (not implementation)
    val visitor = new CPPElement("void Accept(ExpVisitor* visitor) const;")

    // add Binary methods if needed
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(new CPPElement (s"""Tree *${domain.AsTree.name.toLowerCase}() const; """))
    } else {
      Seq.empty
    }
    addedMethods = addedMethods :+ visitor
    addedMethods = addedMethods ++ astreeMethod

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """, """#include "ExpVisitor.h" """))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CPPFile = {

    // binary methods?
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(new CPPElement ("""virtual Tree *astree() const = 0;"""))
    } else {
      Seq.empty
    }

    val astreeHeaders:Seq[String] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(""" #include "Tree.h" """)
    } else {
      Seq.empty
    }

    new CPPClass("Exp", "Exp",
      Seq(new CPPElement(s"""virtual void Accept(ExpVisitor* visitor) const = 0;""")) ++ astreeMethod, Seq.empty)
      .addHeader(Seq(s"""#include "visitor.h" """, s"""class ExpVisitor;""") ++ astreeHeaders)
  }

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass(model:domain.Model):CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().types.map(exp =>
        new CPPElement(s"""virtual void Visit$exp(const $exp* e) = 0;"""))
    val allHeaders = getModel.flatten().types.map(exp => s"""#include "$exp.h" """)

    val moreImports = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(
        s"""
           |#include "Tree.h" // Binary Methods needs these include files
           |#include "Node.h"
           |#include "Leaf.h"
           |#include "DefinedSubtypes.h" """.stripMargin)
    } else {
      Seq.empty
    }

    new CPPClass("ExpVisitor", "ExpVisitor", allOps, Seq.empty)
      .addHeader(Seq(s"""#include "visitor.h" """) ++ allHeaders ++ moreImports)
  }

  def generateBinaryMethodHelpers():Seq[CPPFile] = {

    // If BinaryMethodTreeBase, need the declarations here.
    if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      declarations
    } else {
      Seq.empty
    }
  }

  // helper methods for C++

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1

      name
    }).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")
  }
}
