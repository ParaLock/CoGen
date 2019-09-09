package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsCompositeTestCase, EqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.M3.{Mult,MultInst}
import org.combinators.ep.domain.math.M4.{Simplify}
import org.combinators.ep.domain.math.M7.{Sqrt, SqrtInst}
import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.tree.{Leaf, Node}

object M8 extends Evolution {
  override implicit def getModel:Model = M7.getModel.evolve("m8", Seq(Power), Seq(Copy))

  // SquareRoot of inner value, and an operation Find that counts the number
  lazy val Power = DataTypeCase.binary("Power")
  lazy val Copy = Operation("copy", TypeRep.DataType(MathDomain.getModel.baseDataType))

  def PowerInst(base:DataTypeInstance, exp:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(InstanceRep(base), InstanceRep(exp)))

  val m8_1 = PowerInst(LitInst(6.0), LitInst(2.0))
  val m8_2 = PowerInst(LitInst(25.0), LitInst(-0.5))
  val m8_3 = PowerInst(LitInst(10.0), LitInst(0.0))

  val m8_tree = MultInst(LitInst(2.0), SqrtInst(LitInst(7.0)))
  val m8_tree1 = new Node(Mult.name.hashCode, Seq(new Leaf(2.0), new Node(Sqrt.name.hashCode, Seq(new Leaf(7.0)))))

  def M8_tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m8_1, Eval, InstanceRep(LitInst(36.0))),
    EqualsTestCase(getModel.baseDataType, m8_2, PrettyP, StringInst("Power(25.0,-0.5)")),
    EqualsTestCase(getModel.baseDataType, m8_3, Eval, InstanceRep(LitInst(1.0))),

    EqualsCompositeTestCase(getModel.baseDataType, m8_3, StringInst("1.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m8_tree, InstanceRep(TypeRep.Tree)(m8_tree1), (Copy, Seq.empty), (Operation.asTree, Seq.empty)),

  )
}
