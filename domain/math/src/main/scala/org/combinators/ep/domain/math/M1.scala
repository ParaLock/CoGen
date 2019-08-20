package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

//class M1(val m0:M0) extends Evolution {
object M1 extends Evolution {
  //val m0 = Model("m0", Seq(Lit, Add), Seq(Eval))
  //val m1 = Model("m1", Seq(Sub), Seq.empty, last = m0.getModel)
  override implicit def getModel:Model = MathDomain.getModel.evolve("m1", Seq(Sub), Seq.empty)

  // m1:model evolution.
  // -------------------
  lazy val Sub:DataTypeCase = DataTypeCase.binary("Sub")

  def SubInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sub, Seq(InstanceRep(left), InstanceRep(right)))

  // testing
  def M1_tests: Seq[TestCase] = Seq(
    EqualsTestCase(SubInst(LitInst(1.0), LitInst(2.0)), Eval, InstanceRep(LitInst(-1.0))),
  )
}
