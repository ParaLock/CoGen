package example.expression.domain

trait I2 extends Evolution {
  self: I1 =>
  val domain: MathDomain

  // i2:model evolution.
  // -------------------
  case object Integer extends domain.TypeRep
  case object Height extends domain.Operation(independent.height, Some(Integer), Seq((independent.height, Integer)))
  val i2 = domain.Model("i2", Seq.empty, Seq(Height), last = i1)

  override def getModel = i2

}
