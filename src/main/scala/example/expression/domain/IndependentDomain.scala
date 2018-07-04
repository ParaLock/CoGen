package example.expression.domain

/**
  * Develop two new independent extensions to e1
  */
trait IndependentDomain extends Domain {

  object independent {
    val height:String = "height"
  }

  // i1 extension
  // ------------
  case object Inv extends subtypes.UnaryExp("Inv")
  val i1 = Model("i1", Seq(Inv), Seq.empty, last=e1)

  // i2 extension
  // ------------
  case object Integer extends Types
  case object Height extends Operation(independent.height, Some(Integer), (independent.height, Integer))
  val i2 = Model("i2", Seq.empty, Seq(Height), last=i1)
}
