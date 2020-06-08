package org.combinators.ep.domain    /*DI:LI:AI*/

import abstractions._

/**
 * M0 <- M1 <- M2
 *       |        \
 *       |        M3 <- M4
 *       |        /
 *       I1 <- I2
 *
 *  Will also have to ensure merging is only allowed when there is a common ancestor. It is theoretically
 *  possible to consider merging truly independent evolutions, but we start with this more common case first.
 *
 *  MA <- MB
 */
class GenericModel(val name:String,
                   val typeCases:Seq[DataTypeCase],
                   val ops:Seq[Operation],
                   val former:Seq[GenericModel],
                   val baseDataType: DataType) {

  def extend(str:String, past: Seq[GenericModel]): GenericModel = {
    new GenericModel(str, Seq.empty, Seq.empty, Seq(this) ++ past, baseDataType)
  }

  /** Straight merge of two Generic models into one, combining all typeCases, ops and formers.  */
  def merge(name:String,  typeCases:Seq[DataTypeCase], ops:Seq[Operation], others:Seq[GenericModel]) : GenericModel = {
    new GenericModel(name, typeCases, ops, others, baseDataType)
  }

  // only the BASE has this set
  def isDomainBase:Boolean = false

  // if all our formers are domainbase then we are the bottom
  def isBottom:Boolean = {
    former.map(gm => gm.isDomainBase).forall(_ == true)
  }

  /** Adds an evolution to this model.
   *
   * Note that the DOMAIN at the bottom will be MathDomain or ShapeDomain, and that needs to be present.
   *
   * @param name  The unique name of the next evolution.
   * @param types The new data types.
   * @param ops   The new operations.
   */
  def evolve(name: String, types: Seq[DataTypeCase], ops: Seq[Operation]): GenericModel = {
    new GenericModel(name, types, ops, Seq(this), baseDataType)
  }

  /** Returns history of this model as a sequence (Removing the MathDomain or ShapeDomain). */
  def toSeq: Seq[GenericModel] = {
    (this +: former.flatMap(_.toSeq)).filterNot(_.isDomainBase)
  }

  /** Returns topological ordering which is chronological when linear. */
  def inChronologicalOrder: Seq[GenericModel] = { //toSeq.reverse.filterNot(_.isDomainBase)

    def helpLinearize(model: GenericModel, current:Int,
                      states:scala.collection.mutable.Map[Int,Seq[GenericModel]]): Unit = {

      // more to process?
      if (!model.isDomainBase) {
        model.former.foreach(past => {
          helpLinearize(past, current + 1, states)
        })

        if (!states.contains(current)) {
          states.put(current, Seq(model))
        } else {
          states.put(current, states(current) :+ model)
        }
      }
    }

    val record = scala.collection.mutable.Map[Int,Seq[GenericModel]]()
    helpLinearize(this, 0, record)

    // now we have Map that records furthest DISTANCE from M0 to latest evolution. We now
    // have to read this backwards, and merge pairwise as we go, IGNORING THOSE WHICH HAVE
    // ALREADY BEEN MERGED...
    var order:Seq[GenericModel] = Seq.empty
    var alreadySeen:Seq[String] = Seq.empty
    for (i <- record.size-1 to 0 by -1) {
      val newer = record(i).filterNot(m => alreadySeen.contains(m.toString))
      newer.foreach(gm => {
        order = Seq(gm) ++ order
        alreadySeen = alreadySeen :+ gm.toString
      })
    }
    order.reverse
  }

  /** Guard check for equals method. */
  private def canEqual(a: Any): Boolean = a.isInstanceOf[GenericModel]

  /** Checks two models for equality.
   * Models are uniquely identified by their name.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: GenericModel => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  /** Computes the hash code of this model from the hash code of its name. */
  override def hashCode: Int = {
    name.hashCode
  }

  // Eliminate all past
  def standAlone:Model = {
    new Model(name, typeCases, ops, baseDataType)   // good enough?
  }

  /** Returns a flattened model where all previous evolutions are squashed into a single evolution on top of the
   * base model.
   *
   * The name of the squashed evolution will be the name of this evolution.
   * If no evolutions are present, the model is returned unchanged.
   */
  def flatten: Model = {
    if (isBottom) {
      this.standAlone
    } else {
      val history = toSeq.reverse
      val (baseModel, evolutions) = (history.head.standAlone, history.tail.map(_.standAlone))

      def squash(intoModel: Model, nextModel: Model): Model =
        new Model(name, intoModel.typeCases ++ nextModel.typeCases, intoModel.ops ++ nextModel.ops, baseDataType, None)

      if (evolutions.nonEmpty) {
        val reduced = (evolutions :+ baseModel).reduceLeft(squash)

        reduced
      } else {
        baseModel
      }
    }
  }

  /** Finds a present or past evolution that defines the given data type case. */
  def findTypeCase(tpe: DataTypeCase): Option[GenericModel] =
    toSeq.find(_.typeCases.contains(tpe))

  /** Finds a present or past evolution that defines the given operation. */
  def findOperation(op: Operation): Option[GenericModel] =
    toSeq.find(_.ops.contains(op))

  def supports(tpe: DataTypeCase): Boolean =
    findTypeCase(tpe).nonEmpty

  /** Determines if operation is supported by this model or any of its antecedents. */
  def supports(op: Operation): Boolean =
    findOperation(op).nonEmpty

  /** Find the most recent Model with an operation. */
  def lastModelWithOperation: Seq[GenericModel] = {
    if (ops.nonEmpty) {
      Seq(this)
    } else {
      former.flatMap(_.lastModelWithOperation)
    }
  }

  /** Finds the most recent Model with a data type case. */
  def lastModelWithDataTypes: Seq[GenericModel] = {
    if (typeCases.nonEmpty) {
      Seq(this)
    } else {
      former.flatMap(_.lastModelWithDataTypes)
    }
  }

  /** Finds all past data type cases, starting with the most recent ones. */
  def pastDataTypes: Seq[DataTypeCase] =
    toSeq.flatMap(_.typeCases)

  /** Finds all past operations, starting with the most recent ones. */
  def pastOperations: Seq[Operation] =
    toSeq.flatMap(_.ops)

  /** Returns the bottom-most model in the sequence. */
  def base: GenericModel = {
    if (former.isEmpty) {
      this
    } else {
      former.head.base
    }
  }

  /** Returns if this evolution has no data type cases or operations. */
  def isEmpty: Boolean = typeCases.isEmpty && ops.isEmpty

  /** Constructs new linear extension graph consistent with these two models.
   *
   * M0 <- M1 <- M2
   *       |        \
   *       |        M3 <- M4
   *       |        /
   *       I1 <- I2
   *
   * linearize should result in (M0 <- M1 <- (I1,M2) <- (I2) <- M3 <- M4
   * */
  def linearize : Model = {

    def helpLinearize(model: GenericModel, current:Int,
                      states:scala.collection.mutable.Map[Int,Seq[GenericModel]]): Unit = {

      // more to process?
      if (!model.isDomainBase) {
        model.former.foreach(past => {
          helpLinearize(past, current + 1, states)
        })

        if (!states.contains(current)) {
          states.put(current, Seq(model))
        } else {
          states.put(current, states(current) :+ model)
        }
      }
    }

    val record = scala.collection.mutable.Map[Int,Seq[GenericModel]]()
    helpLinearize(this, 0, record)

    // now we have Map that records furthest DISTANCE from M0 to latest evolution. We now
    // have to read this backwards, and merge pairwise as we go, IGNORING THOSE WHICH HAVE
    // ALREADY BEEN MERGED...
    var prevModel:Option[Model] = None
    var alreadySeen:Seq[String] = Seq.empty
    for (i <- record.size-1 to 0 by -1) {
      val newer = record(i).filterNot(m => alreadySeen.contains(m.toString))
      val combined = newer.reduce( (m1: GenericModel, m2: GenericModel) => m1.standAlone.merge(m1.name + m2.name, m2.standAlone) )
      record(i).foreach(gm => alreadySeen = alreadySeen :+ gm.toString)
      prevModel = Some(new Model(combined.name, combined.typeCases, combined.ops, combined.baseDataType, prevModel))
    }
    prevModel.get
  }

  /**
   * Determines if this model or its history contain any binary methods.
   *
   * Typical usage is to call getModel.flatten before calling this method.
   */
  def hasBinaryMethod: Boolean =
    toSeq.exists(_.ops.exists(_.isBinary(this)))

  /**
   * Determine if this model or its history contain any producer operations.
   *
   * Typical usage is to call getModel.flatten before calling this method.
   */
  def hasProducerOperation: Boolean =
    toSeq.exists(_.ops.exists(_.isProducer(this)))

  /**
   * Determines if this model comes before the given model in the evolution history.
   *
   * Note that if models are the same then return false.
   */
  def before(other:GenericModel): Boolean =
    this != other && other.toSeq.contains(this)

  /**
   * Return the earlier model given the evolution history.
   * Note that if models are the same, then just return the same one.
   */
  def earlier(other:GenericModel):GenericModel = {
    if (before(other)) {
      this
    } else {
      other
    }
  }

  /**
   * Return the earlier model given the evolution history.
   * Note that if models are the same, then just return the same one.
   */
  def later(other:GenericModel):GenericModel = {
    if (before(other)) {
      other
    } else {
      this
    }
  }

  override def toString:String = {
    "GenericModel " + name + "[" + typeCases.map(_.name).mkString(",")+ "," + ops.map(_.name).mkString(",") +
      " former:" + former.map(_.name).mkString(",") + "]"
  }

  /** Debugging function. */
  def output = {
    println(toString)
  }
}

// Ignore IntelliJ ScalaDoc error: https://youtrack.jetbrains.com/issue/SCL-14638
/** Models a named domain evolution with new data type cases and operations, as well as the last evolution.
  *
  * Use `evolve` to obtain the next model and [[org.combinators.ep.domain.GenericModel.base]] for the initial one.
  *
  * This will be renamed LinearModel and a superclass is non-linear model. In nonlinear model, could
  * have a linearize() method that "zips" up to create a linear model.
  *
  * @note Names act as unique identifiers within the context of one domain.
  */
sealed class Model (
   name:String,
   typeCases:Seq[DataTypeCase],
   ops:Seq[Operation],
   bdt: DataType,
   val last:Option[Model] = None) extends GenericModel (name, typeCases, ops, last.toSeq, bdt) {

  /** Adds an evolution to this model.
   *
   * @param name  The unique name of the next evolution.
   * @param types The new data types.
   * @param ops   The new operations.
   */
  override def evolve(name: String, types: Seq[DataTypeCase], ops: Seq[Operation]): Model =
    new Model(name, types, ops, baseDataType, Some(this))

  /** From linear Model all will be Model. */
  override def toSeq: Seq[Model] = {
    this +: last.map(_.toSeq).getOrElse(Seq.empty)
   // (this +: last.toSeq.flatMap(_.toSeq)).distinct   +: last.map(_.toSeq).getOrElse(Seq.empty)
  }

  // if all our formers are domainbase then we are the bottom
  override def isBottom:Boolean = {
    if (last.isDefined) {
      last.get.isDomainBase
    } else {
      true  // arbitrary choice. Not sure what else to do since should never happen
    }
  }

  /** Returns topological ordering which is chronological when linear. */
  override def inChronologicalOrder: Seq[Model] = toSeq.reverse     // necessary anymore? .tail

  /** Find the most recent Model with an operation. */
  override def lastModelWithOperation: Seq[Model] = {
    if (ops.nonEmpty) {
      Seq(this)
    } else {
      last.toSeq.flatMap(_.lastModelWithOperation)
    }
  }

  /** Finds the most recent Model with a data type case. */
  override def lastModelWithDataTypes: Seq[Model] = {
    if (typeCases.nonEmpty) {
      Seq(this)
    } else {
      last.toSeq.flatMap(_.lastModelWithDataTypes)
    }
  }

  /** Finds a present or past evolution that defines the given data type case. */
  override def findTypeCase(tpe: DataTypeCase): Option[Model] =
    toSeq.find(_.typeCases.contains(tpe))

  /** Finds a present or past evolution that defines the given operation. */
  override def findOperation(op: Operation): Option[Model] =
    toSeq.find(_.ops.contains(op))


  //  /** Returns a flattened model where all previous evolutions are squashed into a single evolution on top of the
//    * base model.
//    *
//    * The name of the squashed evolution will be the name of this evolution.
//    * If no evolutions are present, the model is returned unchanged.
//    */
//  override def flatten: Model = {
//    val history = toSeq.reverse
//    val (baseModel, evolutions) = (history.head, history.tail)
//
//    def squash(intoModel: Model, nextModel: Model): Model =
//      new Model(name, intoModel.typeCases ++ nextModel.typeCases, intoModel.ops ++ nextModel.ops, Some(baseModel))
//
//    if (evolutions.nonEmpty) evolutions.reduceLeft(squash)
//    else baseModel
//  }

  /** Constructs new linear extension graph consistent with these two models.
    *
    * Example:
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e2, [E2], [op2, op3]) ]
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e3, [E3], [op4, op5]), (e4, [E4], [op6, op7]) ]
    * are merged to
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e2:e3, [E2, E3], [op2, op3, op4, op5]), (e4, [E4], [op6, op7]) ]
    */
  def merge(name:String, other:Model) : Model = {
    def combineEvolutions(lastEvolution: Option[Model], evolutions: (Option[Model], Option[Model])): Option[Model] =
      evolutions match {
        case (Some(e1), Some(e2)) =>
          Some(
            new Model(
              Seq(e1.name, e2.name).distinct.mkString(":"),
              (e1.typeCases ++ e2.typeCases).distinct,
              (e1.ops ++ e2.ops).distinct,
              baseDataType,
              lastEvolution
            ))
        case (None, Some(e)) => Some(new Model(e.name, e.typeCases, e.ops, baseDataType, lastEvolution))
        case (Some(e), None) => Some(new Model(e.name, e.typeCases, e.ops, baseDataType, lastEvolution))
        case _ => lastEvolution
      }
    println("merge " + name + " with " + other.name)
    val extendedHistory: Seq[(Option[Model], Option[Model])] =
      toSeq.reverse.map(Some(_)).zipAll(other.toSeq.reverse.map(Some(_)), None, None)
    extendedHistory.foldLeft[Option[Model]](None)(combineEvolutions).get
  }

  /**
   * Return the earlier model given the evolution history.
   * Note that if models are the same, then just return the same one.
   */
  def earlier(other:Model):Model = {
    if (before(other)) {
      this
    } else {
      other
    }
  }

  /**
   * Return the earlier model given the evolution history.
   * Note that if models are the same, then just return the same one.
   */
  def later(other:Model):Model = {
    if (before(other)) {
      other
    } else {
      this
    }
  }

  /** Debugging function. */
  override def output = {
    println("Model " + name + "[" + typeCases.map(_.name).mkString(",")+ "," + ops.map(_.name).mkString(",") +
      " last:" + last.getOrElse("") + "]")
  }
}

object GenericModel {
  /** Provides a base domain model to start evolving from.
    *
    * @param domainName The name of the domain which is being modeled.
    * @param baseTypeName The name of the data type that is being modeled.
    */
  def base(domainName: String, baseTypeName: String): GenericModel =
    new GenericModel(domainName, Seq.empty, Seq.empty, Seq.empty, DataType(baseTypeName)) {
      override def isDomainBase:Boolean = true
    }
}

object Model {
  /** Provides a base domain model to start evolving from.
   *
   * @param domainName The name of the domain which is being modeled.
   * @param baseTypeName The name of the data type that is being modeled.
   */
  def base(domainName: String, baseTypeName: String): Model =
    new Model(domainName, Seq.empty, Seq.empty, DataType(baseTypeName)) {
      override def isDomainBase:Boolean = true
    }
}