package org.combinators.robotics.examples

import org.combinators.ep.domain.tree.Node
import org.combinators.robotics.domain_model.{AnalogSensor, Behaviour, DigitalSensor, MotorActuator, TimeBasedStrategy}

class RoboticsDomain {
  def build_simple_robot(): Unit = {

    var bumpSensor = new DigitalSensor()
    var distanceSensor = new AnalogSensor()

    var motorController = new MotorActuator()

    var driveStraight = new Behaviour()
    var turnNinetyDegrees = new Behaviour()
    var rankStrategy = new TimeBasedStrategy()

  }
}
