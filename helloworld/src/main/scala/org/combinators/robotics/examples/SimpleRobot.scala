package org.combinators.robotics.examples

import org.combinators.ep.domain.tree.Node
import org.combinators.robotics.application_domain.{AnalogSensor, Behaviour, DigitalSensor, MotorActuator, TimeBasedStrategy}

class SimpleRobot {
  def build(): Unit = {

    var bumpSensor = new DigitalSensor()
    var distanceSensor = new AnalogSensor()

    var motorController = new MotorActuator()

    var driveStraight = new Behaviour()
    var turnNinetyDegrees = new Behaviour()
    var rankStrategy = new TimeBasedStrategy()

  }
}
