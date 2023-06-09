package org.combinators.robotics.examples

import org.combinators.robotics.domain_model.ros.Node
import org.combinators.robotics.domain_model.ros._
import org.combinators.robotics.domain_model.ros.roles.{Publisher, Subscriber}
import org.combinators.robotics.domain_model.{AnalogSensor, Behaviour, DigitalSensor, MotorActuator, TimeSliceStrategy}

import scala.collection.mutable.ArrayBuffer

class RoboticsDomain {

  var nodes: ArrayBuffer[Node] = ArrayBuffer[Node]()

  var testServiceName: String = "test/service/sum"
  var testTopicName: String = "test/topic"

  nodes += new Node(
    "A",
    new roles.Client(
      testServiceName,
      "AddTwoIntsRequest",
      "AddTwoIntsResponse",
      "AddTwoInts",
      "ClientOnResponse.java"
    ),
    "ClientLoopFragment.java"
  )

  nodes += new Node(
    "B",
    new roles.Server(
      testServiceName,
      "AddTwoIntsRequest",
      "AddTwoIntsResponse",
      "AddTwoInts",
      "ServerOnRequest.java"
    ),
    "ServerLoopFragment.java"
  )

  nodes += new Node(
    "C",
    new roles.Publisher(
      testTopicName,
      "String"
    ),
    "PublisherLoopFragment.java"
  )

  nodes += new Node(
    "D",
    new Subscriber(
      testTopicName,
      "String",
      "ClientOnMessage.java"
    ),
    "SubscriberLoopFragment.java"
  )

}
