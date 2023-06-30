package org.combinators.robotics.examples

import org.combinators.robotics.domain_model.ros.messages._
import org.combinators.robotics.domain_model.ros.Node
import org.combinators.robotics.domain_model.ros._
import org.combinators.robotics.domain_model.ros.roles.{Publisher, Subscriber}

import scala.collection.mutable.ArrayBuffer


class RoboticsDomain {

  var nodes: ArrayBuffer[Node] = ArrayBuffer[Node]()

  var testServiceName: String = "test/service/sum"
  var testTopicName: String = "test/topic"

  nodes += new Node(
    "Client",
    new roles.Client(
      testServiceName,
      new SumRequest().getClass,
      new SumResponse().getClass,
      "/Robotics/ROS_Fragments/ClientOnResponse.java"
    ),
    Some("/Robotics/ROS_Fragments/ClientOnLoop.java")
  )

  nodes += new Node(
    "Server",
    new roles.Server(
      testServiceName,
      new SumRequest().getClass,
      new SumResponse().getClass,
      "/Robotics/ROS_Fragments/ServerOnRequest.java"
    ),
  )

  nodes += new Node(
    "Publisher",
    new roles.Publisher(
      testTopicName,
      new StringMessage().getClass
    ),
    Some("/Robotics/ROS_Fragments/PublisherOnLoop.java")
  )

  nodes += new Node(
    "Subscriber",
    new Subscriber(
      testTopicName,
      new StringMessage().getClass,
      "/Robotics/ROS_Fragments/SubscriberOnMessage.java"
    )
  )

}
