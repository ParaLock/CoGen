package org.combinators.robotics.domain_model.ros.roles

class Subscriber(
                  var topic: String,
                  var msgType: String,
                  var onMsgFragment: String
                ) extends Role {

}
