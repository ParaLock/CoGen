package org.combinators.robotics.domain_model.ros.roles

class Subscriber(
                  var topic: String,
                  var msgType: Class[_],
                  var onMsgFragment: String
                ) extends Role {

}
