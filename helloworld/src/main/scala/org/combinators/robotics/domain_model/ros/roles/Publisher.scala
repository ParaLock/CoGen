package org.combinators.robotics.domain_model.ros.roles;

class Publisher (
                 var topic: String,
                 var msgType: Class[_]
               ) extends Role {
}
