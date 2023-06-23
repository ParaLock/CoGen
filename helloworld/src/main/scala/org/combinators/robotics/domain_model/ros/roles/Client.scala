package org.combinators.robotics.domain_model.ros.roles;

import org.combinators.robotics.domain_model.ros.Node;

class Client(
              var serviceName: String,
              var requestMsgType: Class[_],
              var responseMsgType: Class[_],
              var onResponseFragment: String,
            ) extends Role {
}
