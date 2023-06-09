package org.combinators.robotics.domain_model.ros.roles;

import org.combinators.robotics.domain_model.ros.Node;

class Client(
              var serviceName: String,
              var requestMsgType: String,
              var responseMsgType: String,
              var payloadMsgType: String,
              var onResponseFragment: String,
            ) extends Role {
}
