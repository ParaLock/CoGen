package org.combinators.robotics.domain_model.ros.roles

class Server(
              var serviceName: String,
              var requestMsgType: String,
              var responseMsgType: String,
              var payloadMsgType: String,
              var onRequestFragment: String
            ) extends Role  {

}
