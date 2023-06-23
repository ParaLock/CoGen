package org.combinators.robotics.domain_model.ros.roles

class Server(
              var serviceName: String,
              var requestMsgType: Class[_],
              var responseMsgType: Class[_],
              var onRequestFragment: String
            ) extends Role  {

}
