package org.combinators.robotics.domain_model.ros

import org.combinators.robotics.domain_model.ros.roles.Role

class Node(
             var name: String,
             var role: Role,
             var loopFragment: String
             ) {
}
