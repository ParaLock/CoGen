package org.combinators.robotics.domain_model.ros

class Node(
             name: String,
             oneToOneCons: Seq[OneToOneConnection],
             manyToManyCons: Seq[ManyToManyConnection]
             ) {
}
