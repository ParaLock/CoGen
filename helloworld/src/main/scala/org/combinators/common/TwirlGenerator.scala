package org.combinators.common

object Generator extends App {

  println(
    templates.java.FragmentTest(
      msg1="test1",
      msg2="test2"
    )
  )
}