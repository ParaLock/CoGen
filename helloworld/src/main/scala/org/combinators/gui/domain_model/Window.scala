package org.combinators.gui.domain_model

import scala.collection.mutable.ListBuffer

class Window(
              val width:Int,
              val height:Int
            ) {

  val elements: ListBuffer[Element] = ListBuffer[Element]()

  val hardwareAccelerated: Boolean = false
  def addElement(newElement: Element): Unit =  {
    elements += newElement
  }
}
