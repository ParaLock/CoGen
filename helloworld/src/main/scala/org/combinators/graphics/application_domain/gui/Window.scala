package org.combinators.graphics.application_domain.gui

import scala.collection.mutable.ListBuffer

class Window(
              val width:Int,
              val height:Int,
              val mode:WindowMode
            ) {

  val elements: ListBuffer[Element] = ListBuffer[Element]()

  val hardwareAccelerated: Boolean = false
  def addElement(newElement: Element): Unit =  {
    elements += newElement
  }
}
