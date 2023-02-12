package org.combinators.graphics.domain

class WindowMode extends Enumeration {
  type ModeType = Value
  val Fullscreen, Windowed = Value
}

class Window(
              val width:Int,
              val height:Int,
              val mode:WindowMode
            ) {

  val hardwareAccelerated: Boolean = false
}
