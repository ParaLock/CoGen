package org.combinators.graphics

import org.combinators.gui.domain_model.{GridLayout, Text, Window}
import org.combinators.rendering.domain_model.{LargeBuffer, Matrix4f, Mesh, Shader, SmallBuffer}

class GUIDomain {

  val window: Window = new Window(
    800,
    600,
    "Basic Application"
  )

  val layout: GridLayout = new GridLayout(3,3)
  val msg = new Text(
    "Hello, world!"
  )

  layout.setElement(0,0, msg)
  layout.setElement(1, 0, msg)
  layout.setElement(2, 0, msg)

  layout.setElement(0, 1, msg)
  layout.setElement(1, 1, msg)
  layout.setElement(2, 1, msg)

  layout.setElement(0, 2, msg)
  layout.setElement(1, 2, msg)
  layout.setElement(2, 2, msg)



}
