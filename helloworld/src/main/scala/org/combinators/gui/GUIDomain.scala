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

  for(i <- 0 to 2) {
    for (j <- 0 to 2) {
      layout.setElement(i, j, new Text(s"Label ${i}, ${j}"))
    }
  }
}
