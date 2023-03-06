package org.combinators.graphics

import org.combinators.gui.domain_model.{GridLayout, Text, Window}
import org.combinators.rendering.domain_model.{LargeBuffer, Matrix4f, Mesh, Shader, SmallBuffer}

class GUIDomain {
  def build_basic_gui_example(): Unit = {

    var window = new Window(
      800,
      600
    );

    var layout = new GridLayout()

    window.addElement(
      new Text(
        "Hello, world!"
      )
    )
  }

}
