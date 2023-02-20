package org.combinators.graphics.examples.simple
import org.combinators.graphics.application_domain.gui.{AbsoluteLayout, Text, Window, WindowMode}


class BasicForm {
  def build(): Unit = {

    var window = new Window(
      800,
      600,
      WindowMode.Windowed
    );

    var layout = new AbsoluteLayout()

    window.addElement(
      new Text(
        "Hello, world!"
      )
    )

  }
}
