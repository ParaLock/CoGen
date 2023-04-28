package org.combinators.gui.domain_model

class GridLayout(val rows: Int, val cols: Int) extends Layout {
  val elements = Array.ofDim[Element](rows * cols)

  def setElement(row: Int, col: Int, element: Element): Unit = {
    this.elements(rows * row + col) = element
  }
}
