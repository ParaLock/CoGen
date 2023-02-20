package org.combinators.graphics.application_domain.gui

class Element {

  var name : String = "default_name"
  var parent : Element = new Element()
  var children : List[Element] = List[Element]()
  var layout : AbsoluteLayout = new AbsoluteLayout()

}
