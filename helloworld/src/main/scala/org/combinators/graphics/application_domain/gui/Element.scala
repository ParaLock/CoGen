package org.combinators.graphics.application_domain.gui

class Element {

  var name : String = "default_name"
  var children : List[Element] = List[Element]()
  var layout : AbsoluteLayout = new AbsoluteLayout()

}
