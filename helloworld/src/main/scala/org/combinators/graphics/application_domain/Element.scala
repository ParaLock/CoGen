package org.combinators.graphics.application_domain

class Element {

  var name : String = "default_name"
  var parent : Element = new Element()
  var children : List[Element] = List[Element]()
  var layout : Layout = new Layout()

}
