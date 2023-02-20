package org.combinators.graphics.application_domain.gpu

class ShaderType extends Enumeration {
  type ShaderType = Value
  val Vertex, Pixel, Geometry, Compute = Value
}
