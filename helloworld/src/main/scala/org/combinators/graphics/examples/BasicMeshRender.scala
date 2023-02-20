package org.combinators.graphics.examples

import org.combinators.graphics.application_domain.gpu.{Mesh, Shader}

class BasicMeshRender {

  var houseMesh = new Mesh(
    "res/chalet.obj"
  )

  var vertexShader = new Shader("")
  var pixelShader = new Shader("")

  var worldTransform = new Matrix4f()
  var viewTransform = new Matrix4f()
  var projectionTransform = new Matrix4f()

  var bigBuffer = new LargeBuffer(houseMesh.data())
  bigBuffer.add(mesh.data())

  var smallBuffer = new SmallBuffer()
  smallBuffer.addNamedData(worldTransform, "world_transform")
  smallBuffer.addNamedData(viewTransform, "view_transform")
  smallBuffer.addNamedData(projectionTransform, "projection_transform")

  vertexShader.bind(bigBuffer)
  pixelShader.bind(smallBuffer)


}
