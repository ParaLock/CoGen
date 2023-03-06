package org.combinators.graphics

import org.combinators.gui.domain_model.{GridLayout, Text, Window}
import org.combinators.rendering.domain_model.{LargeBuffer, Matrix4f, Mesh, Shader, SmallBuffer}

class RenderingDomain {

  def build_mesh_render_example(): Unit = {

    var houseMesh = new Mesh(
      "res/chalet.obj"
    )

    var worldTransform = new Matrix4f()
    var viewTransform = new Matrix4f()
    var projectionTransform = new Matrix4f()

    var bigBuffer = new LargeBuffer(
      houseMesh.dataHandle()
    )
    var smallBuffer = new SmallBuffer()
    smallBuffer.addNamedData(worldTransform.dataHandle(), "world_transform")
    smallBuffer.addNamedData(viewTransform.dataHandle(), "view_transform")
    smallBuffer.addNamedData(projectionTransform.dataHandle(), "projection_transform")

    var vertexShader = new Shader("")
    var pixelShader = new Shader("")

    vertexShader.bind(bigBuffer)
    pixelShader.bind(smallBuffer)
  }

}
