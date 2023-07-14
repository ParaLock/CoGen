package org.combinators.graphics

import org.combinators.gui.domain_model.{GridLayout, Text, Window}
import org.combinators.rendering.domain_model.resource_types.{Matrix, Mesh, Vector3f}
import org.combinators.rendering.domain_model.{AdhocBuffer, GraphicsPipeline, Image, ImageUsage, Shader, BulkBuffer}
import org.combinators.rendering.domain_model.policies.{RasterizationPolicy, StreamingPolicy}
import org.combinators.robotics.domain_model.ros.Node

import scala.collection.mutable.ArrayBuffer

class RenderingDomain {



  def build_example(): Unit = {

    var pipelines: ArrayBuffer[Node] = ArrayBuffer[Node]()

    var worldMatrixType = new Matrix(4,4)
    var viewMatrixType = new Matrix(4,4)
    var projectionMatrixType = new Matrix(4,4)
    var magicMatrixType = new Matrix(2, 2)

    var vertexBuffer = new BulkBuffer(
      Tuple3[Float, Float, Float].getClass
    )
    var indexBuffer = new AdhocBuffer(
      Seq(Int.getClass)
    )
    var transformMatrixBuffer = new AdhocBuffer(
      Seq(
        worldMatrixType.getClass,
        viewMatrixType.getClass,
        projectionMatrixType.getClass,
        magicMatrixType.getClass
      )
    )

    var xyzVecArrayType = Seq[Vector3f]()
    var meshScaleBuffer = new AdhocBuffer(
      Seq(
        Float.getClass
      )
    )

    var lightsPosBuffer = new AdhocBuffer(
      Seq(
        xyzVecArrayType.getClass
      )
    )

    var basicDiffuseTextureType = new Image(
      None,
      None,
      ImageUsage.Texture
    )

    var basicMeshVertShader = new Shader(
      "resources/basicMeshShader.vert",
      Seq(vertexBuffer, indexBuffer, transformMatrixBuffer)
    )

    var basicMeshPixelShader = new Shader(
      "resources/basicMeshShader.pixel",
      Seq(basicDiffuseTextureType, meshScaleBuffer)
    )

    var fullscreenVertShader = new Shader(
      "resources/fullscreenQuad.vert",
      Seq.empty
    )

    var basicLightingPixelShader = new Shader(
      "resources/basicLightingShader.pixel",
      Seq(lightsPosBuffer)
    )



    var geometryBuffer = new Image(Some(1024), Some(800), ImageUsage.Texture)
    var framebuffer    = new Image(Some(1024), Some(800), ImageUsage.Display)

    var meshPipeline = new GraphicsPipeline(

      basicMeshVertShader,
      basicMeshPixelShader,
      new RasterizationPolicy(1,2, 3),
      geometryBuffer
    )

    var lightingPipeline = new GraphicsPipeline(
      fullscreenVertShader,
      basicLightingPixelShader,
      new RasterizationPolicy(1, 2, 3),
      framebuffer
    )

    pipelines += meshPipeline
    pipelines += lightingPipeline

  }

}
