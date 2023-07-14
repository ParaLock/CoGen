package org.combinators.graphics

import org.combinators.gui.domain_model.{GridLayout, Text, Window}
import org.combinators.rendering.domain_model.resource_types.{Matrix, Mesh, Vector3f}
import org.combinators.rendering.domain_model.{AdhocBuffer, BulkBuffer, GraphicsPipeline, Image, ImageUsage, Shader}
import org.combinators.rendering.domain_model.policies.{CreationPolicy, RasterizationPolicy, StreamingPolicy, SynchronizationPolicy}
import org.combinators.robotics.domain_model.ros.Node

import java.util
import scala.collection.mutable.ArrayBuffer

class RenderingDomain {

  var synchronizationPolicy = new SynchronizationPolicy(false)

  def build_example(): Unit = {

    var pipelines: ArrayBuffer[Node] = ArrayBuffer[Node]()

    var worldMatrixType = new Matrix(4,4)
    var viewMatrixType = new Matrix(4,4)
    var projectionMatrixType = new Matrix(4,4)
    var magicMatrixType = new Matrix(2, 2)

    var vertexBuffer = new BulkBuffer(
      Seq[Tuple3[Float, Float, Float]].getClass,
      new CreationPolicy(true)
    )
    var indexBuffer = new BulkBuffer(
      Seq[Int].getClass,
      new CreationPolicy(true)
    )
    var transformBuffer = new AdhocBuffer(
      Seq(
        worldMatrixType.getClass,
        viewMatrixType.getClass,
        projectionMatrixType.getClass,
        magicMatrixType.getClass
      ),
      new CreationPolicy(false)
    )

    var xyzVecArrayType = Seq[Vector3f]()

    var lightsInfoBuffer = new AdhocBuffer(
      Seq(
        xyzVecArrayType.getClass
      ),
      new CreationPolicy(false)
    )

    var basicDiffuseTextureType = new Image(
      None,
      None,
      ImageUsage.Texture,
      new CreationPolicy(true)
    )

    var basicMeshVertShader = new Shader(
      "resources/basicMeshShader.vert",
      Seq(vertexBuffer, indexBuffer, transformBuffer)
    )

    var basicMeshPixelShader = new Shader(
      "resources/basicMeshShader.pixel",
      Seq(basicDiffuseTextureType)
    )

    var fullscreenVertShader = new Shader(
      "resources/fullscreenQuad.vert",
      Seq.empty
    )

    var basicLightingPixelShader = new Shader(
      "resources/basicLightingShader.pixel",
      Seq(lightsInfoBuffer)
    )

    var geometryBuffer = new Image(
      Some(1024),
      Some(800),
      ImageUsage.Texture,
      new CreationPolicy(false)
    )
    var framebuffer    = new Image(
      Some(1024),
      Some(800),
      ImageUsage.Display,
      new CreationPolicy(false)
    )

    var meshPipeline = new GraphicsPipeline(
      basicMeshVertShader,
      basicMeshPixelShader,
      new RasterizationPolicy(1,2, 3),
      Seq(geometryBuffer)
    )

    var lightingPipeline = new GraphicsPipeline(
      fullscreenVertShader,
      basicLightingPixelShader,
      new RasterizationPolicy(1, 2, 3),
      Seq(framebuffer)
    )

    pipelines += meshPipeline
    pipelines += lightingPipeline

  }

}
