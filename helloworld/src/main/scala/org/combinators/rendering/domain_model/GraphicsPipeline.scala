package org.combinators.rendering.domain_model

import org.combinators.rendering.domain_model.policies.RasterizationPolicy

class GraphicsPipeline(
                        inputs: Seq[RenderResource],
                        vertexShader: Shader,
                        pixelShader: Shader,
                        rasterizationPolicy: RasterizationPolicy,
                        outputs: Seq[Image]
                      ) {}
