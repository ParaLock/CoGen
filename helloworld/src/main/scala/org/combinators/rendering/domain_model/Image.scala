package org.combinators.rendering.domain_model

import org.combinators.rendering.domain_model.policies.CreationPolicy

class Image(initalHeight: Option[Int], initialWidth: Option[Int], usage: Int, creationPolicy: CreationPolicy) extends RenderResource {}
