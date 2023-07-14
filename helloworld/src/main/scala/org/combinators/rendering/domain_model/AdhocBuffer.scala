package org.combinators.rendering.domain_model

import org.combinators.rendering.domain_model.policies.CreationPolicy

class AdhocBuffer(elements: Seq[Class[_]], creationPolicy: CreationPolicy) extends RenderResource {}
