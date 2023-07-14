package org.combinators.rendering.domain_model

import org.combinators.rendering.domain_model.policies.CreationPolicy

class BulkBuffer(data: Class[_], creationPolicy: CreationPolicy) extends RenderResource {}
