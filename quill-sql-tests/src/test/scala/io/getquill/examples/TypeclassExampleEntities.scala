package io.getquill.examples

import io.getquill.MirrorContext.Codec.*

object TypeclassExampleEntities {
  case class Node(id: Int, timestamp: Int, status: String)
  case class Master(key: Int, lastCheck: Int, state: String)
  case class Worker(shard: Int, lastTime: Int, reply: String)
  given CompositeDecoder[Node] = deriveComposite
  given CompositeDecoder[Master] = deriveComposite
  given CompositeDecoder[Worker] = deriveComposite
}
