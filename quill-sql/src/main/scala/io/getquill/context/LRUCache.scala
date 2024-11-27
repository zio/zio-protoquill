package io.getquill.context

import scala.collection.mutable

class LRUCache[K, V](val capacity: Int) {
  require(capacity >= 1, "A Cache of negative or 0 capacity makes no sense")
  private val cache = mutable.LinkedHashMap.empty[K, V]

  private def evict(): Unit = {
    cache.remove(cache.head._1) // Evict the least recently used item
  }

  def get(key: K): Option[V] = synchronized {
    // When reading, we attempt to remove the value
    cache.remove(key) match {
      case Some(value) =>
        cache.put(
          key,
          value
        ) // Put it back at the end to indicate recent access
        Some(value)
      case None => None
    }
  }

  def getOrDefault(key: K, default: => V) = synchronized {
    cache.get(key) match {
      case Some(value) => value
      case None =>
        val comuted = default
        put(key, comuted)
        comuted
    }
  }

  def put(key: K, value: V): Unit = synchronized {
    cache.remove(key) match {
      case _ if cache.size >= capacity =>
        evict()
        cache.put(key, value) // Add new element at the end
      case _ =>
        cache.put(key, value)
    }
  }
}