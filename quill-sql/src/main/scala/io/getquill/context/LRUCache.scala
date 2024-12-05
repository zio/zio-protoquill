package io.getquill.context
//
//import scala.collection.mutable
//
//
//
//class LRUCache[K, V](val capacity: Int) {
//  require(capacity >= 1, "A Cache of negative or 0 capacity makes no sense")
//  private val cache = mutable.LinkedHashMap.empty[K, V]
//
//  private def evict(): Unit = {
//    cache.remove(cache.head._1) // Evict the least recently used item
//  }
//
//  def get(key: K): Option[V] = synchronized {
//    // When reading, we attempt to remove the value
//    cache.remove(key) match {
//      case Some(value) =>
//        cache.put(
//          key,
//          value
//        ) // Put it back at the end to indicate recent access
//        Some(value)
//      case None => None
//    }
//  }
//
//  def getOrDefault(key: K, default: => V) = synchronized {
//    cache.get(key) match {
//      case Some(value) => value
//      case None =>
//        val comuted = default
//        put(key, comuted)
//        comuted
//    }
//  }
//
//  def put(key: K, value: V): Unit = synchronized {
//    cache.remove(key) match {
//      case _ if cache.size >= capacity =>
//        evict()
//        cache.put(key, value) // Add new element at the end
//      case _ =>
//        cache.put(key, value)
//    }
//  }
//}


import java.util.Collections.synchronizedMap

import scala.jdk.CollectionConverters._
import scala.collection.mutable

class LRUCache[K, V](maxEntries: Int)
  extends java.util.LinkedHashMap[K, V](100, .75f, true) {

  require(maxEntries >= 1, "A Cache of negative or 0 capacity makes no sense")

  override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean = size > maxEntries
}

object LRUCache {
  //def apply[K, V](maxEntries: Int): mutable.Map[K, V] = synchronizedMap(new LRUCache[K, V](maxEntries)).asScala
  def apply[K, V](maxEntries: Int): java.util.Map[K, V] = synchronizedMap(new LRUCache[K, V](maxEntries))
}

extension [K, V](map: LRUCache[K, V]) {
  def getOrDefault(key: K, default: => V) = synchronized {
    val value = map.get(key)
    if (value != null) value
    else {
      val computed = default
      map.put(key, computed)
      computed
    }
  }
}

