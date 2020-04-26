package com.topaz.utils

class CaseInsensitiveMap[V] private(val original: Map[String, V]) extends Map[String, V] {
  private val caseInsensitive = original.map {
    case (k, v) => k.toLowerCase() -> v
  }

  def +[V1 >: V](kv: (String, V1)): Map[String, V1] = {
    new CaseInsensitiveMap(original + kv)
  }

  def get(key: String): Option[V] = caseInsensitive.get(key.toLowerCase)

  def iterator: Iterator[(String, V)] = original.iterator

  def -(key: String): Map[String, V] = {
    new CaseInsensitiveMap(original.filterNot(_._1.toLowerCase == key.toLowerCase))
  }

  def asNormalMap: Map[String, V] = original
}

object CaseInsensitiveMap {
  def apply[V](map: Map[String, V]): Map[String, V] = {
    require(
      map.keys.map(_.toLowerCase).size == map.size,
      s"Can't create a CaseInsensitiveMap when multiple keys would map to the same value: ${map.keys.mkString(", ")}"
    )
    new CaseInsensitiveMap(map)
  }

  def apply[V](): Map[String, V] = new CaseInsensitiveMap(Map.empty)

  def apply[V](elems: (String, V)*): Map[String, V] = {
    apply(Map(elems: _*))
  }
}
