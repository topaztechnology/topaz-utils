package com.topaz.utils

trait InternedData[T] {
  protected def cacheName: String
  protected val cache: Cache = Cache.createStaticCache(cacheName)
}
