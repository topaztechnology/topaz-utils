package com.topaz.utils

import java.util.concurrent.{Callable, ConcurrentHashMap}
import java.util.concurrent.atomic.AtomicLong

import com.github.benmanes.caffeine.cache.Caffeine
import scala.collection.JavaConverters._
import scala.collection.breakOut

trait Cache {
  def name: String
  def memoize[K <: Object, V <: Object](key: K)(f: => V): V
}

class CachedCallable(f: => AnyRef) extends Callable[AnyRef] {
  lazy val result: AnyRef = f
  override def call(): AnyRef = result
}

case class CaffeineCache private(name: String, maximumSize: Int) extends Cache with Log {

  private val cache: com.github.benmanes.caffeine.cache.Cache[Object, CachedCallable] = {
    var builder = Caffeine.newBuilder()
      .maximumSize(maximumSize)
    if (Cache.showCacheStats)
      builder = builder.recordStats()
    builder.build()
  }
  
  private val lastCacheStatsOutput = new AtomicLong(System.currentTimeMillis())
  private val showStatsEvery: Long = 10 * 1000
  
  override def memoize[K, V](key: K)(f:  => V): V = {
    if (Cache.showCacheStats) {
      val now = System.currentTimeMillis()
      val lastUpdate = lastCacheStatsOutput.get()
      if (now - lastUpdate > showStatsEvery) {
        lastCacheStatsOutput.set(now)
        log.info(s"Cache stats ($name): ${cache.stats()}")
      }
    }
    def call = f.asInstanceOf[AnyRef]
    cache.get(key.asInstanceOf[AnyRef], (_: Object) => new CachedCallable(call)).call().asInstanceOf[V]
  }
}

case class StaticCache private(name: String)
  extends Cache
{
  private val cache = new ConcurrentHashMap[AnyRef, CachedCallable]()

  def keys(): Iterator[AnyRef] = {
    cache.keys().asScala
  }
  def isEmpty(): Boolean = {
    cache.asScala.isEmpty
  }
  override def memoize[K, V](key: K)(f:  => V): V = {
    def call = f.asInstanceOf[AnyRef]

    val callable = new CachedCallable(call)
    Option(cache.putIfAbsent(key.asInstanceOf[AnyRef], callable)).map(
      _.call().asInstanceOf[V]
    ).getOrElse(callable.call().asInstanceOf[V])
  }

  def asScalaMap[K, V](): Map[K, V] = {
    cache.asScala.map {
      case (k, call) => (k.asInstanceOf[K], call.result.asInstanceOf[V])
    }(breakOut)
  }
}

object Cache {
  /**
    * A cache with no maximum size. Handy for caching things that you know
    * of which there are a small(-ish) amount and means you don't pay the price for working 
    * out an eviction policy.
    */
  def createStaticCache(name: String): StaticCache = {
    StaticCache(name)
  }

  /**
    * A cache backed by Window TinyLfu (https://github.com/ben-manes/caffeine/wiki/Efficiency) policy of eviction.
    */
  def createLimitedCache(name: String, maximumSize: Int): CaffeineCache = {
    CaffeineCache(name, maximumSize)
  }

  /**
    * Only works for limited caches. Prints out stats every 10 seconds but only if the
    * cache is in use.
    * -Dcom.topaz.showCacheStats=true
    */
  val showCacheStats: Boolean = Option(sys.props("com.topaz.showCacheStats")) match {
    case Some("true") => true
    case _ => false
  }
}
