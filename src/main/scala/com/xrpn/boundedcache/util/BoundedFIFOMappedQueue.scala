package com.xrpn.boundedcache.util

import com.xrpn.boundedcache.algebra.BoundedCacheStore

import scala.collection.immutable

object BoundedFIFOMappedQueue {

  import MappedQueue._

  private[this] def impl[K,V](mq: MappedQueue[K,V], maxSize: Int): BoundedCacheStore[K,V] = new BoundedCacheStore[K,V]{
    override def upsert(k: K, v: V): BoundedCacheStore[K,V] =
      if (0 == maxSize) impl(mq,0)
      else if (mq.cacheQueue.exists(_ == k)) {
        impl(update(mq)(k,v),maxSize)
      } else {
        val boundedMq = if (!isFull) mq else fifoEvict(mq)
        impl(insert(boundedMq)(k,v), maxSize)
      }
    override def lookup(k: K): Option[V] = get(mq)(k)
    override def invalidate(k: K): BoundedCacheStore[K, V] =
      if (mq.cacheQueue.exists(_ == k)) impl(remove(mq)(k),maxSize)
      else impl(mq,maxSize)
    override def clear: BoundedCacheStore[K, V] = create(maxSize)
    override def isFull: Boolean = maxSize <= mq.cacheQueue.length
  }

  def create[K,V](maxSize: Int): BoundedCacheStore[K, V] =
    impl(MappedQueue(immutable.HashMap.empty[K,V],immutable.Queue.newBuilder[K].result),maxSize)

}
