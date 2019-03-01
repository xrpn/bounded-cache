package com.xrpn.boundedcache.util
import scala.collection.immutable

object MappedQueue {


  private[util] case class MappedQueue[K, V](
                                              cacheMap: immutable.Map[K, V],
                                              cacheQueue: immutable.Queue[K]) {
    // maintaining this invariant requires a lock on access
    require(
      cacheMap.size == cacheQueue.length,
      "invariant violation: different number of items in cacheMap and cacheQueue")
    // check this invariant while testing only (expensive)
    // require(cacheQueue.iterator.toList.forall(key => cacheMap.keySet.contains(key)),"invariant violation: different key set in cacheMap and cacheQueue")
  }

  private[util] def fifoEvict[K, V](mq: MappedQueue[K, V]): MappedQueue[K, V] = {
    // FIFO eviction must happen inside a lock to maintain MappedQueue invariants
    if (mq.cacheQueue.isEmpty /* nothing to evict */) mq
    else {
      val (key, queue) = mq.cacheQueue.dequeue
      MappedQueue(mq.cacheMap.-(key), queue)
    }
  }

  //
  // naked CRUD functionality; invariants must be maintained (locks needed)
  //

  private[util] def update[K, V](mq: MappedQueue[K, V])(k: K, v: V): MappedQueue[K, V] = {
    val newCacheMap = mq.cacheMap.-(k)
    MappedQueue(newCacheMap.+((k, v)), mq.cacheQueue)
  }

  private[util] def insert[K, V](mq: MappedQueue[K, V])(k: K, v: V): MappedQueue[K, V] = {
    MappedQueue(mq.cacheMap.+((k, v)), mq.cacheQueue.enqueue(k))
  }

  private[util] def get[K, V](mq: MappedQueue[K, V])(k: K): Option[V] = mq.cacheMap.get(k)

  private[util] def remove[K, V](mq: MappedQueue[K, V])(k: K): MappedQueue[K, V] = {
    MappedQueue[K, V](
      mq.cacheMap.-(k),
      immutable.Queue[K](mq.cacheQueue.iterator.toList.filterNot(_ == k): _*))
  }
}
