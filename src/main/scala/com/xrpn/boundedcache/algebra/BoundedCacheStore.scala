package com.xrpn.boundedcache.algebra

trait BoundedCacheStore[K, V] {
  def upsert(k: K, v: V): BoundedCacheStore[K,V] // assumed to be size-safe or w/e
  def lookup(k: K): Option[V]
  def invalidate(k: K): BoundedCacheStore[K,V]
  def clear: BoundedCacheStore[K,V]
  def isFull: Boolean
}
