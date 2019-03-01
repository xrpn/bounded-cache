package com.xrpn.boundedcache.algebra

import cats.effect.Sync
import cats.effect.concurrent.Ref
import com.xrpn.boundedcache.util.BoundedFIFOMappedQueue

trait BoundedCache[F[_], K, V] {
  def lookup(k: K): F[Option[V]]
  def upsert(k: K, v: V): F[Unit]
  def invalidate(k: K): F[Unit]
  def clear: F[Unit]
  def isFull: F[Boolean]
}

object BoundedCache {

  sealed trait EvictionPolicy
  case object LRU extends EvictionPolicy // Least Recently Used
  case object FIFO extends EvictionPolicy // First In, First Out
  case object LIFO extends EvictionPolicy // Last In, First Out
  case object TTL extends EvictionPolicy // Time To Live

  def FIFOImpl[F[_]: Sync, K, V](size: Int): F[BoundedCache[F, K, V]] = {
    import cats.implicits._
    for {
      csRef <- Ref.of[F, BoundedCacheStore[K, V]](BoundedFIFOMappedQueue.create[K, V](size))
    } yield new BoundedCache[F, K, V] {

        override def lookup(k: K): F[Option[V]] =
          csRef.get
            .map(_.lookup(k))

        override def upsert(k: K, v: V): F[Unit] =
          csRef
            .modify[Unit] { cs: BoundedCacheStore[K, V] =>
              (cs.upsert(k, v), ())
            }

        override def invalidate(k: K): F[Unit] =
          csRef
            .modify[Unit] { cs: BoundedCacheStore[K, V] =>
              (cs.invalidate(k), ())
            }

        override def clear: F[Unit] =
          csRef
            .modify[Unit] { cs: BoundedCacheStore[K, V] =>
              (cs.clear, ())
            }

        override def isFull: F[Boolean] =
          csRef.get
            .map(_.isFull)

      }
  }
}