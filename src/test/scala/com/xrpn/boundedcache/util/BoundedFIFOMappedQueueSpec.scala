package com.xrpn.boundedcache.util
import com.xrpn.boundedcache.algebra.BoundedCacheStore
import org.specs2.mutable.Specification

class BoundedFIFOMappedQueueSpec extends Specification {

  "BoundedFIFOMappedQueue" should {
    "accept input and cache it" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](1)
      mq.isFull must beFalse
      val mqp = mq.upsert("One",1)
      mqp.lookup("One") must beSome(1)
      mqp.isFull must beTrue
    }

    "accept input degenerate" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](0)
      mq.isFull must beTrue
      mq.upsert("One",1).lookup("One") must beNone
    }

    "invalidate some content" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](2)
      val pp = mq.upsert("One",1).upsert("Two",2)
      pp.isFull must beTrue
      val ppi = pp.invalidate("One")
      ppi.isFull must beFalse
      ppi.lookup("One") must beNone
      ppi.lookup("Two") must beSome(2)
    }

    "invalidate non-existing content" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](2)
      val pp = mq.upsert("One",1).upsert("Two",2).invalidate("Foo")
      pp.isFull must beTrue
      pp.lookup("One") must beSome(1)
      pp.lookup("Two") must beSome(2)
    }

    "clear all contents" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](2)
      val pp = mq.upsert("One",1).upsert("Two",2).clear
      pp.isFull must beFalse
      pp.lookup("One") must beNone
      pp.lookup("Two") must beNone
    }

    "enforce bound and FIFO eviction trivially on insert" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](1)
      val populated = mq.upsert("One",1).upsert("Two",2)
      populated.lookup("One") must beNone
      populated.lookup("Two") must beSome(2)
    }

    "enforce bound and FIFO eviction on insert" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](3)
      val populated = mq.upsert("One",1).upsert("Two",2).upsert("Three",3).upsert("Four",4)
      populated.isFull must beTrue
      populated.lookup("One") must beNone
      populated.lookup("Two") must beSome(2)
      populated.lookup("Three") must beSome(3)
      populated.lookup("Four") must beSome(4)
    }

    "enforce bound and FIFO eviction trivially on update" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](1)
      val ppa = mq.upsert("One",1)
      ppa.isFull must beTrue
      val ppb = ppa.upsert("One",2)
      ppb.isFull must beTrue
      ppb.lookup("One") must beSome(2)
    }

    "enforce bound and FIFO eviction on update" in {
      val mq: BoundedCacheStore[String, Int] = BoundedFIFOMappedQueue.create[String,Int](3)
      val ppa = mq.upsert("One",1).upsert("Two",2).upsert("Three",3).upsert("Four",4).upsert("Three",6)
      ppa.isFull must beTrue
      ppa.lookup("One") must beNone
      ppa.lookup("Two") must beSome(2)
      ppa.lookup("Three") must beSome(6)
      ppa.lookup("Four") must beSome(4)
      val ppb = ppa.upsert("Five",5).upsert("Six",6)
      ppb.isFull must beTrue
      ppb.lookup("One") must beNone
      ppb.lookup("Two") must beNone
      ppb.lookup("Three") must beNone
      ppb.lookup("Four") must beSome(4)
      ppb.lookup("Five") must beSome(5)
      ppb.lookup("Six") must beSome(6)
    }

  }

}
