package org.threeten.bp.zone

import java.util.{ Collection => JCollection, Map => JMap, Set => JSet, SortedMap => JSortedMap }
import java.util.AbstractMap
import java.util.AbstractMap.SimpleEntry
import java.util.Comparator

import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import scala.collection.immutable

// TreeMap is not available in Scala.js however it is needed for Time Zone support
// This is a simple implementation of NavigableMap, performance is likely terrible
private[bp] class ZoneMap[K: ClassTag, V] private[bp] (var map: immutable.TreeMap[K, V])(implicit
  ordering:                                                     Ordering[K]
) extends AbstractMap[K, V]
    with java.util.NavigableMap[K, V] {
  def this()(implicit ordering: Ordering[K]) =
    this(immutable.TreeMap[K, V]())

  override def descendingMap(): java.util.NavigableMap[K, V] = new ZoneMap[K, V](map)

  override def firstEntry(): java.util.Map.Entry[K, V] = {
    val fk = firstKey()
    map.get(fk).map(new SimpleEntry(fk, _)).getOrElse(null.asInstanceOf[java.util.Map.Entry[K, V]])
  }

  override def higherEntry(key: K): java.util.Map.Entry[K, V] = {
    val k = map.filterKeys(x => ordering.compare(x, key) > 0)
    if (k.isEmpty) null.asInstanceOf[java.util.Map.Entry[K, V]]
    else new SimpleEntry(k.head._1, k.head._2)
  }

  override def ceilingEntry(key: K): java.util.Map.Entry[K, V] = {
    val k = map.filterKeys(x => ordering.compare(x, key) >= 0)
    if (k.isEmpty) null.asInstanceOf[java.util.Map.Entry[K, V]]
    else new SimpleEntry(k.head._1, k.head._2)
  }

  override def pollFirstEntry(): java.util.Map.Entry[K, V] = {
    val fk    = firstKey()
    val entry = map
      .get(fk)
      .map(new SimpleEntry(fk, _))
      .getOrElse(null.asInstanceOf[java.util.Map.Entry[K, V]])
    map -= fk
    entry
  }

  override def floorEntry(key: K): java.util.Map.Entry[K, V] = {
    val k = map.filterKeys(x => ordering.compare(x, key) <= 0)
    if (k.isEmpty) null.asInstanceOf[java.util.Map.Entry[K, V]]
    else new SimpleEntry(k.last._1, k.last._2)
  }

  override def lowerEntry(key: K): java.util.Map.Entry[K, V] = {
    val k = map.filterKeys(x => ordering.compare(x, key) < 0)
    if (k.isEmpty) null.asInstanceOf[java.util.Map.Entry[K, V]]
    else new SimpleEntry(k.last._1, k.last._2)
  }

  override def pollLastEntry(): java.util.Map.Entry[K, V] = {
    val lk    = lastKey()
    val entry = map
      .get(lk)
      .map(new SimpleEntry(lk, _))
      .getOrElse(null.asInstanceOf[java.util.Map.Entry[K, V]])
    map -= lk
    entry
  }

  override def lastEntry(): java.util.Map.Entry[K, V] = {
    val lk = lastKey()
    map.get(lk).map(new SimpleEntry(lk, _)).getOrElse(null.asInstanceOf[java.util.Map.Entry[K, V]])
  }

  // Will not be implemented. It needs NavigableSet
  override def navigableKeySet() = ???

  override def subMap(fromKey: K, fromInclusive: Boolean, toKey: K, toInclusive: Boolean) = {
    val hk        =
      if (toInclusive) map.filterKeys(x => ordering.compare(x, toKey) <= 0)
      else
        map.filterKeys(x => ordering.compare(x, toKey) < 0)
    val fk        =
      if (fromInclusive) map.filterKeys(x => ordering.compare(x, fromKey) >= 0)
      else
        map.filterKeys(x => ordering.compare(x, fromKey) > 0)
    val intersect = hk.keySet.intersect(fk.keySet).map(k => k -> hk.get(k).getOrElse(fk(k))).toMap
    new ZoneMap(immutable.TreeMap(intersect.toSeq: _*))
  }

  override def subMap(fromKey: K, toKey: K) = subMap(fromKey, true, toKey, false)

  override def headMap(toKey: K, inclusive: Boolean): java.util.NavigableMap[K, V] = {
    val k =
      if (inclusive) map.filterKeys(x => ordering.compare(x, toKey) <= 0)
      else
        map.filterKeys(x => ordering.compare(x, toKey) < 0)
    if (k.isEmpty) new ZoneMap(immutable.TreeMap()) else new ZoneMap(immutable.TreeMap(k.toSeq: _*))
  }

  override def headMap(toKey: K): JSortedMap[K, V] = headMap(toKey, false)

  override def ceilingKey(key: K): K = {
    val k = map.filterKeys(x => ordering.compare(x, key) >= 0)
    if (k.isEmpty) null.asInstanceOf[K] else k.head._1
  }

  override def floorKey(key: K): K = {
    val k = map.filterKeys(x => ordering.compare(x, key) <= 0)
    if (k.isEmpty) null.asInstanceOf[K] else k.last._1
  }

  // Will not be implemented. It needs NavigableSet
  override def descendingKeySet() = ???

  override def tailMap(fromKey: K, inclusive: Boolean): java.util.NavigableMap[K, V] = {
    val k =
      if (inclusive) map.filterKeys(x => ordering.compare(x, fromKey) >= 0)
      else
        map.filterKeys(x => ordering.compare(x, fromKey) > 0)
    if (k.isEmpty) new ZoneMap(immutable.TreeMap()) else new ZoneMap(immutable.TreeMap(k.toSeq: _*))
  }

  override def tailMap(fromKey: K): JSortedMap[K, V] = tailMap(fromKey, true)

  override def lowerKey(key: K): K = {
    val k = map.filterKeys(x => ordering.compare(x, key) < 0)
    if (k.isEmpty) null.asInstanceOf[K] else k.last._1
  }

  override def higherKey(key: K) = {
    val k = map.filterKeys(x => ordering.compare(x, key) > 0)
    if (k.isEmpty) null.asInstanceOf[K] else k.head._1
  }

  override def firstKey(): K = map.firstKey

  override def comparator(): Comparator[K] = map.ordering

  override def lastKey(): K = map.lastKey

  override def values(): JCollection[V] = map.values.asJavaCollection

  override def put(key: K, value: V): V = {
    val prev: Option[V] = map.get(key)
    map += ((key, value))
    prev.getOrElse(null.asInstanceOf[V])
  }

  override def clear(): Unit =
    map = immutable.TreeMap()

  override def entrySet(): JSet[JMap.Entry[K, V]] =
    map
      .map { case (k, v) =>
        new SimpleEntry[K, V](k, v): JMap.Entry[K, V]
      }
      .toSet
      .asJava
}

object ZoneMap {

  def apply[K: ClassTag, V](map: immutable.TreeMap[K, V])(implicit
    ordering:                    Ordering[K]
  ): java.util.NavigableMap[K, V] = new ZoneMap[K, V](map)
}
