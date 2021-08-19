package org.threeten.bp

import java.util.NavigableMap
import java.util.TreeMap
import scala.collection.JavaConverters._

object Platform {
  type NPE = NullPointerException
  type DFE = IndexOutOfBoundsException
  type CCE = ClassCastException

  /**
   * Returns `true` if and only if the code is executing on a JVM. Note: Returns `false` when
   * executing on any JS VM.
   */
  final val executingInJVM = false

  def setupLocales(): Unit = ()

  def zoneMap(m: scala.collection.immutable.TreeMap[Int, String]): NavigableMap[Int, String] =
    new TreeMap(m.asJava)
}
