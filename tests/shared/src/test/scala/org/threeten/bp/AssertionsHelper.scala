package org.threeten.bp

import org.scalactic.{ Prettifier, source }
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/**
 * Helper methods to avoid rewriting much of the TestNG tests
 */
trait AssertionsHelper { this: AnyFunSuite =>
  def isIsoLeap(year: Long): Boolean =
    if (year % 4 != 0)
      false
    else if (year % 100 == 0 && year % 400 != 0)
      false
    else
      true

  def assertEquals[A, B](
    o1:  A,
    o2:  B,
    msg: String
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(o1 == o2, msg)

  def assertEquals[A, B](
    o1: A,
    o2: B
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(o1 == o2)

  def assertSame[A <: AnyRef, B <: AnyRef](
    o1: A,
    o2: B
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(o1 eq o2)

  def assertNotEquals[A, B](
    o1: A,
    o2: B
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(o1 != o2)

  def assertFalse(b: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(!b)

  def assertTrue(b: Boolean)(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(b)

  def assertNull[A](a: A)(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(a == null)

  def assertNotNull[A](
    a:   A,
    msg: String
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(a != null, msg)

  def assertNotNull[A](a: A)(implicit prettifier: Prettifier, pos: source.Position): Assertion =
    assert(a != null)

}
