/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten.bp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoField.{
  INSTANT_SECONDS,
  MICRO_OF_SECOND,
  MILLI_OF_SECOND,
  NANO_OF_SECOND
}
import org.threeten.bp.temporal.ChronoUnit.{ DAYS, NANOS, SECONDS }
import org.threeten.bp.temporal._

/** Test Instant. */
object TestInstant {
  val MIN_SECOND: Long = Instant.MIN.getEpochSecond
  val MAX_SECOND: Long = Instant.MAX.getEpochSecond
}

class TestInstant
    extends AnyFunSuite
    with GenDateTimeTest
    with AssertionsHelper
    with BeforeAndAfter {
  private var TEST_12345_123456789: Instant = null

  before {
    TEST_12345_123456789 = Instant.ofEpochSecond(12345, 123456789)
  }

  protected def samples: List[TemporalAccessor] =
    List(TEST_12345_123456789, Instant.MIN, Instant.MAX, Instant.EPOCH)

  protected def validFields: List[TemporalField] =
    List(NANO_OF_SECOND, MICRO_OF_SECOND, MILLI_OF_SECOND, INSTANT_SECONDS)

  protected def invalidFields: List[TemporalField] = {
    val list: List[TemporalField] = List(ChronoField.values: _*)
    (list :+ JulianFields.JULIAN_DAY :+ JulianFields.MODIFIED_JULIAN_DAY :+ JulianFields.RATA_DIE)
      .filterNot(validFields.contains)
  }

  private def check(instant: Instant, epochSecs: Long, nos: Int): Unit = {
    assertEquals(instant.getEpochSecond, epochSecs)
    assertEquals(instant.getNano, nos)
    assertEquals(instant, instant)
    assertEquals(instant.hashCode, instant.hashCode)
  }

  test("constant_EPOCH") {
    check(Instant.EPOCH, 0, 0)
  }

  test("constant_MIN") {
    check(Instant.MIN, -31557014167219200L, 0)
  }

  test("constant_MAX") {
    check(Instant.MAX, 31556889864403199L, 999999999)
  }

  test("now") {
    val expected: Instant = Instant.now(Clock.systemUTC)
    val test: Instant     = Instant.now
    val diff: Long        = Math.abs(test.toEpochMilli - expected.toEpochMilli)
    assertTrue(diff < 100)
  }

  test("now_Clock_nullClock") {
    assertThrows[NullPointerException] {
      Instant.now(null)
    }
  }

  test("now_Clock_allSecsInDay_utc") {
    var i: Int = 0
    while (i < (2 * 24 * 60 * 60)) {
      {
        val expected: Instant = Instant.ofEpochSecond(i).plusNanos(123456789L)
        val clock: Clock      = Clock.fixed(expected, ZoneOffset.UTC)
        val test: Instant     = Instant.now(clock)
        assertEquals(test, expected)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("now_Clock_allSecsInDay_beforeEpoch") {
    var i: Int = -1
    while (i >= -(24 * 60 * 60)) {
      {
        val expected: Instant = Instant.ofEpochSecond(i).plusNanos(123456789L)
        val clock: Clock      = Clock.fixed(expected, ZoneOffset.UTC)
        val test: Instant     = Instant.now(clock)
        assertEquals(test, expected)
      }
      {
        i -= 1
        i + 1
      }
    }
  }

  test("factory_seconds_long") {
    var i: Long = -2
    while (i <= 2) {
      {
        val t: Instant = Instant.ofEpochSecond(i)
        assertEquals(t.getEpochSecond, i)
        assertEquals(t.getNano, 0)
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("factory_seconds_long_long") {
    var i: Long = -2
    while (i <= 2) {
      {
        {
          var j: Int = 0
          while (j < 10) {
            {
              val t: Instant = Instant.ofEpochSecond(i, j)
              assertEquals(t.getEpochSecond, i)
              assertEquals(t.getNano, j)
            }
            {
              j += 1
              j - 1
            }
          }
        }
        {
          var j: Int = -10
          while (j < 0) {
            {
              val t: Instant = Instant.ofEpochSecond(i, j)
              assertEquals(t.getEpochSecond, i - 1)
              assertEquals(t.getNano, j + 1000000000)
            }
            {
              j += 1
              j - 1
            }
          }
        }
        {
          var j: Int = 999999990
          while (j < 1000000000) {
            {
              val t: Instant = Instant.ofEpochSecond(i, j)
              assertEquals(t.getEpochSecond, i)
              assertEquals(t.getNano, j)
            }
            {
              j += 1
              j - 1
            }
          }
        }
      }
      {
        i += 1
        i - 1
      }
    }
  }

  test("factory_seconds_long_long_nanosNegativeAdjusted") {
    val test: Instant = Instant.ofEpochSecond(2L, -1)
    assertEquals(test.getEpochSecond, 1)
    assertEquals(test.getNano, 999999999)
  }

  test("factory_seconds_long_long_tooBig") {
    assertThrows[DateTimeException] {
      Instant.ofEpochSecond(TestInstant.MAX_SECOND, 1000000000)
    }
  }

  test("factory_seconds_long_long_tooBigBig") {
    assertThrows[ArithmeticException] {
      Instant.ofEpochSecond(Long.MaxValue, Long.MaxValue)
    }
  }

  private def provider_factory_millis_long: List[List[Long]] =
    List(
      List[Long](0, 0, 0, 0),
      List[Long](0, 999999, 0, 999999),
      List[Long](1, 0, 0, 1000000),
      List[Long](1, 1, 0, 1000001),
      List[Long](2, 0, 0, 2000000),
      List[Long](999, 0, 0, 999000000),
      List[Long](1000, 0, 1, 0),
      List[Long](1001, 0, 1, 1000000),
      List[Long](-1, 1, -1, 999000001),
      List[Long](-1, 0, -1, 999000000),
      List[Long](-2, 999999, -1, 998999999),
      List[Long](-2, 0, -1, 998000000),
      List[Long](-999, 0, -1, 1000000),
      List[Long](-1000, 0, -1, 0),
      List[Long](-1001, 0, -2, 999000000),
      List[Long](Long.MaxValue, 0, Long.MaxValue / 1000, (Long.MaxValue % 1000).toInt * 1000000),
      List[Long](Long.MaxValue - 1,
                 0,
                 (Long.MaxValue - 1) / 1000,
                 ((Long.MaxValue - 1) % 1000).toInt * 1000000
      ),
      List[Long](Long.MinValue,
                 0,
                 (Long.MinValue / 1000) - 1,
                 (Long.MinValue % 1000).toInt * 1000000 + 1000000000
      ),
      List[Long](Long.MinValue,
                 1,
                 (Long.MinValue / 1000) - 1,
                 (Long.MinValue % 1000).toInt * 1000000 + 1000000000 + 1
      ),
      List[Long](Long.MinValue + 1,
                 0,
                 ((Long.MinValue + 1) / 1000) - 1,
                 ((Long.MinValue + 1) % 1000).toInt * 1000000 + 1000000000
      ),
      List[Long](Long.MinValue + 1,
                 1,
                 ((Long.MinValue + 1) / 1000) - 1,
                 ((Long.MinValue + 1) % 1000).toInt * 1000000 + 1000000000 + 1
      )
    )

  test("factory_millis_long") {
    provider_factory_millis_long.foreach {
      case millis :: nanos :: expectedSeconds :: expectedNanoOfSecond :: Nil =>
        val t: Instant = Instant.ofEpochMilli(millis).plusNanos(nanos)
        assertEquals(t.getEpochSecond, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
        assertEquals(t.toEpochMilli, millis)
      case _                                                                 =>
        fail()
    }
  }

  def provider_factory_parse: List[List[Any]] =
    List(
      List("1970-01-01T00:00:00Z", 0, 0),
      List("1970-01-01t00:00:00Z", 0, 0),
      List("1970-01-01T00:00:00z", 0, 0),
      List("1970-01-01T00:00:00.0Z", 0, 0),
      List("1970-01-01T00:00:00.000000000Z", 0, 0),
      List("1970-01-01T00:00:00.000000001Z", 0, 1),
      List("1970-01-01T00:00:00.100000000Z", 0, 100000000),
      List("1970-01-01T00:00:01Z", 1, 0),
      List("1970-01-01T00:01:00Z", 60, 0),
      List("1970-01-01T00:01:01Z", 61, 0),
      List("1970-01-01T00:01:01.000000001Z", 61, 1),
      List("1970-01-01T01:00:00.000000000Z", 3600, 0),
      List("1970-01-01T01:01:01.000000001Z", 3661, 1),
      List("1970-01-02T01:01:01.100000000Z", 90061, 100000000)
    )

  test("factory_parse") {
    provider_factory_parse.foreach {
      case (text: String) :: expectedEpochSeconds :: expectedNanoOfSecond :: Nil =>
        val t: Instant = Instant.parse(text)
        assertEquals(t.getEpochSecond, expectedEpochSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                     =>
        fail()
    }
  }

  test("factory_parseLowercase") {
    provider_factory_parse.foreach {
      case (text: String) :: expectedEpochSeconds :: expectedNanoOfSecond :: Nil =>
        val t: Instant = Instant.parse(text.toLowerCase)
        assertEquals(t.getEpochSecond, expectedEpochSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                     =>
        fail()
    }
  }

  def provider_factory_parseFailures: List[List[AnyRef]] =
    List(List(""),
         List("Z"),
         List("1970-01-01T00:00:00"),
         List("1970-01-01T00:00:0Z"),
         List("1970-01-01T00:00:00.0000000000Z")
    )

  test("factory_parseFailures") {
    provider_factory_parseFailures.foreach {
      case (text: String) :: Nil =>
        assertThrows[DateTimeParseException] {
          Instant.parse(text)
        }
      case _                     =>
        fail()
    }
  }

  test("factory_parseFailures_comma") {
    provider_factory_parseFailures.foreach {
      case (text: String) :: Nil =>
        assertThrows[DateTimeParseException] {
          val _text = text.replace('.', ',')
          Instant.parse(_text)
        }
      case _                     =>
        fail()
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      Instant.parse(null)
    }
  }

  test("get_TemporalField") {
    val test: Instant = TEST_12345_123456789
    assertEquals(test.get(ChronoField.NANO_OF_SECOND), 123456789)
    assertEquals(test.get(ChronoField.MICRO_OF_SECOND), 123456)
    assertEquals(test.get(ChronoField.MILLI_OF_SECOND), 123)
  }

  test("getLong_TemporalField") {
    val test: Instant = TEST_12345_123456789
    assertEquals(test.getLong(ChronoField.NANO_OF_SECOND), 123456789)
    assertEquals(test.getLong(ChronoField.MICRO_OF_SECOND), 123456)
    assertEquals(test.getLong(ChronoField.MILLI_OF_SECOND), 123)
    assertEquals(test.getLong(ChronoField.INSTANT_SECONDS), 12345)
  }

  test("query") {
    assertEquals(TEST_12345_123456789.query(TemporalQueries.chronology), null)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.localDate), null)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.localTime), null)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.offset), null)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.precision), ChronoUnit.NANOS)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.zone), null)
    assertEquals(TEST_12345_123456789.query(TemporalQueries.zoneId), null)
  }

  test("query_null") {
    assertThrows[Platform.NPE] {
      TEST_12345_123456789.query(null)
    }
  }

  def provider_plus: List[List[Long]] = {
    List(
      List(TestInstant.MIN_SECOND, 0, -TestInstant.MIN_SECOND, 0, 0, 0),
      List(TestInstant.MIN_SECOND, 0, 1, 0, TestInstant.MIN_SECOND + 1, 0),
      List(TestInstant.MIN_SECOND, 0, 0, 500, TestInstant.MIN_SECOND, 500),
      List(TestInstant.MIN_SECOND, 0, 0, 1000000000, TestInstant.MIN_SECOND + 1, 0),
      List(TestInstant.MIN_SECOND + 1, 0, -1, 0, TestInstant.MIN_SECOND, 0),
      List(TestInstant.MIN_SECOND + 1, 0, 0, -500, TestInstant.MIN_SECOND, 999999500),
      List(TestInstant.MIN_SECOND + 1, 0, 0, -1000000000, TestInstant.MIN_SECOND, 0),
      List(-4, 666666667, -4, 666666667, -7, 333333334),
      List(-4, 666666667, -3, 0, -7, 666666667),
      List(-4, 666666667, -2, 0, -6, 666666667),
      List(-4, 666666667, -1, 0, -5, 666666667),
      List(-4, 666666667, -1, 333333334, -4, 1),
      List(-4, 666666667, -1, 666666667, -4, 333333334),
      List(-4, 666666667, -1, 999999999, -4, 666666666),
      List(-4, 666666667, 0, 0, -4, 666666667),
      List(-4, 666666667, 0, 1, -4, 666666668),
      List(-4, 666666667, 0, 333333333, -3, 0),
      List(-4, 666666667, 0, 666666666, -3, 333333333),
      List(-4, 666666667, 1, 0, -3, 666666667),
      List(-4, 666666667, 2, 0, -2, 666666667),
      List(-4, 666666667, 3, 0, -1, 666666667),
      List(-4, 666666667, 3, 333333333, 0, 0),
      List(-3, 0, -4, 666666667, -7, 666666667),
      List(-3, 0, -3, 0, -6, 0),
      List(-3, 0, -2, 0, -5, 0),
      List(-3, 0, -1, 0, -4, 0),
      List(-3, 0, -1, 333333334, -4, 333333334),
      List(-3, 0, -1, 666666667, -4, 666666667),
      List(-3, 0, -1, 999999999, -4, 999999999),
      List(-3, 0, 0, 0, -3, 0),
      List(-3, 0, 0, 1, -3, 1),
      List(-3, 0, 0, 333333333, -3, 333333333),
      List(-3, 0, 0, 666666666, -3, 666666666),
      List(-3, 0, 1, 0, -2, 0),
      List(-3, 0, 2, 0, -1, 0),
      List(-3, 0, 3, 0, 0, 0),
      List(-3, 0, 3, 333333333, 0, 333333333),
      List(-2, 0, -4, 666666667, -6, 666666667),
      List(-2, 0, -3, 0, -5, 0),
      List(-2, 0, -2, 0, -4, 0),
      List(-2, 0, -1, 0, -3, 0),
      List(-2, 0, -1, 333333334, -3, 333333334),
      List(-2, 0, -1, 666666667, -3, 666666667),
      List(-2, 0, -1, 999999999, -3, 999999999),
      List(-2, 0, 0, 0, -2, 0),
      List(-2, 0, 0, 1, -2, 1),
      List(-2, 0, 0, 333333333, -2, 333333333),
      List(-2, 0, 0, 666666666, -2, 666666666),
      List(-2, 0, 1, 0, -1, 0),
      List(-2, 0, 2, 0, 0, 0),
      List(-2, 0, 3, 0, 1, 0),
      List(-2, 0, 3, 333333333, 1, 333333333),
      List(-1, 0, -4, 666666667, -5, 666666667),
      List(-1, 0, -3, 0, -4, 0),
      List(-1, 0, -2, 0, -3, 0),
      List(-1, 0, -1, 0, -2, 0),
      List(-1, 0, -1, 333333334, -2, 333333334),
      List(-1, 0, -1, 666666667, -2, 666666667),
      List(-1, 0, -1, 999999999, -2, 999999999),
      List(-1, 0, 0, 0, -1, 0),
      List(-1, 0, 0, 1, -1, 1),
      List(-1, 0, 0, 333333333, -1, 333333333),
      List(-1, 0, 0, 666666666, -1, 666666666),
      List(-1, 0, 1, 0, 0, 0),
      List(-1, 0, 2, 0, 1, 0),
      List(-1, 0, 3, 0, 2, 0),
      List(-1, 0, 3, 333333333, 2, 333333333),
      List(-1, 666666667, -4, 666666667, -4, 333333334),
      List(-1, 666666667, -3, 0, -4, 666666667),
      List(-1, 666666667, -2, 0, -3, 666666667),
      List(-1, 666666667, -1, 0, -2, 666666667),
      List(-1, 666666667, -1, 333333334, -1, 1),
      List(-1, 666666667, -1, 666666667, -1, 333333334),
      List(-1, 666666667, -1, 999999999, -1, 666666666),
      List(-1, 666666667, 0, 0, -1, 666666667),
      List(-1, 666666667, 0, 1, -1, 666666668),
      List(-1, 666666667, 0, 333333333, 0, 0),
      List(-1, 666666667, 0, 666666666, 0, 333333333),
      List(-1, 666666667, 1, 0, 0, 666666667),
      List(-1, 666666667, 2, 0, 1, 666666667),
      List(-1, 666666667, 3, 0, 2, 666666667),
      List(-1, 666666667, 3, 333333333, 3, 0),
      List(0, 0, -4, 666666667, -4, 666666667),
      List(0, 0, -3, 0, -3, 0),
      List(0, 0, -2, 0, -2, 0),
      List(0, 0, -1, 0, -1, 0),
      List(0, 0, -1, 333333334, -1, 333333334),
      List(0, 0, -1, 666666667, -1, 666666667),
      List(0, 0, -1, 999999999, -1, 999999999),
      List(0, 0, 0, 0, 0, 0),
      List(0, 0, 0, 1, 0, 1),
      List(0, 0, 0, 333333333, 0, 333333333),
      List(0, 0, 0, 666666666, 0, 666666666),
      List(0, 0, 1, 0, 1, 0),
      List(0, 0, 2, 0, 2, 0),
      List(0, 0, 3, 0, 3, 0),
      List(0, 0, 3, 333333333, 3, 333333333),
      List(0, 333333333, -4, 666666667, -3, 0),
      List(0, 333333333, -3, 0, -3, 333333333),
      List(0, 333333333, -2, 0, -2, 333333333),
      List(0, 333333333, -1, 0, -1, 333333333),
      List(0, 333333333, -1, 333333334, -1, 666666667),
      List(0, 333333333, -1, 666666667, 0, 0),
      List(0, 333333333, -1, 999999999, 0, 333333332),
      List(0, 333333333, 0, 0, 0, 333333333),
      List(0, 333333333, 0, 1, 0, 333333334),
      List(0, 333333333, 0, 333333333, 0, 666666666),
      List(0, 333333333, 0, 666666666, 0, 999999999),
      List(0, 333333333, 1, 0, 1, 333333333),
      List(0, 333333333, 2, 0, 2, 333333333),
      List(0, 333333333, 3, 0, 3, 333333333),
      List(0, 333333333, 3, 333333333, 3, 666666666),
      List(1, 0, -4, 666666667, -3, 666666667),
      List(1, 0, -3, 0, -2, 0),
      List(1, 0, -2, 0, -1, 0),
      List(1, 0, -1, 0, 0, 0),
      List(1, 0, -1, 333333334, 0, 333333334),
      List(1, 0, -1, 666666667, 0, 666666667),
      List(1, 0, -1, 999999999, 0, 999999999),
      List(1, 0, 0, 0, 1, 0),
      List(1, 0, 0, 1, 1, 1),
      List(1, 0, 0, 333333333, 1, 333333333),
      List(1, 0, 0, 666666666, 1, 666666666),
      List(1, 0, 1, 0, 2, 0),
      List(1, 0, 2, 0, 3, 0),
      List(1, 0, 3, 0, 4, 0),
      List(1, 0, 3, 333333333, 4, 333333333),
      List(2, 0, -4, 666666667, -2, 666666667),
      List(2, 0, -3, 0, -1, 0),
      List(2, 0, -2, 0, 0, 0),
      List(2, 0, -1, 0, 1, 0),
      List(2, 0, -1, 333333334, 1, 333333334),
      List(2, 0, -1, 666666667, 1, 666666667),
      List(2, 0, -1, 999999999, 1, 999999999),
      List(2, 0, 0, 0, 2, 0),
      List(2, 0, 0, 1, 2, 1),
      List(2, 0, 0, 333333333, 2, 333333333),
      List(2, 0, 0, 666666666, 2, 666666666),
      List(2, 0, 1, 0, 3, 0),
      List(2, 0, 2, 0, 4, 0),
      List(2, 0, 3, 0, 5, 0),
      List(2, 0, 3, 333333333, 5, 333333333),
      List(3, 0, -4, 666666667, -1, 666666667),
      List(3, 0, -3, 0, 0, 0),
      List(3, 0, -2, 0, 1, 0),
      List(3, 0, -1, 0, 2, 0),
      List(3, 0, -1, 333333334, 2, 333333334),
      List(3, 0, -1, 666666667, 2, 666666667),
      List(3, 0, -1, 999999999, 2, 999999999),
      List(3, 0, 0, 0, 3, 0),
      List(3, 0, 0, 1, 3, 1),
      List(3, 0, 0, 333333333, 3, 333333333),
      List(3, 0, 0, 666666666, 3, 666666666),
      List(3, 0, 1, 0, 4, 0),
      List(3, 0, 2, 0, 5, 0),
      List(3, 0, 3, 0, 6, 0),
      List(3, 0, 3, 333333333, 6, 333333333),
      List(3, 333333333, -4, 666666667, 0, 0),
      List(3, 333333333, -3, 0, 0, 333333333),
      List(3, 333333333, -2, 0, 1, 333333333),
      List(3, 333333333, -1, 0, 2, 333333333),
      List(3, 333333333, -1, 333333334, 2, 666666667),
      List(3, 333333333, -1, 666666667, 3, 0),
      List(3, 333333333, -1, 999999999, 3, 333333332),
      List(3, 333333333, 0, 0, 3, 333333333),
      List(3, 333333333, 0, 1, 3, 333333334),
      List(3, 333333333, 0, 333333333, 3, 666666666),
      List(3, 333333333, 0, 666666666, 3, 999999999),
      List(3, 333333333, 1, 0, 4, 333333333),
      List(3, 333333333, 2, 0, 5, 333333333),
      List(3, 333333333, 3, 0, 6, 333333333),
      List(3, 333333333, 3, 333333333, 6, 666666666),
      List(TestInstant.MAX_SECOND - 1, 0, 1, 0, TestInstant.MAX_SECOND, 0),
      List(TestInstant.MAX_SECOND - 1, 0, 0, 500, TestInstant.MAX_SECOND - 1, 500),
      List(TestInstant.MAX_SECOND - 1, 0, 0, 1000000000, TestInstant.MAX_SECOND, 0),
      List(TestInstant.MAX_SECOND, 0, -1, 0, TestInstant.MAX_SECOND - 1, 0),
      List(TestInstant.MAX_SECOND, 0, 0, -500, TestInstant.MAX_SECOND - 1, 999999500),
      List(TestInstant.MAX_SECOND, 0, 0, -1000000000, TestInstant.MAX_SECOND - 1, 0),
      List(TestInstant.MAX_SECOND, 0, -TestInstant.MAX_SECOND, 0, 0, 0)
    )
  }

  test("plus_Duration") {
    provider_plus.foreach {
      case seconds :: nanos :: otherSeconds :: otherNanos :: expectedSeconds :: expectedNanoOfSecond :: Nil =>
        val i: Instant =
          Instant.ofEpochSecond(seconds, nanos).plus(Duration.ofSeconds(otherSeconds, otherNanos))
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                =>
        fail()
    }
  }

  test("plus_Duration_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      i.plus(Duration.ofSeconds(0, 1))
    }
  }

  test("plus_Duration_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND)
      i.plus(Duration.ofSeconds(-1, 999999999))
    }
  }

  test("plus_longTemporalUnit") {
    provider_plus.foreach {
      case seconds :: nanos :: otherSeconds :: otherNanos :: expectedSeconds :: expectedNanoOfSecond :: Nil =>
        val i: Instant =
          Instant.ofEpochSecond(seconds, nanos).plus(otherSeconds, SECONDS).plus(otherNanos, NANOS)
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                =>
        fail()
    }
  }

  test("plus_longTemporalUnit_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      i.plus(1, NANOS)
    }
  }

  test("plus_longTemporalUnit_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND)
      i.plus(999999999, NANOS)
      i.plus(-1, SECONDS)
    }
  }

  def provider_plusSeconds_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, 1, 0),
      List(0, 0, -1, -1, 0),
      List(0, 0, TestInstant.MAX_SECOND, TestInstant.MAX_SECOND, 0),
      List(0, 0, TestInstant.MIN_SECOND, TestInstant.MIN_SECOND, 0),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 2, 0),
      List(1, 0, -1, 0, 0),
      List(1, 0, TestInstant.MAX_SECOND - 1, TestInstant.MAX_SECOND, 0),
      List(1, 0, TestInstant.MIN_SECOND, TestInstant.MIN_SECOND + 1, 0),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 2, 1),
      List(1, 1, -1, 0, 1),
      List(1, 1, TestInstant.MAX_SECOND - 1, TestInstant.MAX_SECOND, 1),
      List(1, 1, TestInstant.MIN_SECOND, TestInstant.MIN_SECOND + 1, 1),
      List(-1, 1, 0, -1, 1),
      List(-1, 1, 1, 0, 1),
      List(-1, 1, -1, -2, 1),
      List(-1, 1, TestInstant.MAX_SECOND, TestInstant.MAX_SECOND - 1, 1),
      List(-1, 1, TestInstant.MIN_SECOND + 1, TestInstant.MIN_SECOND, 1),
      List(TestInstant.MAX_SECOND, 2, -TestInstant.MAX_SECOND, 0, 2),
      List(TestInstant.MIN_SECOND, 2, -TestInstant.MIN_SECOND, 0, 2)
    )

  test("plusSeconds_long") {
    provider_plusSeconds_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Instant = Instant.ofEpochSecond(seconds, nanos)
        t = t.plusSeconds(amount)
        assertEquals(t.getEpochSecond, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusSeconds_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Instant = Instant.ofEpochSecond(1, 0)
      t.plusSeconds(Long.MaxValue)
    }
  }

  test("plusSeconds_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Instant = Instant.ofEpochSecond(-1, 0)
      t.plusSeconds(Long.MinValue)
    }
  }

  def provider_plusMillis_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, 0, 1000000),
      List(0, 0, 999, 0, 999000000),
      List(0, 0, 1000, 1, 0),
      List(0, 0, 1001, 1, 1000000),
      List(0, 0, 1999, 1, 999000000),
      List(0, 0, 2000, 2, 0),
      List(0, 0, -1, -1, 999000000),
      List(0, 0, -999, -1, 1000000),
      List(0, 0, -1000, -1, 0),
      List(0, 0, -1001, -2, 999000000),
      List(0, 0, -1999, -2, 1000000),
      List(0, 1, 0, 0, 1),
      List(0, 1, 1, 0, 1000001),
      List(0, 1, 998, 0, 998000001),
      List(0, 1, 999, 0, 999000001),
      List(0, 1, 1000, 1, 1),
      List(0, 1, 1998, 1, 998000001),
      List(0, 1, 1999, 1, 999000001),
      List(0, 1, 2000, 2, 1),
      List(0, 1, -1, -1, 999000001),
      List(0, 1, -2, -1, 998000001),
      List(0, 1, -1000, -1, 1),
      List(0, 1, -1001, -2, 999000001),
      List(0, 1000000, 0, 0, 1000000),
      List(0, 1000000, 1, 0, 2000000),
      List(0, 1000000, 998, 0, 999000000),
      List(0, 1000000, 999, 1, 0),
      List(0, 1000000, 1000, 1, 1000000),
      List(0, 1000000, 1998, 1, 999000000),
      List(0, 1000000, 1999, 2, 0),
      List(0, 1000000, 2000, 2, 1000000),
      List(0, 1000000, -1, 0, 0),
      List(0, 1000000, -2, -1, 999000000),
      List(0, 1000000, -999, -1, 2000000),
      List(0, 1000000, -1000, -1, 1000000),
      List(0, 1000000, -1001, -1, 0),
      List(0, 1000000, -1002, -2, 999000000),
      List(0, 999999999, 0, 0, 999999999),
      List(0, 999999999, 1, 1, 999999),
      List(0, 999999999, 999, 1, 998999999),
      List(0, 999999999, 1000, 1, 999999999),
      List(0, 999999999, 1001, 2, 999999),
      List(0, 999999999, -1, 0, 998999999),
      List(0, 999999999, -1000, -1, 999999999),
      List(0, 999999999, -1001, -1, 998999999),
      List(0, 0, Long.MaxValue, Long.MaxValue / 1000, (Long.MaxValue % 1000).toInt * 1000000),
      List(0,
           0,
           Long.MinValue,
           Long.MinValue / 1000 - 1,
           (Long.MinValue % 1000).toInt * 1000000 + 1000000000
      )
    )

  test("plusMillis_long") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Instant = Instant.ofEpochSecond(seconds, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getEpochSecond, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_oneMore") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Instant = Instant.ofEpochSecond(seconds + 1, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getEpochSecond, expectedSeconds + 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_minusOneLess") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Instant = Instant.ofEpochSecond(seconds - 1, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getEpochSecond, expectedSeconds - 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_max") {
    var t: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 998999999)
    t = t.plusMillis(1)
    assertEquals(t.getEpochSecond, TestInstant.MAX_SECOND)
    assertEquals(t.getNano, 999999999)
  }

  test("plusMillis_long_overflowTooBig") {
    assertThrows[DateTimeException] {
      val t: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999000000)
      t.plusMillis(1)
    }
  }

  test("plusMillis_long_min") {
    var t: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 1000000)
    t = t.plusMillis(-1)
    assertEquals(t.getEpochSecond, TestInstant.MIN_SECOND)
    assertEquals(t.getNano, 0)
  }

  test("plusMillis_long_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val t: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 0)
      t.plusMillis(-1)
    }
  }

  def provider_plusNanos_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, 0, 1),
      List(0, 0, 999999999, 0, 999999999),
      List(0, 0, 1000000000, 1, 0),
      List(0, 0, 1000000001, 1, 1),
      List(0, 0, 1999999999, 1, 999999999),
      List(0, 0, 2000000000, 2, 0),
      List(0, 0, -1, -1, 999999999),
      List(0, 0, -999999999, -1, 1),
      List(0, 0, -1000000000, -1, 0),
      List(0, 0, -1000000001, -2, 999999999),
      List(0, 0, -1999999999, -2, 1),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 1, 1),
      List(1, 0, 999999999, 1, 999999999),
      List(1, 0, 1000000000, 2, 0),
      List(1, 0, 1000000001, 2, 1),
      List(1, 0, 1999999999, 2, 999999999),
      List(1, 0, 2000000000, 3, 0),
      List(1, 0, -1, 0, 999999999),
      List(1, 0, -999999999, 0, 1),
      List(1, 0, -1000000000, 0, 0),
      List(1, 0, -1000000001, -1, 999999999),
      List(1, 0, -1999999999, -1, 1),
      List(-1, 0, 0, -1, 0),
      List(-1, 0, 1, -1, 1),
      List(-1, 0, 999999999, -1, 999999999),
      List(-1, 0, 1000000000, 0, 0),
      List(-1, 0, 1000000001, 0, 1),
      List(-1, 0, 1999999999, 0, 999999999),
      List(-1, 0, 2000000000, 1, 0),
      List(-1, 0, -1, -2, 999999999),
      List(-1, 0, -999999999, -2, 1),
      List(-1, 0, -1000000000, -2, 0),
      List(-1, 0, -1000000001, -3, 999999999),
      List(-1, 0, -1999999999, -3, 1),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 1, 2),
      List(1, 1, 999999998, 1, 999999999),
      List(1, 1, 999999999, 2, 0),
      List(1, 1, 1000000000, 2, 1),
      List(1, 1, 1999999998, 2, 999999999),
      List(1, 1, 1999999999, 3, 0),
      List(1, 1, 2000000000, 3, 1),
      List(1, 1, -1, 1, 0),
      List(1, 1, -2, 0, 999999999),
      List(1, 1, -1000000000, 0, 1),
      List(1, 1, -1000000001, 0, 0),
      List(1, 1, -1000000002, -1, 999999999),
      List(1, 1, -2000000000, -1, 1),
      List(1, 999999999, 0, 1, 999999999),
      List(1, 999999999, 1, 2, 0),
      List(1, 999999999, 999999999, 2, 999999998),
      List(1, 999999999, 1000000000, 2, 999999999),
      List(1, 999999999, 1000000001, 3, 0),
      List(1, 999999999, -1, 1, 999999998),
      List(1, 999999999, -1000000000, 0, 999999999),
      List(1, 999999999, -1000000001, 0, 999999998),
      List(1, 999999999, -1999999999, 0, 0),
      List(1, 999999999, -2000000000, -1, 999999999),
      List(TestInstant.MAX_SECOND, 0, 999999999, TestInstant.MAX_SECOND, 999999999),
      List(TestInstant.MAX_SECOND - 1, 0, 1999999999, TestInstant.MAX_SECOND, 999999999),
      List(TestInstant.MIN_SECOND, 1, -1, TestInstant.MIN_SECOND, 0),
      List(TestInstant.MIN_SECOND + 1, 1, -1000000001, TestInstant.MIN_SECOND, 0),
      List(0,
           0,
           TestInstant.MAX_SECOND,
           TestInstant.MAX_SECOND / 1000000000,
           (TestInstant.MAX_SECOND % 1000000000).toInt
      ),
      List(0,
           0,
           TestInstant.MIN_SECOND,
           TestInstant.MIN_SECOND / 1000000000 - 1,
           (TestInstant.MIN_SECOND % 1000000000).toInt + 1000000000
      )
    )

  test("plusNanos_long") {
    provider_plusNanos_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Instant = Instant.ofEpochSecond(seconds, nanos)
        t = t.plusNanos(amount)
        assertEquals(t.getEpochSecond, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusNanos_long_overflowTooBig") {
    assertThrows[DateTimeException] {
      val t: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      t.plusNanos(1)
    }
  }

  test("plusNanos_long_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val t: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 0)
      t.plusNanos(-1)
    }
  }

  def provider_minus: List[List[Long]] = {
    List(
      List(TestInstant.MIN_SECOND, 0, TestInstant.MIN_SECOND, 0, 0, 0),
      List(TestInstant.MIN_SECOND, 0, -1, 0, TestInstant.MIN_SECOND + 1, 0),
      List(TestInstant.MIN_SECOND, 0, 0, -500, TestInstant.MIN_SECOND, 500),
      List(TestInstant.MIN_SECOND, 0, 0, -1000000000, TestInstant.MIN_SECOND + 1, 0),
      List(TestInstant.MIN_SECOND + 1, 0, 1, 0, TestInstant.MIN_SECOND, 0),
      List(TestInstant.MIN_SECOND + 1, 0, 0, 500, TestInstant.MIN_SECOND, 999999500),
      List(TestInstant.MIN_SECOND + 1, 0, 0, 1000000000, TestInstant.MIN_SECOND, 0),
      List(-4, 666666667, -4, 666666667, 0, 0),
      List(-4, 666666667, -3, 0, -1, 666666667),
      List(-4, 666666667, -2, 0, -2, 666666667),
      List(-4, 666666667, -1, 0, -3, 666666667),
      List(-4, 666666667, -1, 333333334, -3, 333333333),
      List(-4, 666666667, -1, 666666667, -3, 0),
      List(-4, 666666667, -1, 999999999, -4, 666666668),
      List(-4, 666666667, 0, 0, -4, 666666667),
      List(-4, 666666667, 0, 1, -4, 666666666),
      List(-4, 666666667, 0, 333333333, -4, 333333334),
      List(-4, 666666667, 0, 666666666, -4, 1),
      List(-4, 666666667, 1, 0, -5, 666666667),
      List(-4, 666666667, 2, 0, -6, 666666667),
      List(-4, 666666667, 3, 0, -7, 666666667),
      List(-4, 666666667, 3, 333333333, -7, 333333334),
      List(-3, 0, -4, 666666667, 0, 333333333),
      List(-3, 0, -3, 0, 0, 0),
      List(-3, 0, -2, 0, -1, 0),
      List(-3, 0, -1, 0, -2, 0),
      List(-3, 0, -1, 333333334, -3, 666666666),
      List(-3, 0, -1, 666666667, -3, 333333333),
      List(-3, 0, -1, 999999999, -3, 1),
      List(-3, 0, 0, 0, -3, 0),
      List(-3, 0, 0, 1, -4, 999999999),
      List(-3, 0, 0, 333333333, -4, 666666667),
      List(-3, 0, 0, 666666666, -4, 333333334),
      List(-3, 0, 1, 0, -4, 0),
      List(-3, 0, 2, 0, -5, 0),
      List(-3, 0, 3, 0, -6, 0),
      List(-3, 0, 3, 333333333, -7, 666666667),
      List(-2, 0, -4, 666666667, 1, 333333333),
      List(-2, 0, -3, 0, 1, 0),
      List(-2, 0, -2, 0, 0, 0),
      List(-2, 0, -1, 0, -1, 0),
      List(-2, 0, -1, 333333334, -2, 666666666),
      List(-2, 0, -1, 666666667, -2, 333333333),
      List(-2, 0, -1, 999999999, -2, 1),
      List(-2, 0, 0, 0, -2, 0),
      List(-2, 0, 0, 1, -3, 999999999),
      List(-2, 0, 0, 333333333, -3, 666666667),
      List(-2, 0, 0, 666666666, -3, 333333334),
      List(-2, 0, 1, 0, -3, 0),
      List(-2, 0, 2, 0, -4, 0),
      List(-2, 0, 3, 0, -5, 0),
      List(-2, 0, 3, 333333333, -6, 666666667),
      List(-1, 0, -4, 666666667, 2, 333333333),
      List(-1, 0, -3, 0, 2, 0),
      List(-1, 0, -2, 0, 1, 0),
      List(-1, 0, -1, 0, 0, 0),
      List(-1, 0, -1, 333333334, -1, 666666666),
      List(-1, 0, -1, 666666667, -1, 333333333),
      List(-1, 0, -1, 999999999, -1, 1),
      List(-1, 0, 0, 0, -1, 0),
      List(-1, 0, 0, 1, -2, 999999999),
      List(-1, 0, 0, 333333333, -2, 666666667),
      List(-1, 0, 0, 666666666, -2, 333333334),
      List(-1, 0, 1, 0, -2, 0),
      List(-1, 0, 2, 0, -3, 0),
      List(-1, 0, 3, 0, -4, 0),
      List(-1, 0, 3, 333333333, -5, 666666667),
      List(-1, 666666667, -4, 666666667, 3, 0),
      List(-1, 666666667, -3, 0, 2, 666666667),
      List(-1, 666666667, -2, 0, 1, 666666667),
      List(-1, 666666667, -1, 0, 0, 666666667),
      List(-1, 666666667, -1, 333333334, 0, 333333333),
      List(-1, 666666667, -1, 666666667, 0, 0),
      List(-1, 666666667, -1, 999999999, -1, 666666668),
      List(-1, 666666667, 0, 0, -1, 666666667),
      List(-1, 666666667, 0, 1, -1, 666666666),
      List(-1, 666666667, 0, 333333333, -1, 333333334),
      List(-1, 666666667, 0, 666666666, -1, 1),
      List(-1, 666666667, 1, 0, -2, 666666667),
      List(-1, 666666667, 2, 0, -3, 666666667),
      List(-1, 666666667, 3, 0, -4, 666666667),
      List(-1, 666666667, 3, 333333333, -4, 333333334),
      List(0, 0, -4, 666666667, 3, 333333333),
      List(0, 0, -3, 0, 3, 0),
      List(0, 0, -2, 0, 2, 0),
      List(0, 0, -1, 0, 1, 0),
      List(0, 0, -1, 333333334, 0, 666666666),
      List(0, 0, -1, 666666667, 0, 333333333),
      List(0, 0, -1, 999999999, 0, 1),
      List(0, 0, 0, 0, 0, 0),
      List(0, 0, 0, 1, -1, 999999999),
      List(0, 0, 0, 333333333, -1, 666666667),
      List(0, 0, 0, 666666666, -1, 333333334),
      List(0, 0, 1, 0, -1, 0),
      List(0, 0, 2, 0, -2, 0),
      List(0, 0, 3, 0, -3, 0),
      List(0, 0, 3, 333333333, -4, 666666667),
      List(0, 333333333, -4, 666666667, 3, 666666666),
      List(0, 333333333, -3, 0, 3, 333333333),
      List(0, 333333333, -2, 0, 2, 333333333),
      List(0, 333333333, -1, 0, 1, 333333333),
      List(0, 333333333, -1, 333333334, 0, 999999999),
      List(0, 333333333, -1, 666666667, 0, 666666666),
      List(0, 333333333, -1, 999999999, 0, 333333334),
      List(0, 333333333, 0, 0, 0, 333333333),
      List(0, 333333333, 0, 1, 0, 333333332),
      List(0, 333333333, 0, 333333333, 0, 0),
      List(0, 333333333, 0, 666666666, -1, 666666667),
      List(0, 333333333, 1, 0, -1, 333333333),
      List(0, 333333333, 2, 0, -2, 333333333),
      List(0, 333333333, 3, 0, -3, 333333333),
      List(0, 333333333, 3, 333333333, -3, 0),
      List(1, 0, -4, 666666667, 4, 333333333),
      List(1, 0, -3, 0, 4, 0),
      List(1, 0, -2, 0, 3, 0),
      List(1, 0, -1, 0, 2, 0),
      List(1, 0, -1, 333333334, 1, 666666666),
      List(1, 0, -1, 666666667, 1, 333333333),
      List(1, 0, -1, 999999999, 1, 1),
      List(1, 0, 0, 0, 1, 0),
      List(1, 0, 0, 1, 0, 999999999),
      List(1, 0, 0, 333333333, 0, 666666667),
      List(1, 0, 0, 666666666, 0, 333333334),
      List(1, 0, 1, 0, 0, 0),
      List(1, 0, 2, 0, -1, 0),
      List(1, 0, 3, 0, -2, 0),
      List(1, 0, 3, 333333333, -3, 666666667),
      List(2, 0, -4, 666666667, 5, 333333333),
      List(2, 0, -3, 0, 5, 0),
      List(2, 0, -2, 0, 4, 0),
      List(2, 0, -1, 0, 3, 0),
      List(2, 0, -1, 333333334, 2, 666666666),
      List(2, 0, -1, 666666667, 2, 333333333),
      List(2, 0, -1, 999999999, 2, 1),
      List(2, 0, 0, 0, 2, 0),
      List(2, 0, 0, 1, 1, 999999999),
      List(2, 0, 0, 333333333, 1, 666666667),
      List(2, 0, 0, 666666666, 1, 333333334),
      List(2, 0, 1, 0, 1, 0),
      List(2, 0, 2, 0, 0, 0),
      List(2, 0, 3, 0, -1, 0),
      List(2, 0, 3, 333333333, -2, 666666667),
      List(3, 0, -4, 666666667, 6, 333333333),
      List(3, 0, -3, 0, 6, 0),
      List(3, 0, -2, 0, 5, 0),
      List(3, 0, -1, 0, 4, 0),
      List(3, 0, -1, 333333334, 3, 666666666),
      List(3, 0, -1, 666666667, 3, 333333333),
      List(3, 0, -1, 999999999, 3, 1),
      List(3, 0, 0, 0, 3, 0),
      List(3, 0, 0, 1, 2, 999999999),
      List(3, 0, 0, 333333333, 2, 666666667),
      List(3, 0, 0, 666666666, 2, 333333334),
      List(3, 0, 1, 0, 2, 0),
      List(3, 0, 2, 0, 1, 0),
      List(3, 0, 3, 0, 0, 0),
      List(3, 0, 3, 333333333, -1, 666666667),
      List(3, 333333333, -4, 666666667, 6, 666666666),
      List(3, 333333333, -3, 0, 6, 333333333),
      List(3, 333333333, -2, 0, 5, 333333333),
      List(3, 333333333, -1, 0, 4, 333333333),
      List(3, 333333333, -1, 333333334, 3, 999999999),
      List(3, 333333333, -1, 666666667, 3, 666666666),
      List(3, 333333333, -1, 999999999, 3, 333333334),
      List(3, 333333333, 0, 0, 3, 333333333),
      List(3, 333333333, 0, 1, 3, 333333332),
      List(3, 333333333, 0, 333333333, 3, 0),
      List(3, 333333333, 0, 666666666, 2, 666666667),
      List(3, 333333333, 1, 0, 2, 333333333),
      List(3, 333333333, 2, 0, 1, 333333333),
      List(3, 333333333, 3, 0, 0, 333333333),
      List(3, 333333333, 3, 333333333, 0, 0),
      List(TestInstant.MAX_SECOND - 1, 0, -1, 0, TestInstant.MAX_SECOND, 0),
      List(TestInstant.MAX_SECOND - 1, 0, 0, -500, TestInstant.MAX_SECOND - 1, 500),
      List(TestInstant.MAX_SECOND - 1, 0, 0, -1000000000, TestInstant.MAX_SECOND, 0),
      List(TestInstant.MAX_SECOND, 0, 1, 0, TestInstant.MAX_SECOND - 1, 0),
      List(TestInstant.MAX_SECOND, 0, 0, 500, TestInstant.MAX_SECOND - 1, 999999500),
      List(TestInstant.MAX_SECOND, 0, 0, 1000000000, TestInstant.MAX_SECOND - 1, 0),
      List(TestInstant.MAX_SECOND, 0, TestInstant.MAX_SECOND, 0, 0, 0)
    )
  }

  test("minus_Duration") {
    provider_minus.foreach {
      case (seconds: Long) :: (nanos: Long) :: (otherSeconds: Long) :: (otherNanos: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val i: Instant =
          Instant.ofEpochSecond(seconds, nanos).minus(Duration.ofSeconds(otherSeconds, otherNanos))
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                                                =>
        fail()
    }
  }

  test("minus_Duration_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND)
      i.minus(Duration.ofSeconds(0, 1))
    }
  }

  test("minus_Duration_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      i.minus(Duration.ofSeconds(-1, 999999999))
    }
  }

  test("minus_longTemporalUnit") {
    provider_minus.foreach {
      case (seconds: Long) :: (nanos: Long) :: (otherSeconds: Long) :: (otherNanos: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val i: Instant = Instant
          .ofEpochSecond(seconds, nanos)
          .minus(otherSeconds, SECONDS)
          .minus(otherNanos, NANOS)
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                                                =>
        fail()
    }
  }

  test("minus_longTemporalUnit_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND)
      i.minus(1, NANOS)
    }
  }

  test("minus_longTemporalUnit_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      i.minus(999999999, NANOS)
      i.minus(-1, SECONDS)
    }
  }

  def provider_minusSeconds_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, -1, 0),
      List(0, 0, -1, 1, 0),
      List(0, 0, -TestInstant.MIN_SECOND, TestInstant.MIN_SECOND, 0),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 0, 0),
      List(1, 0, -1, 2, 0),
      List(1, 0, -TestInstant.MIN_SECOND + 1, TestInstant.MIN_SECOND, 0),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 0, 1),
      List(1, 1, -1, 2, 1),
      List(1, 1, -TestInstant.MIN_SECOND, TestInstant.MIN_SECOND + 1, 1),
      List(1, 1, -TestInstant.MIN_SECOND + 1, TestInstant.MIN_SECOND, 1),
      List(-1, 1, 0, -1, 1),
      List(-1, 1, 1, -2, 1),
      List(-1, 1, -1, 0, 1),
      List(-1, 1, -TestInstant.MAX_SECOND, TestInstant.MAX_SECOND - 1, 1),
      List(-1, 1, -(TestInstant.MAX_SECOND + 1), TestInstant.MAX_SECOND, 1),
      List(TestInstant.MIN_SECOND, 2, TestInstant.MIN_SECOND, 0, 2),
      List(TestInstant.MIN_SECOND + 1, 2, TestInstant.MIN_SECOND, 1, 2),
      List(TestInstant.MAX_SECOND - 1, 2, TestInstant.MAX_SECOND, -1, 2),
      List(TestInstant.MAX_SECOND, 2, TestInstant.MAX_SECOND, 0, 2)
    )

  test("minusSeconds_long") {
    provider_minusSeconds_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val j: Instant = Instant.ofEpochSecond(seconds, nanos)
        val i          = j.minusSeconds(amount)
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusSeconds_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val i: Instant = Instant.ofEpochSecond(1, 0)
      i.minusSeconds(Long.MinValue + 1)
    }
  }

  test("minusSeconds_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val i: Instant = Instant.ofEpochSecond(-2, 0)
      i.minusSeconds(Long.MaxValue)
    }
  }

  def provider_minusMillis_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, -1, 999000000),
      List(0, 0, 999, -1, 1000000),
      List(0, 0, 1000, -1, 0),
      List(0, 0, 1001, -2, 999000000),
      List(0, 0, 1999, -2, 1000000),
      List(0, 0, 2000, -2, 0),
      List(0, 0, -1, 0, 1000000),
      List(0, 0, -999, 0, 999000000),
      List(0, 0, -1000, 1, 0),
      List(0, 0, -1001, 1, 1000000),
      List(0, 0, -1999, 1, 999000000),
      List(0, 1, 0, 0, 1),
      List(0, 1, 1, -1, 999000001),
      List(0, 1, 998, -1, 2000001),
      List(0, 1, 999, -1, 1000001),
      List(0, 1, 1000, -1, 1),
      List(0, 1, 1998, -2, 2000001),
      List(0, 1, 1999, -2, 1000001),
      List(0, 1, 2000, -2, 1),
      List(0, 1, -1, 0, 1000001),
      List(0, 1, -2, 0, 2000001),
      List(0, 1, -1000, 1, 1),
      List(0, 1, -1001, 1, 1000001),
      List(0, 1000000, 0, 0, 1000000),
      List(0, 1000000, 1, 0, 0),
      List(0, 1000000, 998, -1, 3000000),
      List(0, 1000000, 999, -1, 2000000),
      List(0, 1000000, 1000, -1, 1000000),
      List(0, 1000000, 1998, -2, 3000000),
      List(0, 1000000, 1999, -2, 2000000),
      List(0, 1000000, 2000, -2, 1000000),
      List(0, 1000000, -1, 0, 2000000),
      List(0, 1000000, -2, 0, 3000000),
      List(0, 1000000, -999, 1, 0),
      List(0, 1000000, -1000, 1, 1000000),
      List(0, 1000000, -1001, 1, 2000000),
      List(0, 1000000, -1002, 1, 3000000),
      List(0, 999999999, 0, 0, 999999999),
      List(0, 999999999, 1, 0, 998999999),
      List(0, 999999999, 999, 0, 999999),
      List(0, 999999999, 1000, -1, 999999999),
      List(0, 999999999, 1001, -1, 998999999),
      List(0, 999999999, -1, 1, 999999),
      List(0, 999999999, -1000, 1, 999999999),
      List(0, 999999999, -1001, 2, 999999),
      List(0,
           0,
           Long.MaxValue,
           -(Long.MaxValue / 1000) - 1,
           -(Long.MaxValue % 1000).toInt * 1000000 + 1000000000
      ),
      List(0, 0, Long.MinValue, -(Long.MinValue / 1000), -(Long.MinValue % 1000).toInt * 1000000)
    )

  test("minusMillis_long") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val j: Instant = Instant.ofEpochSecond(seconds, nanos)
        val i          = j.minusMillis(amount)
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_oneMore") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val j: Instant = Instant.ofEpochSecond(seconds + 1, nanos)
        val i          = j.minusMillis(amount)
        assertEquals(i.getEpochSecond, expectedSeconds + 1)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_minusOneLess") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val j: Instant = Instant.ofEpochSecond(seconds - 1, nanos)
        val i          = j.minusMillis(amount)
        assertEquals(i.getEpochSecond, expectedSeconds - 1)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_max") {
    var i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 998999999)
    i = i.minusMillis(-1)
    assertEquals(i.getEpochSecond, TestInstant.MAX_SECOND)
    assertEquals(i.getNano, 999999999)
  }

  test("minusMillis_long_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999000000)
      i.minusMillis(-1)
    }
  }

  test("minusMillis_long_min") {
    var i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 1000000)
    i = i.minusMillis(1)
    assertEquals(i.getEpochSecond, TestInstant.MIN_SECOND)
    assertEquals(i.getNano, 0)
  }

  test("minusMillis_long_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 0)
      i.minusMillis(1)
    }
  }

  def provider_minusNanos_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, -1, 999999999),
      List(0, 0, 999999999, -1, 1),
      List(0, 0, 1000000000, -1, 0),
      List(0, 0, 1000000001, -2, 999999999),
      List(0, 0, 1999999999, -2, 1),
      List(0, 0, 2000000000, -2, 0),
      List(0, 0, -1, 0, 1),
      List(0, 0, -999999999, 0, 999999999),
      List(0, 0, -1000000000, 1, 0),
      List(0, 0, -1000000001, 1, 1),
      List(0, 0, -1999999999, 1, 999999999),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 0, 999999999),
      List(1, 0, 999999999, 0, 1),
      List(1, 0, 1000000000, 0, 0),
      List(1, 0, 1000000001, -1, 999999999),
      List(1, 0, 1999999999, -1, 1),
      List(1, 0, 2000000000, -1, 0),
      List(1, 0, -1, 1, 1),
      List(1, 0, -999999999, 1, 999999999),
      List(1, 0, -1000000000, 2, 0),
      List(1, 0, -1000000001, 2, 1),
      List(1, 0, -1999999999, 2, 999999999),
      List(-1, 0, 0, -1, 0),
      List(-1, 0, 1, -2, 999999999),
      List(-1, 0, 999999999, -2, 1),
      List(-1, 0, 1000000000, -2, 0),
      List(-1, 0, 1000000001, -3, 999999999),
      List(-1, 0, 1999999999, -3, 1),
      List(-1, 0, 2000000000, -3, 0),
      List(-1, 0, -1, -1, 1),
      List(-1, 0, -999999999, -1, 999999999),
      List(-1, 0, -1000000000, 0, 0),
      List(-1, 0, -1000000001, 0, 1),
      List(-1, 0, -1999999999, 0, 999999999),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 1, 0),
      List(1, 1, 999999998, 0, 3),
      List(1, 1, 999999999, 0, 2),
      List(1, 1, 1000000000, 0, 1),
      List(1, 1, 1999999998, -1, 3),
      List(1, 1, 1999999999, -1, 2),
      List(1, 1, 2000000000, -1, 1),
      List(1, 1, -1, 1, 2),
      List(1, 1, -2, 1, 3),
      List(1, 1, -1000000000, 2, 1),
      List(1, 1, -1000000001, 2, 2),
      List(1, 1, -1000000002, 2, 3),
      List(1, 1, -2000000000, 3, 1),
      List(1, 999999999, 0, 1, 999999999),
      List(1, 999999999, 1, 1, 999999998),
      List(1, 999999999, 999999999, 1, 0),
      List(1, 999999999, 1000000000, 0, 999999999),
      List(1, 999999999, 1000000001, 0, 999999998),
      List(1, 999999999, -1, 2, 0),
      List(1, 999999999, -1000000000, 2, 999999999),
      List(1, 999999999, -1000000001, 3, 0),
      List(1, 999999999, -1999999999, 3, 999999998),
      List(1, 999999999, -2000000000, 3, 999999999),
      List(TestInstant.MAX_SECOND, 0, -999999999, TestInstant.MAX_SECOND, 999999999),
      List(TestInstant.MAX_SECOND - 1, 0, -1999999999, TestInstant.MAX_SECOND, 999999999),
      List(TestInstant.MIN_SECOND, 1, 1, TestInstant.MIN_SECOND, 0),
      List(TestInstant.MIN_SECOND + 1, 1, 1000000001, TestInstant.MIN_SECOND, 0),
      List(0,
           0,
           Long.MaxValue,
           -(Long.MaxValue / 1000000000) - 1,
           -(Long.MaxValue % 1000000000).toInt + 1000000000
      ),
      List(0, 0, Long.MinValue, -(Long.MinValue / 1000000000), -(Long.MinValue % 1000000000).toInt)
    )

  test("minusNanos_long") {
    provider_minusNanos_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val j: Instant = Instant.ofEpochSecond(seconds, nanos)
        val i          = j.minusNanos(amount)
        assertEquals(i.getEpochSecond, expectedSeconds)
        assertEquals(i.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusNanos_long_overflowTooBig") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MAX_SECOND, 999999999)
      i.minusNanos(-1)
    }
  }

  test("minusNanos_long_overflowTooSmall") {
    assertThrows[DateTimeException] {
      val i: Instant = Instant.ofEpochSecond(TestInstant.MIN_SECOND, 0)
      i.minusNanos(1)
    }
  }

  test("truncatedTo") {
    assertEquals(Instant.ofEpochSecond(2L, 1000000).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(2L)
    )
    assertEquals(Instant.ofEpochSecond(2L, -1000000).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(1L)
    )
    assertEquals(Instant.ofEpochSecond(0L, -1000000).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(-1L)
    )
    assertEquals(Instant.ofEpochSecond(-1L).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(-1L)
    )
    assertEquals(Instant.ofEpochSecond(-1L, -1000000).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(-2L)
    )
    assertEquals(Instant.ofEpochSecond(-2L).truncatedTo(ChronoUnit.SECONDS),
                 Instant.ofEpochSecond(-2L)
    )
  }

  test("toEpochMilli") {
    assertEquals(Instant.ofEpochSecond(1L, 1000000).toEpochMilli, 1001L)
    assertEquals(Instant.ofEpochSecond(1L, 2000000).toEpochMilli, 1002L)
    assertEquals(Instant.ofEpochSecond(1L, 567).toEpochMilli, 1000L)
    assertEquals(Instant.ofEpochSecond(Long.MaxValue / 1000).toEpochMilli,
                 (Long.MaxValue / 1000) * 1000
    )
    assertEquals(Instant.ofEpochSecond(Long.MinValue / 1000).toEpochMilli,
                 (Long.MinValue / 1000) * 1000
    )
    assertEquals(Instant.ofEpochSecond(0L, -1000000).toEpochMilli, -1L)
    assertEquals(Instant.ofEpochSecond(0L, 1000000).toEpochMilli, 1)
    assertEquals(Instant.ofEpochSecond(0L, 999999).toEpochMilli, 0)
    assertEquals(Instant.ofEpochSecond(0L, 1).toEpochMilli, 0)
    assertEquals(Instant.ofEpochSecond(0L, 0).toEpochMilli, 0)
    assertEquals(Instant.ofEpochSecond(0L, -1).toEpochMilli, -1L)
    assertEquals(Instant.ofEpochSecond(0L, -999999).toEpochMilli, -1L)
    assertEquals(Instant.ofEpochSecond(0L, -1000000).toEpochMilli, -1L)
    assertEquals(Instant.ofEpochSecond(0L, -1000001).toEpochMilli, -2L)
  }

  test("toEpochMilli_tooBig") {
    assertThrows[ArithmeticException] {
      Instant.ofEpochSecond(Long.MaxValue / 1000 + 1).toEpochMilli
    }
  }

  test("toEpochMilli_tooSmall") {
    assertThrows[ArithmeticException] {
      Instant.ofEpochSecond(Long.MinValue / 1000 - 1).toEpochMilli
    }
  }

  test("comparisons") {
    doTest_comparisons_Instant(
      Instant.ofEpochSecond(-2L, 0),
      Instant.ofEpochSecond(-2L, 999999998),
      Instant.ofEpochSecond(-2L, 999999999),
      Instant.ofEpochSecond(-1L, 0),
      Instant.ofEpochSecond(-1L, 1),
      Instant.ofEpochSecond(-1L, 999999998),
      Instant.ofEpochSecond(-1L, 999999999),
      Instant.ofEpochSecond(0L, 0),
      Instant.ofEpochSecond(0L, 1),
      Instant.ofEpochSecond(0L, 2),
      Instant.ofEpochSecond(0L, 999999999),
      Instant.ofEpochSecond(1L, 0),
      Instant.ofEpochSecond(2L, 0)
    )
  }

  private def doTest_comparisons_Instant(instants: Instant*): Unit = {
    var i: Int = 0
    while (i < instants.length) {
      val a: Instant = instants(i)
      var j: Int     = 0
      while (j < instants.length) {
        val b: Instant = instants(j)
        if (i < j) {
          assertEquals(a.compareTo(b) < 0, true, a + " <=> " + b)
          assertEquals(a.isBefore(b), true, a + " <=> " + b)
          assertEquals(a.isAfter(b), false, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else if (i > j) {
          assertEquals(a.compareTo(b) > 0, true, a + " <=> " + b)
          assertEquals(a.isBefore(b), false, a + " <=> " + b)
          assertEquals(a.isAfter(b), true, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else {
          assertEquals(a.compareTo(b), 0, a + " <=> " + b)
          assertEquals(a.isBefore(b), false, a + " <=> " + b)
          assertEquals(a.isAfter(b), false, a + " <=> " + b)
          assertEquals(a == b, true, a + " <=> " + b)
        }
        j += 1
      }
      i += 1
    }
  }

  test("compareTo_ObjectNull") {
    assertThrows[Platform.NPE] {
      val a: Instant = Instant.ofEpochSecond(0L, 0)
      a.compareTo(null)
    }
  }

  test("isBefore_ObjectNull") {
    assertThrows[Platform.NPE] {
      val a: Instant = Instant.ofEpochSecond(0L, 0)
      a.isBefore(null)
    }
  }

  test("isAfter_ObjectNull") {
    assertThrows[Platform.NPE] {
      val a: Instant = Instant.ofEpochSecond(0L, 0)
      a.isAfter(null)
    }
  }

  test("equals") {
    val test5a: Instant = Instant.ofEpochSecond(5L, 20)
    val test5b: Instant = Instant.ofEpochSecond(5L, 20)
    val test5n: Instant = Instant.ofEpochSecond(5L, 30)
    val test6: Instant  = Instant.ofEpochSecond(6L, 20)
    assertEquals(test5a == test5a, true)
    assertEquals(test5a == test5b, true)
    assertEquals(test5a == test5n, false)
    assertEquals(test5a == test6, false)
    assertEquals(test5b == test5a, true)
    assertEquals(test5b == test5b, true)
    assertEquals(test5b == test5n, false)
    assertEquals(test5b == test6, false)
    assertEquals(test5n == test5a, false)
    assertEquals(test5n == test5b, false)
    assertEquals(test5n == test5n, true)
    assertEquals(test5n == test6, false)
    assertEquals(test6 == test5a, false)
    assertEquals(test6 == test5b, false)
    assertEquals(test6 == test5n, false)
    assertEquals(test6 == test6, true)
  }

  test("equals_null") {
    val test5: Instant = Instant.ofEpochSecond(5L, 20)
    assertEquals(test5 == null, false)
  }

  test("equals_otherClass") {
    val test5: Instant = Instant.ofEpochSecond(5L, 20)
    assertNotEquals(test5, "")
  }

  test("hashCode") {
    val test5a: Instant = Instant.ofEpochSecond(5L, 20)
    val test5b: Instant = Instant.ofEpochSecond(5L, 20)
    val test5n: Instant = Instant.ofEpochSecond(5L, 30)
    val test6: Instant  = Instant.ofEpochSecond(6L, 20)
    assertEquals(test5a.hashCode == test5a.hashCode, true)
    assertEquals(test5a.hashCode == test5b.hashCode, true)
    assertEquals(test5b.hashCode == test5b.hashCode, true)
    assertEquals(test5a.hashCode == test5n.hashCode, false)
    assertEquals(test5a.hashCode == test6.hashCode, false)
  }

  def data_toString: List[List[AnyRef]] =
    List(
      List(Instant.ofEpochSecond(65L, 567), "1970-01-01T00:01:05.000000567Z"),
      List(Instant.ofEpochSecond(1, 0), "1970-01-01T00:00:01Z"),
      List(Instant.ofEpochSecond(60, 0), "1970-01-01T00:01:00Z"),
      List(Instant.ofEpochSecond(3600, 0), "1970-01-01T01:00:00Z"),
      List(Instant.ofEpochSecond(-1, 0), "1969-12-31T23:59:59Z"),
      List(LocalDateTime.of(0, 1, 2, 0, 0).toInstant(ZoneOffset.UTC), "0000-01-02T00:00:00Z"),
      List(LocalDateTime.of(0, 1, 1, 12, 30).toInstant(ZoneOffset.UTC), "0000-01-01T12:30:00Z"),
      List(LocalDateTime.of(0, 1, 1, 0, 0, 0, 1).toInstant(ZoneOffset.UTC),
           "0000-01-01T00:00:00.000000001Z"
      ),
      List(LocalDateTime.of(0, 1, 1, 0, 0).toInstant(ZoneOffset.UTC), "0000-01-01T00:00:00Z"),
      List(LocalDateTime.of(-1, 12, 31, 23, 59, 59, 999999999).toInstant(ZoneOffset.UTC),
           "-0001-12-31T23:59:59.999999999Z"
      ),
      List(LocalDateTime.of(-1, 12, 31, 12, 30).toInstant(ZoneOffset.UTC), "-0001-12-31T12:30:00Z"),
      List(LocalDateTime.of(-1, 12, 30, 12, 30).toInstant(ZoneOffset.UTC), "-0001-12-30T12:30:00Z"),
      List(LocalDateTime.of(-9999, 1, 2, 12, 30).toInstant(ZoneOffset.UTC),
           "-9999-01-02T12:30:00Z"
      ),
      List(LocalDateTime.of(-9999, 1, 1, 12, 30).toInstant(ZoneOffset.UTC),
           "-9999-01-01T12:30:00Z"
      ),
      List(LocalDateTime.of(-9999, 1, 1, 0, 0).toInstant(ZoneOffset.UTC), "-9999-01-01T00:00:00Z"),
      List(LocalDateTime.of(-10000, 12, 31, 23, 59, 59, 999999999).toInstant(ZoneOffset.UTC),
           "-10000-12-31T23:59:59.999999999Z"
      ),
      List(LocalDateTime.of(-10000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "-10000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(-10000, 12, 30, 12, 30).toInstant(ZoneOffset.UTC),
           "-10000-12-30T12:30:00Z"
      ),
      List(LocalDateTime.of(-15000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "-15000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(-19999, 1, 2, 12, 30).toInstant(ZoneOffset.UTC),
           "-19999-01-02T12:30:00Z"
      ),
      List(LocalDateTime.of(-19999, 1, 1, 12, 30).toInstant(ZoneOffset.UTC),
           "-19999-01-01T12:30:00Z"
      ),
      List(LocalDateTime.of(-19999, 1, 1, 0, 0).toInstant(ZoneOffset.UTC),
           "-19999-01-01T00:00:00Z"
      ),
      List(LocalDateTime.of(-20000, 12, 31, 23, 59, 59, 999999999).toInstant(ZoneOffset.UTC),
           "-20000-12-31T23:59:59.999999999Z"
      ),
      List(LocalDateTime.of(-20000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "-20000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(-20000, 12, 30, 12, 30).toInstant(ZoneOffset.UTC),
           "-20000-12-30T12:30:00Z"
      ),
      List(LocalDateTime.of(-25000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "-25000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(9999, 12, 30, 12, 30).toInstant(ZoneOffset.UTC),
           "9999-12-30T12:30:00Z"
      ),
      List(LocalDateTime.of(9999, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "9999-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(9999, 12, 31, 23, 59, 59, 999999999).toInstant(ZoneOffset.UTC),
           "9999-12-31T23:59:59.999999999Z"
      ),
      List(LocalDateTime.of(10000, 1, 1, 0, 0).toInstant(ZoneOffset.UTC), "+10000-01-01T00:00:00Z"),
      List(LocalDateTime.of(10000, 1, 1, 12, 30).toInstant(ZoneOffset.UTC),
           "+10000-01-01T12:30:00Z"
      ),
      List(LocalDateTime.of(10000, 1, 2, 12, 30).toInstant(ZoneOffset.UTC),
           "+10000-01-02T12:30:00Z"
      ),
      List(LocalDateTime.of(15000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "+15000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(19999, 12, 30, 12, 30).toInstant(ZoneOffset.UTC),
           "+19999-12-30T12:30:00Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "+19999-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 999999999).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.999999999Z"
      ),
      List(LocalDateTime.of(20000, 1, 1, 0, 0).toInstant(ZoneOffset.UTC), "+20000-01-01T00:00:00Z"),
      List(LocalDateTime.of(20000, 1, 1, 12, 30).toInstant(ZoneOffset.UTC),
           "+20000-01-01T12:30:00Z"
      ),
      List(LocalDateTime.of(20000, 1, 2, 12, 30).toInstant(ZoneOffset.UTC),
           "+20000-01-02T12:30:00Z"
      ),
      List(LocalDateTime.of(25000, 12, 31, 12, 30).toInstant(ZoneOffset.UTC),
           "+25000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 9999999).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.009999999Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 999999000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.999999Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 9999000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.009999Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 123000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.123Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 100000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.100Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 20000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.020Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 3000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.003Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 400000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000400Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 50000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000050Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 6000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000006Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 700).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000700Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 80).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000080Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 9).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000009Z"
      ),
      List(LocalDateTime.of(-999999999, 1, 1, 12, 30).toInstant(ZoneOffset.UTC).minus(1, DAYS),
           "-1000000000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(999999999, 12, 31, 12, 30).toInstant(ZoneOffset.UTC).plus(1, DAYS),
           "+1000000000-01-01T12:30:00Z"
      ),
      List(Instant.MIN, "-1000000000-01-01T00:00:00Z"),
      List(Instant.MAX, "+1000000000-12-31T23:59:59.999999999Z"),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 123000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.123Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 100000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.100Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 20000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.020Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 3000000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.003Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 400000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000400Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 50000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000050Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 6000).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000006Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 700).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000700Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 80).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000080Z"
      ),
      List(LocalDateTime.of(19999, 12, 31, 23, 59, 59, 9).toInstant(ZoneOffset.UTC),
           "+19999-12-31T23:59:59.000000009Z"
      ),
      List(LocalDateTime.of(-999999999, 1, 1, 12, 30).toInstant(ZoneOffset.UTC).minus(1, DAYS),
           "-1000000000-12-31T12:30:00Z"
      ),
      List(LocalDateTime.of(999999999, 12, 31, 12, 30).toInstant(ZoneOffset.UTC).plus(1, DAYS),
           "+1000000000-01-01T12:30:00Z"
      ),
      List(Instant.MIN, "-1000000000-01-01T00:00:00Z"),
      List(Instant.MAX, "+1000000000-12-31T23:59:59.999999999Z")
    )

  test("toString") {
    data_toString.foreach {
      case (instant: Instant) :: (expected: String) :: Nil =>
        assertEquals(instant.toString, expected)
      case _                                               =>
        fail()
    }
  }

  test("parse") {
    data_toString.foreach {
      case (instant: Instant) :: (text: String) :: Nil =>
        assertEquals(Instant.parse(text), instant)
      case _                                           =>
        fail()
    }
  }

  test("parseLowercase") {
    data_toString.foreach {
      case (instant: Instant) :: (text: String) :: Nil =>
        assertEquals(Instant.parse(text.toLowerCase), instant)
      case _                                           =>
        fail()
    }
  }
}
