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
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.TemporalUnit

/** Test Duration. */
class TestDuration extends AnyFunSuite with AssertionsHelper {

  test("zero") {
    assertEquals(Duration.ZERO.getSeconds, 0L)
    assertEquals(Duration.ZERO.getNano, 0)
  }

  test("factory_seconds_long") {
    var i: Long = -2
    while (i <= 2) {
      {
        val t: Duration = Duration.ofSeconds(i)
        assertEquals(t.getSeconds, i)
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
              val t: Duration = Duration.ofSeconds(i, j)
              assertEquals(t.getSeconds, i)
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
              val t: Duration = Duration.ofSeconds(i, j)
              assertEquals(t.getSeconds, i - 1)
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
              val t: Duration = Duration.ofSeconds(i, j)
              assertEquals(t.getSeconds, i)
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
    val test: Duration = Duration.ofSeconds(2L, -1)
    assertEquals(test.getSeconds, 1)
    assertEquals(test.getNano, 999999999)
  }

  test("factory_seconds_long_long_tooBig") {
    assertThrows[ArithmeticException] {
      Duration.ofSeconds(Long.MaxValue, 1000000000)
    }
  }

  def provider_factory_millis_long: List[List[Int]] =
    List(
      List(0, 0, 0),
      List(1, 0, 1000000),
      List(2, 0, 2000000),
      List(999, 0, 999000000),
      List(1000, 1, 0),
      List(1001, 1, 1000000),
      List(-1, -1, 999000000),
      List(-2, -1, 998000000),
      List(-999, -1, 1000000),
      List(-1000, -1, 0),
      List(-1001, -2, 999000000)
    )

  test("factory_millis_long") {
    provider_factory_millis_long.foreach {
      case millis :: expectedSeconds :: expectedNanoOfSecond :: Nil =>
        val test: Duration = Duration.ofMillis(millis)
        assertEquals(test.getSeconds, expectedSeconds)
        assertEquals(test.getNano, expectedNanoOfSecond)
      case _                                                        =>
        fail()
    }
  }

  test("factory_nanos_nanos") {
    val test: Duration = Duration.ofNanos(1)
    assertEquals(test.getSeconds, 0)
    assertEquals(test.getNano, 1)
  }

  test("factory_nanos_nanosSecs") {
    val test: Duration = Duration.ofNanos(1000000002)
    assertEquals(test.getSeconds, 1)
    assertEquals(test.getNano, 2)
  }

  test("factory_nanos_negative") {
    val test: Duration = Duration.ofNanos(-2000000001)
    assertEquals(test.getSeconds, -3)
    assertEquals(test.getNano, 999999999)
  }

  test("factory_nanos_max") {
    val test: Duration = Duration.ofNanos(Long.MaxValue)
    assertEquals(test.getSeconds, Long.MaxValue / 1000000000)
    assertEquals(test.getNano, Long.MaxValue % 1000000000)
  }

  test("factory_nanos_min") {
    val test: Duration = Duration.ofNanos(Long.MinValue)
    assertEquals(test.getSeconds, Long.MinValue / 1000000000 - 1)
    assertEquals(test.getNano, Long.MinValue % 1000000000 + 1000000000)
  }

  test("factory_minutes") {
    val test: Duration = Duration.ofMinutes(2)
    assertEquals(test.getSeconds, 120)
    assertEquals(test.getNano, 0)
  }

  test("factory_minutes_max") {
    val test: Duration = Duration.ofMinutes(Long.MaxValue / 60)
    assertEquals(test.getSeconds, (Long.MaxValue / 60) * 60)
    assertEquals(test.getNano, 0)
  }

  test("factory_minutes_min") {
    val test: Duration = Duration.ofMinutes(Long.MinValue / 60)
    assertEquals(test.getSeconds, (Long.MinValue / 60) * 60)
    assertEquals(test.getNano, 0)
  }

  test("factory_minutes_tooBig") {
    assertThrows[ArithmeticException] {
      Duration.ofMinutes(Long.MaxValue / 60 + 1)
    }
  }

  test("factory_minutes_tooSmall") {
    assertThrows[ArithmeticException] {
      Duration.ofMinutes(Long.MinValue / 60 - 1)
    }
  }

  test("factory_hours") {
    val test: Duration = Duration.ofHours(2)
    assertEquals(test.getSeconds, 2 * 3600)
    assertEquals(test.getNano, 0)
  }

  test("factory_hours_max") {
    val test: Duration = Duration.ofHours(Long.MaxValue / 3600)
    assertEquals(test.getSeconds, (Long.MaxValue / 3600) * 3600)
    assertEquals(test.getNano, 0)
  }

  test("factory_hours_min") {
    val test: Duration = Duration.ofHours(Long.MinValue / 3600)
    assertEquals(test.getSeconds, (Long.MinValue / 3600) * 3600)
    assertEquals(test.getNano, 0)
  }

  test("factory_hours_tooBig") {
    assertThrows[ArithmeticException] {
      Duration.ofHours(Long.MaxValue / 3600 + 1)
    }
  }

  test("factory_hours_tooSmall") {
    assertThrows[ArithmeticException] {
      Duration.ofHours(Long.MinValue / 3600 - 1)
    }
  }

  test("factory_days") {
    val test: Duration = Duration.ofDays(2)
    assertEquals(test.getSeconds, 2 * 86400)
    assertEquals(test.getNano, 0)
  }

  test("factory_days_max") {
    val test: Duration = Duration.ofDays(Long.MaxValue / 86400)
    assertEquals(test.getSeconds, (Long.MaxValue / 86400) * 86400)
    assertEquals(test.getNano, 0)
  }

  test("factory_days_min") {
    val test: Duration = Duration.ofDays(Long.MinValue / 86400)
    assertEquals(test.getSeconds, (Long.MinValue / 86400) * 86400)
    assertEquals(test.getNano, 0)
  }

  test("factory_days_tooBig") {
    assertThrows[ArithmeticException] {
      Duration.ofDays(Long.MaxValue / 86400 + 1)
    }
  }

  test("factory_days_tooSmall") {
    assertThrows[ArithmeticException] {
      Duration.ofDays(Long.MinValue / 86400 - 1)
    }
  }

  def provider_factory_of_longTemporalUnit: List[List[Any]] =
    List(
      List(0L, NANOS, 0L, 0),
      List(0L, MICROS, 0L, 0),
      List(0L, MILLIS, 0L, 0),
      List(0L, SECONDS, 0L, 0),
      List(0L, MINUTES, 0L, 0),
      List(0L, HOURS, 0L, 0),
      List(0L, HALF_DAYS, 0L, 0),
      List(0L, DAYS, 0L, 0),
      List(1L, NANOS, 0L, 1),
      List(1L, MICROS, 0L, 1000),
      List(1L, MILLIS, 0L, 1000000),
      List(1L, SECONDS, 1L, 0),
      List(1L, MINUTES, 60L, 0),
      List(1L, HOURS, 3600L, 0),
      List(1L, HALF_DAYS, 43200L, 0),
      List(1L, DAYS, 86400L, 0),
      List(3L, NANOS, 0L, 3),
      List(3L, MICROS, 0L, 3000),
      List(3L, MILLIS, 0L, 3000000),
      List(3L, SECONDS, 3L, 0),
      List(3L, MINUTES, 3 * 60L, 0),
      List(3L, HOURS, 3 * 3600L, 0),
      List(3L, HALF_DAYS, 3 * 43200L, 0),
      List(3L, DAYS, 3 * 86400L, 0),
      List(-1L, NANOS, -1L, 999999999),
      List(-1L, MICROS, -1L, 999999000),
      List(-1L, MILLIS, -1L, 999000000),
      List(-1L, SECONDS, -1L, 0),
      List(-1L, MINUTES, -60L, 0),
      List(-1L, HOURS, -3600L, 0),
      List(-1L, HALF_DAYS, -43200L, 0),
      List(-1L, DAYS, -86400L, 0),
      List(-3L, NANOS, -1L, 999999997),
      List(-3L, MICROS, -1L, 999997000),
      List(-3L, MILLIS, -1L, 997000000),
      List(-3L, SECONDS, -3L, 0),
      List(-3L, MINUTES, -3 * 60L, 0),
      List(-3L, HOURS, -3 * 3600L, 0),
      List(-3L, HALF_DAYS, -3 * 43200L, 0),
      List(-3L, DAYS, -3 * 86400L, 0),
      List(Long.MaxValue, NANOS, Long.MaxValue / 1000000000, (Long.MaxValue % 1000000000).toInt),
      List(Long.MinValue,
           NANOS,
           Long.MinValue / 1000000000 - 1,
           (Long.MinValue                                                   % 1000000000 + 1000000000).toInt
      ),
      List(Long.MaxValue,
           MICROS,
           Long.MaxValue / 1000000,
           ((Long.MaxValue                                                  % 1000000) * 1000).toInt
      ),
      List(Long.MinValue,
           MICROS,
           Long.MinValue / 1000000 - 1,
           ((Long.MinValue                                                  % 1000000 + 1000000) * 1000).toInt
      ),
      List(Long.MaxValue, MILLIS, Long.MaxValue / 1000, ((Long.MaxValue     % 1000) * 1000000).toInt),
      List(Long.MinValue,
           MILLIS,
           Long.MinValue / 1000 - 1,
           ((Long.MinValue                                                  % 1000 + 1000) * 1000000).toInt
      ),
      List(Long.MaxValue, SECONDS, Long.MaxValue, 0),
      List(Long.MinValue, SECONDS, Long.MinValue, 0),
      List(Long.MaxValue / 60, MINUTES, (Long.MaxValue / 60) * 60, 0),
      List(Long.MinValue / 60, MINUTES, (Long.MinValue / 60) * 60, 0),
      List(Long.MaxValue / 3600, HOURS, (Long.MaxValue / 3600) * 3600, 0),
      List(Long.MinValue / 3600, HOURS, (Long.MinValue / 3600) * 3600, 0),
      List(Long.MaxValue / 43200, HALF_DAYS, (Long.MaxValue / 43200) * 43200, 0),
      List(Long.MinValue / 43200, HALF_DAYS, (Long.MinValue / 43200) * 43200, 0)
    )

  test("factory_of_longTemporalUnit") {
    provider_factory_of_longTemporalUnit.foreach {
      case (amount: Long) :: (unit: TemporalUnit) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Int) :: Nil =>
        val t: Duration = Duration.of(amount, unit)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                       =>
        fail()
    }
  }

  def provider_factory_of_longTemporalUnit_outOfRange: List[List[Any]] =
    List(
      List(Long.MaxValue / 60 + 1, MINUTES),
      List(Long.MinValue / 60 - 1, MINUTES),
      List(Long.MaxValue / 3600 + 1, HOURS),
      List(Long.MinValue / 3600 - 1, HOURS),
      List(Long.MaxValue / 43200 + 1, HALF_DAYS),
      List(Long.MinValue / 43200 - 1, HALF_DAYS)
    )

  test("factory_of_longTemporalUnit_outOfRange") {
    provider_factory_of_longTemporalUnit_outOfRange.foreach {
      case (amount: Long) :: (unit: TemporalUnit) :: Nil =>
        assertThrows[ArithmeticException] {
          Duration.of(amount, unit)
        }
      case _                                             =>
        fail()
    }
  }

  test("factory_of_longTemporalUnit_estimatedUnit") {
    assertThrows[DateTimeException] {
      Duration.of(2, WEEKS)
    }
  }

  test("factory_of_longTemporalUnit_null") {
    assertThrows[NullPointerException] {
      Duration.of(1, null.asInstanceOf[TemporalUnit])
    }
  }

  def provider_factory_between_Instant_Instant: List[List[Int]] =
    List(List(0, 0, 0, 0, 0, 0),
         List(3, 0, 7, 0, 4, 0),
         List(3, 20, 7, 50, 4, 30),
         List(3, 80, 7, 50, 3, 999999970),
         List(7, 0, 3, 0, -4, 0)
    )

  test("factory_between_Instant_Instant") {
    provider_factory_between_Instant_Instant.foreach {
      case secs1 :: nanos1 :: secs2 :: nanos2 :: expectedSeconds :: expectedNanoOfSecond :: Nil =>
        val start: Instant = Instant.ofEpochSecond(secs1, nanos1)
        val end: Instant   = Instant.ofEpochSecond(secs2, nanos2)
        val t: Duration    = Duration.between(start, end)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                    =>
        fail()
    }
  }

  test("factory_between_Instant_Instant_startNull") {
    assertThrows[Platform.NPE] {
      val end: Instant = Instant.ofEpochSecond(1)
      Duration.between(null, end)
    }
  }

  test("factory_between_Instant_Instant_endNull") {
    assertThrows[Platform.NPE] {
      val start: Instant = Instant.ofEpochSecond(1)
      Duration.between(start, null)
    }
  }

  def provider_factory_parse: List[List[Any]] =
    List(
      List("PT0S", 0L, 0L),
      List("PT1S", 1L, 0L),
      List("PT12S", 12L, 0L),
      List("PT123456789S", 123456789L, 0L),
      List("PT" + Long.MaxValue + "S", Long.MaxValue, 0L),
      List("PT+1S", 1L, 0L),
      List("PT+12S", 12L, 0L),
      List("PT-1S", -1L, 0L),
      List("PT-12S", -12L, 0L),
      List("PT-123456789S", -123456789L, 0L),
      List("PT" + Long.MinValue + "S", Long.MinValue, 0L),
      List("PT0.1S", 0L, 100000000L),
      List("PT1.1S", 1L, 100000000L),
      List("PT1.12S", 1L, 120000000L),
      List("PT1.123S", 1L, 123000000L),
      List("PT1.1234S", 1L, 123400000L),
      List("PT1.12345S", 1L, 123450000L),
      List("PT1.123456S", 1L, 123456000L),
      List("PT1.1234567S", 1L, 123456700L),
      List("PT1.12345678S", 1L, 123456780L),
      List("PT1.123456789S", 1L, 123456789L),
      List("PT-0.1S", -1L, 1000000000 - 100000000L),
      List("PT-1.1S", -2L, 1000000000 - 100000000L),
      List("PT-1.12S", -2L, 1000000000 - 120000000L),
      List("PT-1.123S", -2L, 1000000000 - 123000000L),
      List("PT-1.1234S", -2L, 1000000000 - 123400000L),
      List("PT-1.12345S", -2L, 1000000000 - 123450000L),
      List("PT-1.123456S", -2L, 1000000000 - 123456000L),
      List("PT-1.1234567S", -2L, 1000000000 - 123456700L),
      List("PT-1.12345678S", -2L, 1000000000 - 123456780L),
      List("PT-1.123456789S", -2L, 1000000000 - 123456789L),
      List("PT" + Long.MaxValue + ".123456789S", Long.MaxValue, 123456789L),
      List("PT" + Long.MinValue + ".000000000S", Long.MinValue, 0L),
      List("PT12M", 12 * 60L, 0L),
      List("PT12M0.35S", 12 * 60L, 350000000L),
      List("PT12M1.35S", 12 * 60 + 1L, 350000000L),
      List("PT12M-0.35S", 12 * 60 - 1L, 1000000000 - 350000000L),
      List("PT12M-1.35S", 12 * 60 - 2L, 1000000000 - 350000000L),
      List("PT12H", 12 * 3600L, 0L),
      List("PT12H0.35S", 12 * 3600L, 350000000L),
      List("PT12H1.35S", 12 * 3600 + 1L, 350000000L),
      List("PT12H-0.35S", 12 * 3600 - 1L, 1000000000 - 350000000L),
      List("PT12H-1.35S", 12 * 3600 - 2L, 1000000000 - 350000000L),
      List("P12D", 12 * 24 * 3600L, 0L),
      List("P12DT0.35S", 12 * 24 * 3600L, 350000000L),
      List("P12DT1.35S", 12 * 24 * 3600 + 1L, 350000000L),
      List("P12DT-0.35S", 12 * 24 * 3600 - 1L, 1000000000 - 350000000L),
      List("P12DT-1.35S", 12 * 24 * 3600 - 2L, 1000000000 - 350000000L)
    )

  test("factory_parse") {
    provider_factory_parse.foreach {
      case (text: String) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val t: Duration = Duration.parse(text)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                =>
        fail()
    }
  }

  test("factory_parse_ignoreCase") {
    provider_factory_parse.foreach {
      case (text: String) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val t: Duration = Duration.parse(text.toLowerCase)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                =>
        fail()
    }
  }

  test("factory_parse_comma") {
    provider_factory_parse.foreach {
      case (text: String) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var _text       = text
        _text = _text.replace('.', ',')
        val t: Duration = Duration.parse(_text)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                =>
        fail()
    }
  }

  def provider_factory_parseFailures: List[List[String]] =
    List(
      List(""),
      List("PTS"),
      List("AT0S"),
      List("PA0S"),
      List("PT0A"),
      List("PT+S"),
      List("PT-S"),
      List("PT.S"),
      List("PTAS"),
      List("PT-.S"),
      List("PT+.S"),
      List("PT1ABC2S"),
      List("PT1.1ABC2S"),
      List("PT123456789123456789123456789S"),
      List("PT0.1234567891S"),
      List("PT.1S"),
      List("PT2.-3"),
      List("PT-2.-3"),
      List("PT2.+3"),
      List("PT-2.+3")
    )

  test("factory_parseFailures") {
    provider_factory_parseFailures.foreach {
      case text :: Nil =>
        assertThrows[DateTimeException] {
          Duration.parse(text)
        }
      case _           =>
        fail()
    }
  }

  test("factory_parseFailures_comma") {
    provider_factory_parseFailures.foreach {
      case text :: Nil =>
        assertThrows[DateTimeException] {
          var _text = text
          _text = _text.replace('.', ',')
          Duration.parse(_text)
        }
      case _           =>
        fail()
    }
  }

  test("factory_parse_tooBig") {
    assertThrows[DateTimeException] {
      Duration.parse("PT" + Long.MaxValue + "1S")
    }
  }

  test("factory_parse_tooBig_decimal") {
    assertThrows[DateTimeException] {
      Duration.parse("PT" + Long.MaxValue + "1.1S")
    }
  }

  test("factory_parse_tooSmall") {
    assertThrows[DateTimeException] {
      Duration.parse("PT" + Long.MinValue + "1S")
    }
  }

  test("factory_parse_tooSmall_decimal") {
    assertThrows[DateTimeException] {
      Duration.parse("PT" + Long.MinValue + ".1S")
    }
  }

  test("factory_parse_nullText") {
    assertThrows[NullPointerException] {
      Duration.parse(null.asInstanceOf[String])
    }
  }

  test("test_isZero") {
    assertEquals(Duration.ofNanos(0).isZero, true)
    assertEquals(Duration.ofSeconds(0).isZero, true)
    assertEquals(Duration.ofNanos(1).isZero, false)
    assertEquals(Duration.ofSeconds(1).isZero, false)
    assertEquals(Duration.ofSeconds(1, 1).isZero, false)
    assertEquals(Duration.ofNanos(-1).isZero, false)
    assertEquals(Duration.ofSeconds(-1).isZero, false)
    assertEquals(Duration.ofSeconds(-1, -1).isZero, false)
  }

  test("test_isNegative") {
    assertEquals(Duration.ofNanos(0).isNegative, false)
    assertEquals(Duration.ofSeconds(0).isNegative, false)
    assertEquals(Duration.ofNanos(1).isNegative, false)
    assertEquals(Duration.ofSeconds(1).isNegative, false)
    assertEquals(Duration.ofSeconds(1, 1).isNegative, false)
    assertEquals(Duration.ofNanos(-1).isNegative, true)
    assertEquals(Duration.ofSeconds(-1).isNegative, true)
    assertEquals(Duration.ofSeconds(-1, -1).isNegative, true)
  }

  def provider_plus: List[List[Long]] = {
    List(
      List(Long.MinValue, 0, Long.MaxValue, 0, -1, 0),
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
      List(Long.MaxValue, 0, Long.MinValue, 0, -1, 0)
    )
  }

  test("plus") {
    provider_plus.foreach {
      case (seconds: Long) :: (nanos: Long) :: (otherSeconds: Long) :: (otherNanos: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val t: Duration =
          Duration.ofSeconds(seconds, nanos).plus(Duration.ofSeconds(otherSeconds, otherNanos))
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                                                =>
        fail()
    }
  }

  test("plusOverflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
      t.plus(Duration.ofSeconds(0, 1))
    }
  }

  test("plusOverflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue)
      t.plus(Duration.ofSeconds(-1, 999999999))
    }
  }

  test("plus_longTemporalUnit_seconds") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.plus(1, SECONDS)
    assertEquals(2, t.getSeconds)
    assertEquals(0, t.getNano)
  }

  test("plus_longTemporalUnit_millis") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.plus(1, MILLIS)
    assertEquals(1, t.getSeconds)
    assertEquals(1000000, t.getNano)
  }

  test("plus_longTemporalUnit_micros") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.plus(1, MICROS)
    assertEquals(1, t.getSeconds)
    assertEquals(1000, t.getNano)
  }

  test("plus_longTemporalUnit_nanos") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.plus(1, NANOS)
    assertEquals(1, t.getSeconds)
    assertEquals(1, t.getNano)
  }

  test("plus_longTemporalUnit_null") {
    assertThrows[NullPointerException] {
      val t: Duration = Duration.ofSeconds(1)
      t.plus(1, null.asInstanceOf[TemporalUnit])
    }
  }

  def provider_plusSeconds_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, 1, 0),
      List(0, 0, -1, -1, 0),
      List(0, 0, Long.MaxValue, Long.MaxValue, 0),
      List(0, 0, Long.MinValue, Long.MinValue, 0),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 2, 0),
      List(1, 0, -1, 0, 0),
      List(1, 0, Long.MaxValue - 1, Long.MaxValue, 0),
      List(1, 0, Long.MinValue, Long.MinValue + 1, 0),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 2, 1),
      List(1, 1, -1, 0, 1),
      List(1, 1, Long.MaxValue - 1, Long.MaxValue, 1),
      List(1, 1, Long.MinValue, Long.MinValue + 1, 1),
      List(-1, 1, 0, -1, 1),
      List(-1, 1, 1, 0, 1),
      List(-1, 1, -1, -2, 1),
      List(-1, 1, Long.MaxValue, Long.MaxValue - 1, 1),
      List(-1, 1, Long.MinValue + 1, Long.MinValue, 1)
    )

  test("plusSeconds_long") {
    provider_plusSeconds_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.plusSeconds(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusSeconds_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(1, 0)
      t.plusSeconds(Long.MaxValue)
    }
  }

  test("plusSeconds_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(-1, 0)
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
      List(0, 999999999, -1001, -1, 998999999)
    )

  test("plusMillis_long") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_oneMore") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds + 1, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds + 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_minusOneLess") {
    provider_plusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds - 1, nanos)
        t = t.plusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds - 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusMillis_long_max") {
    var t: Duration = Duration.ofSeconds(Long.MaxValue, 998999999)
    t = t.plusMillis(1)
    assertEquals(t.getSeconds, Long.MaxValue)
    assertEquals(t.getNano, 999999999)
  }

  test("plusMillis_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999000000)
      t.plusMillis(1)
    }
  }

  test("plusMillis_long_min") {
    var t: Duration = Duration.ofSeconds(Long.MinValue, 1000000)
    t = t.plusMillis(-1)
    assertEquals(t.getSeconds, Long.MinValue)
    assertEquals(t.getNano, 0)
  }

  test("plusMillis_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue, 0)
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
      List(Long.MaxValue, 0, 999999999, Long.MaxValue, 999999999),
      List(Long.MaxValue - 1, 0, 1999999999, Long.MaxValue, 999999999),
      List(Long.MinValue, 1, -1, Long.MinValue, 0),
      List(Long.MinValue + 1, 1, -1000000001, Long.MinValue, 0)
    )

  test("plusNanos_long") {
    provider_plusNanos_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.plusNanos(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("plusNanos_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
      t.plusNanos(1)
    }
  }

  test("plusNanos_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue, 0)
      t.plusNanos(-1)
    }
  }

  def provider_minus: List[List[Long]] = {
    List(
      List(Long.MinValue, 0, Long.MinValue + 1, 0, -1, 0),
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
      List(Long.MaxValue, 0, Long.MaxValue, 0, 0, 0)
    )
  }

  test("minus") {
    provider_minus.foreach {
      case (seconds: Long) :: (nanos: Long) :: (otherSeconds: Long) :: (otherNanos: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        val t: Duration =
          Duration.ofSeconds(seconds, nanos).minus(Duration.ofSeconds(otherSeconds, otherNanos))
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                                                =>
        fail()
    }
  }

  test("minusOverflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue)
      t.minus(Duration.ofSeconds(0, 1))
    }
  }

  test("minusOverflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
      t.minus(Duration.ofSeconds(-1, 999999999))
    }
  }

  test("minus_longTemporalUnit_seconds") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.minus(1, SECONDS)
    assertEquals(0, t.getSeconds)
    assertEquals(0, t.getNano)
  }

  test("minus_longTemporalUnit_millis") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.minus(1, MILLIS)
    assertEquals(0, t.getSeconds)
    assertEquals(999000000, t.getNano)
  }

  test("minus_longTemporalUnit_micros") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.minus(1, MICROS)
    assertEquals(0, t.getSeconds)
    assertEquals(999999000, t.getNano)
  }

  test("minus_longTemporalUnit_nanos") {
    var t: Duration = Duration.ofSeconds(1)
    t = t.minus(1, NANOS)
    assertEquals(0, t.getSeconds)
    assertEquals(999999999, t.getNano)
  }

  def minus_longTemporalUnit_null(): Unit =
    assertThrows[NullPointerException] {
      val t: Duration = Duration.ofSeconds(1)
      t.minus(1, null.asInstanceOf[TemporalUnit])
    }

  def provider_minusSeconds_long: List[List[Long]] =
    List(
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, -1, 0),
      List(0, 0, -1, 1, 0),
      List(0, 0, Long.MaxValue, -Long.MaxValue, 0),
      List(0, 0, Long.MinValue + 1, Long.MaxValue, 0),
      List(1, 0, 0, 1, 0),
      List(1, 0, 1, 0, 0),
      List(1, 0, -1, 2, 0),
      List(1, 0, Long.MaxValue - 1, -Long.MaxValue + 2, 0),
      List(1, 0, Long.MinValue + 2, Long.MaxValue, 0),
      List(1, 1, 0, 1, 1),
      List(1, 1, 1, 0, 1),
      List(1, 1, -1, 2, 1),
      List(1, 1, Long.MaxValue, -Long.MaxValue + 1, 1),
      List(1, 1, Long.MinValue + 2, Long.MaxValue, 1),
      List(-1, 1, 0, -1, 1),
      List(-1, 1, 1, -2, 1),
      List(-1, 1, -1, 0, 1),
      List(-1, 1, Long.MaxValue, Long.MinValue, 1),
      List(-1, 1, Long.MinValue + 1, Long.MaxValue - 1, 1)
    )

  test("minusSeconds_long") {
    provider_minusSeconds_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.minusSeconds(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusSeconds_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(1, 0)
      t.minusSeconds(Long.MinValue + 1)
    }
  }

  test("minusSeconds_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(-2, 0)
      t.minusSeconds(Long.MaxValue)
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
      List(0, 999999999, -1001, 2, 999999)
    )

  test("minusMillis_long") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.minusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_oneMore") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds + 1, nanos)
        t = t.minusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds + 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_minusOneLess") {
    provider_minusMillis_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds - 1, nanos)
        t = t.minusMillis(amount)
        assertEquals(t.getSeconds, expectedSeconds - 1)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusMillis_long_max") {
    var t: Duration = Duration.ofSeconds(Long.MaxValue, 998999999)
    t = t.minusMillis(-1)
    assertEquals(t.getSeconds, Long.MaxValue)
    assertEquals(t.getNano, 999999999)
  }

  test("minusMillis_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999000000)
      t.minusMillis(-1)
    }
  }

  test("minusMillis_long_min") {
    var t: Duration = Duration.ofSeconds(Long.MinValue, 1000000)
    t = t.minusMillis(1)
    assertEquals(t.getSeconds, Long.MinValue)
    assertEquals(t.getNano, 0)
  }

  test("minusMillis_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue, 0)
      t.minusMillis(1)
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
      List(Long.MaxValue, 0, -999999999, Long.MaxValue, 999999999),
      List(Long.MaxValue - 1, 0, -1999999999, Long.MaxValue, 999999999),
      List(Long.MinValue, 1, 1, Long.MinValue, 0),
      List(Long.MinValue + 1, 1, 1000000001, Long.MinValue, 0)
    )

  test("minusNanos_long") {
    provider_minusNanos_long.foreach {
      case (seconds: Long) :: (nanos: Long) :: (amount: Long) :: (expectedSeconds: Long) :: (expectedNanoOfSecond: Long) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.minusNanos(amount)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanoOfSecond)
      case _                                                                                                                    =>
        fail()
    }
  }

  test("minusNanos_long_overflowTooBig") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MaxValue, 999999999)
      t.minusNanos(-1)
    }
  }

  test("minusNanos_long_overflowTooSmall") {
    assertThrows[ArithmeticException] {
      val t: Duration = Duration.ofSeconds(Long.MinValue, 0)
      t.minusNanos(1)
    }
  }

  def provider_multipliedBy: List[List[Any]] =
    List(
      List(-4, 666666667, -3, 9, 999999999),
      List(-4, 666666667, -2, 6, 666666666),
      List(-4, 666666667, -1, 3, 333333333),
      List(-4, 666666667, 0, 0, 0),
      List(-4, 666666667, 1, -4, 666666667),
      List(-4, 666666667, 2, -7, 333333334),
      List(-4, 666666667, 3, -10, 1),
      List(-3, 0, -3, 9, 0),
      List(-3, 0, -2, 6, 0),
      List(-3, 0, -1, 3, 0),
      List(-3, 0, 0, 0, 0),
      List(-3, 0, 1, -3, 0),
      List(-3, 0, 2, -6, 0),
      List(-3, 0, 3, -9, 0),
      List(-2, 0, -3, 6, 0),
      List(-2, 0, -2, 4, 0),
      List(-2, 0, -1, 2, 0),
      List(-2, 0, 0, 0, 0),
      List(-2, 0, 1, -2, 0),
      List(-2, 0, 2, -4, 0),
      List(-2, 0, 3, -6, 0),
      List(-1, 0, -3, 3, 0),
      List(-1, 0, -2, 2, 0),
      List(-1, 0, -1, 1, 0),
      List(-1, 0, 0, 0, 0),
      List(-1, 0, 1, -1, 0),
      List(-1, 0, 2, -2, 0),
      List(-1, 0, 3, -3, 0),
      List(-1, 500000000, -3, 1, 500000000),
      List(-1, 500000000, -2, 1, 0),
      List(-1, 500000000, -1, 0, 500000000),
      List(-1, 500000000, 0, 0, 0),
      List(-1, 500000000, 1, -1, 500000000),
      List(-1, 500000000, 2, -1, 0),
      List(-1, 500000000, 3, -2, 500000000),
      List(0, 0, -3, 0, 0),
      List(0, 0, -2, 0, 0),
      List(0, 0, -1, 0, 0),
      List(0, 0, 0, 0, 0),
      List(0, 0, 1, 0, 0),
      List(0, 0, 2, 0, 0),
      List(0, 0, 3, 0, 0),
      List(0, 500000000, -3, -2, 500000000),
      List(0, 500000000, -2, -1, 0),
      List(0, 500000000, -1, -1, 500000000),
      List(0, 500000000, 0, 0, 0),
      List(0, 500000000, 1, 0, 500000000),
      List(0, 500000000, 2, 1, 0),
      List(0, 500000000, 3, 1, 500000000),
      List(1, 0, -3, -3, 0),
      List(1, 0, -2, -2, 0),
      List(1, 0, -1, -1, 0),
      List(1, 0, 0, 0, 0),
      List(1, 0, 1, 1, 0),
      List(1, 0, 2, 2, 0),
      List(1, 0, 3, 3, 0),
      List(2, 0, -3, -6, 0),
      List(2, 0, -2, -4, 0),
      List(2, 0, -1, -2, 0),
      List(2, 0, 0, 0, 0),
      List(2, 0, 1, 2, 0),
      List(2, 0, 2, 4, 0),
      List(2, 0, 3, 6, 0),
      List(3, 0, -3, -9, 0),
      List(3, 0, -2, -6, 0),
      List(3, 0, -1, -3, 0),
      List(3, 0, 0, 0, 0),
      List(3, 0, 1, 3, 0),
      List(3, 0, 2, 6, 0),
      List(3, 0, 3, 9, 0),
      List(3, 333333333, -3, -10, 1),
      List(3, 333333333, -2, -7, 333333334),
      List(3, 333333333, -1, -4, 666666667),
      List(3, 333333333, 0, 0, 0),
      List(3, 333333333, 1, 3, 333333333),
      List(3, 333333333, 2, 6, 666666666),
      List(3, 333333333, 3, 9, 999999999)
    )

  test("multipliedBy") {
    provider_multipliedBy.foreach {
      case (seconds: Int) :: (nanos: Int) :: (multiplicand: Int) :: (expectedSeconds: Int) :: (expectedNanos: Int) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.multipliedBy(multiplicand)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanos)
      case _                                                                                                              =>
        fail()
    }
  }

  test("multipliedBy_max") {
    val test: Duration = Duration.ofSeconds(1)
    assertEquals(test.multipliedBy(Long.MaxValue), Duration.ofSeconds(Long.MaxValue))
  }

  test("multipliedBy_min") {
    val test: Duration = Duration.ofSeconds(1)
    assertEquals(test.multipliedBy(Long.MinValue), Duration.ofSeconds(Long.MinValue))
  }

  test("multipliedBy_tooBig") {
    assertThrows[ArithmeticException] {
      val test: Duration = Duration.ofSeconds(1, 1)
      test.multipliedBy(Long.MaxValue)
    }
  }

  test("multipliedBy_tooBig_negative") {
    assertThrows[ArithmeticException] {
      val test: Duration = Duration.ofSeconds(1, 1)
      test.multipliedBy(Long.MinValue)
    }
  }

  def provider_dividedBy: List[List[Any]] =
    List(
      List(-4, 666666667, -3, 1, 111111111),
      List(-4, 666666667, -2, 1, 666666666),
      List(-4, 666666667, -1, 3, 333333333),
      List(-4, 666666667, 1, -4, 666666667),
      List(-4, 666666667, 2, -2, 333333334),
      List(-4, 666666667, 3, -2, 888888889),
      List(-3, 0, -3, 1, 0),
      List(-3, 0, -2, 1, 500000000),
      List(-3, 0, -1, 3, 0),
      List(-3, 0, 1, -3, 0),
      List(-3, 0, 2, -2, 500000000),
      List(-3, 0, 3, -1, 0),
      List(-2, 0, -3, 0, 666666666),
      List(-2, 0, -2, 1, 0),
      List(-2, 0, -1, 2, 0),
      List(-2, 0, 1, -2, 0),
      List(-2, 0, 2, -1, 0),
      List(-2, 0, 3, -1, 333333334),
      List(-1, 0, -3, 0, 333333333),
      List(-1, 0, -2, 0, 500000000),
      List(-1, 0, -1, 1, 0),
      List(-1, 0, 1, -1, 0),
      List(-1, 0, 2, -1, 500000000),
      List(-1, 0, 3, -1, 666666667),
      List(-1, 500000000, -3, 0, 166666666),
      List(-1, 500000000, -2, 0, 250000000),
      List(-1, 500000000, -1, 0, 500000000),
      List(-1, 500000000, 1, -1, 500000000),
      List(-1, 500000000, 2, -1, 750000000),
      List(-1, 500000000, 3, -1, 833333334),
      List(0, 0, -3, 0, 0),
      List(0, 0, -2, 0, 0),
      List(0, 0, -1, 0, 0),
      List(0, 0, 1, 0, 0),
      List(0, 0, 2, 0, 0),
      List(0, 0, 3, 0, 0),
      List(0, 500000000, -3, -1, 833333334),
      List(0, 500000000, -2, -1, 750000000),
      List(0, 500000000, -1, -1, 500000000),
      List(0, 500000000, 1, 0, 500000000),
      List(0, 500000000, 2, 0, 250000000),
      List(0, 500000000, 3, 0, 166666666),
      List(1, 0, -3, -1, 666666667),
      List(1, 0, -2, -1, 500000000),
      List(1, 0, -1, -1, 0),
      List(1, 0, 1, 1, 0),
      List(1, 0, 2, 0, 500000000),
      List(1, 0, 3, 0, 333333333),
      List(2, 0, -3, -1, 333333334),
      List(2, 0, -2, -1, 0),
      List(2, 0, -1, -2, 0),
      List(2, 0, 1, 2, 0),
      List(2, 0, 2, 1, 0),
      List(2, 0, 3, 0, 666666666),
      List(3, 0, -3, -1, 0),
      List(3, 0, -2, -2, 500000000),
      List(3, 0, -1, -3, 0),
      List(3, 0, 1, 3, 0),
      List(3, 0, 2, 1, 500000000),
      List(3, 0, 3, 1, 0),
      List(3, 333333333, -3, -2, 888888889),
      List(3, 333333333, -2, -2, 333333334),
      List(3, 333333333, -1, -4, 666666667),
      List(3, 333333333, 1, 3, 333333333),
      List(3, 333333333, 2, 1, 666666666),
      List(3, 333333333, 3, 1, 111111111)
    )

  test("dividedBy") {
    provider_dividedBy.foreach {
      case (seconds: Int) :: (nanos: Int) :: (divisor: Int) :: (expectedSeconds: Int) :: (expectedNanos: Int) :: Nil =>
        var t: Duration = Duration.ofSeconds(seconds, nanos)
        t = t.dividedBy(divisor)
        assertEquals(t.getSeconds, expectedSeconds)
        assertEquals(t.getNano, expectedNanos)
      case _                                                                                                         =>
        fail()
    }
  }

  test("dividedByZero") {
    provider_dividedBy.foreach {
      case (seconds: Int) :: (nanos: Int) :: (_: Int) :: (_: Int) :: (_: Int) :: Nil =>
        assertThrows[ArithmeticException] {
          val t: Duration = Duration.ofSeconds(seconds, nanos)
          t.dividedBy(0)
        }
      case _                                                                         =>
        fail()
    }
  }

  test("dividedBy_max") {
    val test: Duration = Duration.ofSeconds(Long.MaxValue)
    assertEquals(test.dividedBy(Long.MaxValue), Duration.ofSeconds(1))
  }

  test("test_negated") {
    assertEquals(Duration.ofSeconds(0).negated, Duration.ofSeconds(0))
    assertEquals(Duration.ofSeconds(12).negated, Duration.ofSeconds(-12))
    assertEquals(Duration.ofSeconds(-12).negated, Duration.ofSeconds(12))
    assertEquals(Duration.ofSeconds(12, 20).negated, Duration.ofSeconds(-12, -20))
    assertEquals(Duration.ofSeconds(12, -20).negated, Duration.ofSeconds(-12, 20))
    assertEquals(Duration.ofSeconds(-12, -20).negated, Duration.ofSeconds(12, 20))
    assertEquals(Duration.ofSeconds(-12, 20).negated, Duration.ofSeconds(12, -20))
    assertEquals(Duration.ofSeconds(Long.MaxValue).negated, Duration.ofSeconds(-Long.MaxValue))
  }

  test("test_negated_overflow") {
    assertThrows[ArithmeticException] {
      Duration.ofSeconds(Long.MinValue).negated
    }
  }

  test("test_abs") {
    assertEquals(Duration.ofSeconds(0).abs, Duration.ofSeconds(0))
    assertEquals(Duration.ofSeconds(12).abs, Duration.ofSeconds(12))
    assertEquals(Duration.ofSeconds(-12).abs, Duration.ofSeconds(12))
    assertEquals(Duration.ofSeconds(12, 20).abs, Duration.ofSeconds(12, 20))
    assertEquals(Duration.ofSeconds(12, -20).abs, Duration.ofSeconds(12, -20))
    assertEquals(Duration.ofSeconds(-12, -20).abs, Duration.ofSeconds(12, 20))
    assertEquals(Duration.ofSeconds(-12, 20).abs, Duration.ofSeconds(12, -20))
    assertEquals(Duration.ofSeconds(Long.MaxValue).abs, Duration.ofSeconds(Long.MaxValue))
  }

  test("test_abs_overflow") {
    assertThrows[ArithmeticException] {
      Duration.ofSeconds(Long.MinValue).abs
    }
  }

  test("test_toNanos") {
    val test: Duration = Duration.ofSeconds(321, 123456789)
    assertEquals(test.toNanos, 321123456789L)
  }

  test("test_toNanos_max") {
    val test: Duration = Duration.ofSeconds(0, Long.MaxValue)
    assertEquals(test.toNanos, Long.MaxValue)
  }

  test("test_toNanos_tooBig") {
    assertThrows[ArithmeticException] {
      val test: Duration = Duration.ofSeconds(0, Long.MaxValue).plusNanos(1)
      test.toNanos
    }
  }

  test("test_toMillis") {
    val test: Duration = Duration.ofSeconds(321, 123456789)
    assertEquals(test.toMillis, 321000 + 123)
  }

  test("test_toMillis_max") {
    val test: Duration = Duration.ofSeconds(Long.MaxValue / 1000, (Long.MaxValue % 1000) * 1000000)
    assertEquals(test.toMillis, Long.MaxValue)
  }

  test("test_toMillis_tooBig") {
    assertThrows[ArithmeticException] {
      val test: Duration =
        Duration.ofSeconds(Long.MaxValue / 1000, ((Long.MaxValue % 1000) + 1) * 1000000)
      test.toMillis
    }
  }

  test("test_comparisons") {
    doTest_comparisons_Duration(
      Duration.ofSeconds(-2L, 0),
      Duration.ofSeconds(-2L, 999999998),
      Duration.ofSeconds(-2L, 999999999),
      Duration.ofSeconds(-1L, 0),
      Duration.ofSeconds(-1L, 1),
      Duration.ofSeconds(-1L, 999999998),
      Duration.ofSeconds(-1L, 999999999),
      Duration.ofSeconds(0L, 0),
      Duration.ofSeconds(0L, 1),
      Duration.ofSeconds(0L, 2),
      Duration.ofSeconds(0L, 999999999),
      Duration.ofSeconds(1L, 0),
      Duration.ofSeconds(2L, 0)
    )
  }

  private def doTest_comparisons_Duration(durations: Duration*): Unit = {
    var i: Int = 0
    while (i < durations.length) {
      val a: Duration = durations(i)
      var j: Int      = 0
      while (j < durations.length) {
        val b: Duration = durations(j)
        if (i < j) {
          assertEquals(a.compareTo(b) < 0, true, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else if (i > j) {
          assertEquals(a.compareTo(b) > 0, true, a + " <=> " + b)
          assertEquals(a == b, false, a + " <=> " + b)
        } else {
          assertEquals(a.compareTo(b), 0, a + " <=> " + b)
          assertEquals(a == b, true, a + " <=> " + b)
        }
        j += 1
      }
      i += 1
    }
  }

  def test_compareTo_ObjectNull(): Unit =
    assertThrows[NullPointerException] {
      val a: Duration = Duration.ofSeconds(0L, 0)
      a.compareTo(null)
    }

  test("test_equals") {
    val test5a: Duration = Duration.ofSeconds(5L, 20)
    val test5b: Duration = Duration.ofSeconds(5L, 20)
    val test5n: Duration = Duration.ofSeconds(5L, 30)
    val test6: Duration  = Duration.ofSeconds(6L, 20)
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

  test("test_equals_null") {
    val test5: Duration = Duration.ofSeconds(5L, 20)
    assertEquals(test5 == null, false)
  }

  test("test_equals_otherClass") {
    val test5: Duration = Duration.ofSeconds(5L, 20)
    assertNotEquals(test5, "")
  }

  test("test_hashCode") {
    val test5a: Duration = Duration.ofSeconds(5L, 20)
    val test5b: Duration = Duration.ofSeconds(5L, 20)
    val test5n: Duration = Duration.ofSeconds(5L, 30)
    val test6: Duration  = Duration.ofSeconds(6L, 20)
    assertEquals(test5a.hashCode == test5a.hashCode, true)
    assertEquals(test5a.hashCode == test5b.hashCode, true)
    assertEquals(test5b.hashCode == test5b.hashCode, true)
    assertEquals(test5a.hashCode == test5n.hashCode, false)
    assertEquals(test5a.hashCode == test6.hashCode, false)
  }

  def provider_toString: List[List[Any]] =
    List(
      List(0, 0, "PT0S"),
      List(0, 1, "PT0.000000001S"),
      List(0, 10, "PT0.00000001S"),
      List(0, 100, "PT0.0000001S"),
      List(0, 1000, "PT0.000001S"),
      List(0, 10000, "PT0.00001S"),
      List(0, 100000, "PT0.0001S"),
      List(0, 1000000, "PT0.001S"),
      List(0, 10000000, "PT0.01S"),
      List(0, 100000000, "PT0.1S"),
      List(0, 120000000, "PT0.12S"),
      List(0, 123000000, "PT0.123S"),
      List(0, 123400000, "PT0.1234S"),
      List(0, 123450000, "PT0.12345S"),
      List(0, 123456000, "PT0.123456S"),
      List(0, 123456700, "PT0.1234567S"),
      List(0, 123456780, "PT0.12345678S"),
      List(0, 123456789, "PT0.123456789S"),
      List(1, 0, "PT1S"),
      List(-1, 0, "PT-1S"),
      List(-1, 1000, "PT-0.999999S"),
      List(-1, 900000000, "PT-0.1S"),
      List(60, 0, "PT1M"),
      List(3600, 0, "PT1H"),
      List(7261, 0, "PT2H1M1S")
    )

  test("test_toString") {
    provider_toString.foreach {
      case (seconds: Int) :: (nanos: Int) :: (expected: String) :: Nil =>
        val t: Duration = Duration.ofSeconds(seconds, nanos)
        assertEquals(t.toString, expected)
      case _                                                           =>
        fail()
    }
  }
}
